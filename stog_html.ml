(** *)

open Stog_types;;


let url_compat s =
 let s = Stog_misc.lowercase s in
 for i = 0 to String.length s - 1 do
   match s.[i] with
     'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' -> ()
    | _  -> s.[i] <- '+'
 done;
 s
;;

let escape_html s =
  let b = Buffer.create 256 in
  for i = 0 to String.length s - 1 do
    let s =
      match s.[i] with
        '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | c -> String.make 1 c
    in
    Buffer.add_string b s
  done;
  Buffer.contents b
;;

let article_url stog art =
  Printf.sprintf "%s/%s" stog.stog_base_url art.art_human_id
;;

let link_to ?(from=`Article) file =
  let pref = match from with
      `Article -> "../"
    | `Index -> ""
  in
  Printf.sprintf "%s%s" pref file
;;

let link_to_article ?(from=`Article) article =
  link_to ~from
    (Printf.sprintf "%s/index.html" article.art_human_id)
;;

let topic_index_file topic =
  url_compat (Printf.sprintf "topic_%s.html" topic)
;;
let keyword_index_file kw =
  url_compat (Printf.sprintf "kw_%s.html" kw)
;;
let month_index_file ~year ~month =
  url_compat (Printf.sprintf "%04d_%02d.html" year month)
;;

let fun_include tmpl_file args =
  if Array.length args < 1 then
    failwith "Missing file for include command";
  let file = args.(0) in
  let file =
    if Filename.is_relative file then
      Filename.concat (Filename.dirname tmpl_file) file
    else
      file
  in
  Stog_misc.string_of_file file;;

let fun_img args =
  if Array.length args < 2 then
    failwith "Missing argument for img command"
  else
    Printf.sprintf "<div class=\"img\"><img class=\"img\" src=\"%s\" width=\"%s\"/></div>" args.(0) args.(1)
;;

let fun_img_legend args =
  if Array.length args < 3 then
    failwith "Missing argument for img-legend command"
  else
    Printf.sprintf
     "<div class=\"img\"><img class=\"img\" title=\"%s\" src=\"%s\" width=\"%s\"/><div class=\"legend\">%s</div></div>"
      args.(1) args.(0) args.(2) args.(1)
;;


let fun_img_float position args =
  if Array.length args < 2 then
    failwith (Printf.sprintf "Missing argument for img-%s command" position)
  else
    Printf.sprintf
     "<div class=\"img-float-%s\"><img class=\"img\" alt=\"\" src=\"%s\" width=\"%s\"/></div>"
      position args.(0) args.(1)
;;

let fun_ref ?from stog args =
  let article, text =
    match Array.length args with
      n when n < 1 -> failwith "Missing argument for 'ref' command"
    | n ->
        let a =
          try
            let (_, a) = Stog_types.article_by_human_id stog args.(0) in
            Some a
          with
            Not_found ->
              prerr_endline (Printf.sprintf "Unknown article '%s'" args.(0));
              None
        in
        let text =
          match a, n with
            None, 1 -> "??"
          | Some a, 1 -> Printf.sprintf "\"%s\"" a.art_title
          | _, _ -> args.(1)
        in
        (a, text)
  in
  match article with
    None -> Printf.sprintf "<span class=\"unknown-ref\">%s</span>" (escape_html text)
  | Some a ->
      Printf.sprintf "<a href=\"%s\">%s</a>"
      (link_to_article ?from a)
      (escape_html text)
;;

let fun_archive_tree ?from stog =
  let b = Buffer.create 256 in
  let f_mon year month set =
    let link = link_to ?from (month_index_file ~year ~month) in
    Printf.bprintf b "<li><a href=\"%s\">%s</a>(%d)</li>"
      link months.(month-1) (Stog_types.Art_set.cardinal set)
  in
  let f_year year mmap =
    Printf.bprintf b "<li>%d<ul>" year;
    Stog_types.Int_map.iter (f_mon year) mmap;
    Buffer.add_string b "</ul></li>"
  in
  Buffer.add_string b "<ul>";
  Stog_types.Int_map.iter f_year stog.stog_archives ;
  Buffer.add_string b "</ul>";
  Buffer.contents b
;;

let fun_rss_feed file args =
  Printf.sprintf
  "<link href=\"%s\" type=\"application/rss+xml\" rel=\"alternate\" title=\"RSS feed\"/>"
  file
;;

let fun_code language args =
  let code = args.(0) in
  let temp_file = Filename.temp_file "stog" "highlight" in
  let com = Printf.sprintf
    "echo %s | highlight --syntax=%s -f > %s"
    (Filename.quote code) language (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let code = Stog_misc.string_of_file temp_file in
      Sys.remove temp_file;
      Printf.sprintf "<pre class=\"code-%s\">%s</pre>"
        language code
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let fun_ocaml = fun_code "ocaml";;

let fun_section cls args =
  Printf.sprintf "<section class=\"%s\">%s</section>"
  cls (String.concat " " (Array.to_list args))
;;

let fun_subsection = fun_section "subsection";;
let fun_section = fun_section "section";;

let fun_search_form stog _ =
  let tmpl = Filename.concat stog.stog_tmpl_dir "search.tmpl" in
  Stog_misc.string_of_file tmpl
;;

let fun_blog_url stog _ = stog.stog_base_url;;

let fun_graph =
  let generated = ref false in
  fun outdir ?from stog _ ->
    let name = "blog-graph.png" in
    let src = link_to ?from name in
    let small_src = link_to ?from ("small-"^name) in
    begin
      match !generated with
        true -> ()
      | false ->
          generated := true;
          let tmp = Filename.temp_file "stog" "dot" in
          Stog_misc.file_of_string ~file: tmp
          (Stog_info.dot_of_graph stog);
          let com = Printf.sprintf "dot -Gcharset=latin1 -Tpng -o %s %s"
            (Filename.quote (Filename.concat outdir src))
            (Filename.quote tmp)
          in
          match Sys.command com with
            0 ->
              begin
                (try Sys.remove tmp with _ -> ());
                let com = Printf.sprintf "convert -scale 120x120 %s %s"
                  (Filename.quote (Filename.concat outdir src))
                  (Filename.quote (Filename.concat outdir small_src))
                in
                match Sys.command com with
                  0 -> ()
                | _ ->
                    prerr_endline (Printf.sprintf "Command failed: %s" com)
              end
          | _ ->
              prerr_endline (Printf.sprintf "Command failed: %s" com)
    end;
    Printf.sprintf "<a href=\"%s\"><img src=\"%s\" alt=\"Graph\"/></a>"
    src small_src
;;

let default_commands outdir tmpl_file ?from ?rss stog =
  [ "include", fun_include tmpl_file ;
    "img", fun_img ;
    "img-legend", fun_img_legend ;
    "img-left", fun_img_float "left" ;
    "img-right", fun_img_float "right" ;
    "archive-tree", (fun _ -> fun_archive_tree ?from stog) ;
    "ocaml", fun_ocaml ;
    "ref", fun_ref ?from stog;
    "section", fun_section ;
    "subsection", fun_subsection ;
    "rssfeed", (match rss with None -> fun _ -> "" | Some file -> fun_rss_feed file);
    "blog-url", fun_blog_url stog ;
    "search-form", fun_search_form stog ;
    "graph", fun_graph outdir ?from stog ;
  ]
;;

let rss_date_of_article article =
    let (y, m, d) = article.art_date in
    {
      Rss.year = y ; month = m ; day = d ;
      hour = 8 ; minute = 0 ; second = 0 ;
      zone = 0 ; week_day = -1 ;
    }
;;

let article_to_rss_item stog article =
  let link = link_to_article ~from: `Index article in
  let link = Printf.sprintf "%s/%s" stog.stog_base_url link in
  let pubdate = rss_date_of_article article in
  Rss.item ~title: article.art_title
  ~link
  ~pubdate
  (*       ?cats:category list ->*)
  ~guid: { Rss.guid_name = link ; guid_permalink = true }
  ()
;;

let generate_rss_feed_file stog ?title link articles file =
  let arts = Stog_types.sort_articles_by_date articles in
  let items = List.map (article_to_rss_item stog) arts in
  let title = Printf.sprintf "%s%s"
    stog.stog_title
    (match title with None -> "" | Some t -> Printf.sprintf ": %s" t)
  in
  let link = stog.stog_base_url ^"/" ^ link in
  let pubdate =
    match arts with
      [] -> None
    | h :: _ -> Some (rss_date_of_article h)
  in
  let channel =
    Rss.channel ~title ~link
    ~desc: stog.stog_desc
    ~managing_editor: stog.stog_email
    ?pubdate ?last_build_date: pubdate
    ~generator: "Stog"
    items
  in
  let channel = Rss.keep_n_items stog.stog_rss_length channel in
  Rss.print_file file channel
;;

let copy_file ?(quote_src=true) ?(quote_dst=true) src dest =
  let com = Printf.sprintf "cp -f %s %s"
    (if quote_src then Filename.quote src else src)
    (if quote_dst then Filename.quote dest else dest)
  in
  match Sys.command com with
    0 -> ()
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let string_of_body s =
  Str.global_replace (Str.regexp_string "<-->") "" s
;;

let html_of_topics stog art args =
  let sep =
    if Array.length args < 1 then ", " else args.(0)
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "topic.tmpl" in
  let f w =
    Stog_tmpl.apply_file [ "topic", (fun _ -> w) ] tmpl
  in
  String.concat sep
  (List.map (fun w ->
    Printf.sprintf "<a href=\"%s\">%s</a>"
      (link_to (topic_index_file w))
      (f w)
   ) art.art_topics)
;;

let html_of_keywords stog art args =
  let sep =
    if Array.length args < 1 then ", " else args.(0)
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "keyword.tmpl" in
  let f w =
    Stog_tmpl.apply_file [ "keyword", (fun _ -> w) ] tmpl
  in
  String.concat sep
  (List.map (fun w ->
      Printf.sprintf "<a href=\"%s\">%s</a>"
      (link_to (keyword_index_file w))
      (f w)
   )
   art.art_keywords)
;;

let remove_re s =
  let re = Str.regexp "^Re:[ ]?" in
  let rec iter s =
    let p =
      try Some (Str.search_forward re s 0)
      with Not_found -> None
    in
    match p with
      None -> s
    | Some p ->
        assert (p=0);
        let matched_len = String.length (Str.matched_string s) in
        let s = String.sub s matched_len (String.length s - matched_len) in
        iter s
  in
  iter s
;;

let escape_mailto_arg s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '&' -> Buffer.add_string b "%26"
    | ' ' -> Buffer.add_string b "%20"
    | '?' -> Buffer.add_string b "%3F"
    | '%' -> Buffer.add_string b "%25"
    | ',' -> Buffer.add_string b "%2C"
    | '\n' -> Buffer.add_string b "%0D%0A"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b
;;

let normalize_email s =
  let s2 =
    try
      let p = String.index s '<' in
      try
        let p1 = String.index_from s p '>' in
        String.sub s (p+1) (p1-p-1)
      with Not_found -> s
    with
      Not_found -> s
  in
  (*prerr_endline (Printf.sprintf "normalize(%s) = %s" s s2);*)
  s2
;;


let build_mailto stog ?message article =
  let emails =
    match message with
      None -> [stog.stog_email]
    | Some message -> [stog.stog_email ; message.mes_from]
  in
  let emails =
    Stog_misc.list_remove_doubles
    (List.map normalize_email emails)
  in
  let hid = article.art_human_id in
  let subject =
    match message with
      None -> Printf.sprintf "%s [%s]" article.art_title (Stog_misc.md5 hid)
    | Some m ->
        Printf.sprintf "Re: %s [%s/%s]"
        (remove_re m.mes_subject)
        (Stog_misc.md5 hid) (Stog_misc.md5 m.mes_id)
  in
  let body = Stog_misc.string_of_file
    (Filename.concat stog.stog_tmpl_dir "comment_body.tmpl")
  in
  Printf.sprintf
    "mailto:%s?subject=%s&amp;body=%s"
    (Stog_misc.encode_string (escape_mailto_arg (String.concat ", " emails)))
    (escape_mailto_arg subject)
    (escape_mailto_arg body)
;;


let html_comment_actions stog article message =
  Printf.sprintf
    "<a href=\"%s\"><img src=\"../comment_reply.png\" alt=\"reply to comment\" title=\"reply to comment\"/></a>"
  (build_mailto stog ~message article)
;;

let re_citation = Str.regexp "\\(\\(^&gt;[^\n]+\n\\)+\\)";;
let gen_id = let id = ref 0 in (fun () -> incr id; !id);;

let html_of_message_body body =
  let body = escape_html (Stog_misc.strip_string body) in
  let subst s =
    let id = gen_id () in
    let s = Str.matched_group 1 body in
    if Stog_misc.count_char s '\n' <= 2 then
      Printf.sprintf "<div class=\"comment-citation\">%s</div>" s
    else
      Printf.sprintf "<div class=\"comment-citation\" onclick=\"if(document.getElementById('comment%d').style.display=='none') {document.getElementById('comment%d').style.display='block';} else {document.getElementById('comment%d').style.display='none';}\">&gt; ... <img src=\"../expand_collapse.png\" alt=\"+/-\"/></div><div class=\"comment-expand\" id=\"comment%d\">%s</div>"
      id id id id
      s
  in
  let body = Str.global_substitute re_citation subst body in
  body
;;

let rec html_of_comments outdir stog article tmpl comments =
  let f (Node (message, subs)) =
    Stog_tmpl.apply_file
    ([
       "date", (fun _ -> Stog_date.mk_mail_date (Stog_date.since_epoch message.mes_time)) ;
       "subject", (fun _ -> escape_html message.mes_subject );
       "from", (fun _ -> escape_html message.mes_from);
       "to", (fun _ -> escape_html (String.concat ", " message.mes_to)) ;
       "body", (fun _ -> html_of_message_body message.mes_body) ;
       "comment-actions", (fun _ -> html_comment_actions stog article message) ;
       "comments", (fun _ -> html_of_comments outdir stog article tmpl subs) ;
     ] @ (default_commands outdir tmpl ~from:`Index stog)
    )
    tmpl
  in
  String.concat "\n" (List.map f comments)
;;

let html_of_comments outdir stog article =
  let tmpl = Filename.concat stog.stog_tmpl_dir "comment.tmpl" in
  html_of_comments outdir stog article tmpl article.art_comments
;;

let generate_article outdir stog art_id article =
  let html_file = Filename.concat outdir
    (link_to_article ~from: `Index article)
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "article.tmpl" in
  let art_dir = Filename.dirname html_file in
  let url = article_url stog article in
  Stog_misc.mkdir art_dir;
  List.iter (fun f -> copy_file f art_dir) article.art_files;

  let next f _ =
    match f stog art_id with
      None -> ""
    | Some id ->
        let a = Stog_types.article stog id in
        let link = link_to_article a in
        Printf.sprintf "<a href=\"%s\">%s</a>"
          link a.art_title
  in
  let comment_actions =
    Printf.sprintf
    "<a href=\"%s\"><img src=\"../comment.png\" alt=\"Post a comment\" title=\"Post a comment\"/></a>"
    (build_mailto stog article)
  in
  Stog_tmpl.apply
  ([
     "title", (fun _ -> article.art_title) ;
     "article-title", (fun _ -> article.art_title) ;
     "article-url", (fun _ -> url) ;
     "stylefile", (fun _ -> "../style.css") ;
     "blog-title", (fun _ -> stog.stog_title) ;
     "blog-description", (fun _ -> stog.stog_desc) ;
     "article-body", (fun _ -> string_of_body article.art_body);
     "article-date", (fun _ -> Stog_types.string_of_date article.art_date) ;
     "next", (next Stog_info.succ_by_date) ;
     "previous", (next Stog_info.pred_by_date) ;
     "article-keywords", html_of_keywords stog article ;
     "article-topics", html_of_topics stog article ;
     "comment-actions", (fun _ -> comment_actions);
     "comments", (fun _ -> html_of_comments outdir stog article) ;
   ] @ (default_commands outdir tmpl stog))
  tmpl html_file
;;

let intro_of_article art =
  let re_sep = Str.regexp_string "<-->" in
  try
    let p = Str.search_forward re_sep art.art_body 0 in
    Printf.sprintf "%s <a href=\"%s\"><img src=\"next.png\" alt=\"next\"/></a>"
    (String.sub art.art_body 0 p)
    (link_to_article ~from:`Index art)
  with
    Not_found -> art.art_body
;;


let article_list outdir ?rss ?set stog args =
  let max =
    if Array.length args >= 1 then
      Some (int_of_string args.(0))
    else
      None
  in
  let arts =
    match set with
      None -> Stog_types.article_list stog
    | Some set ->
        let l = Stog_types.Art_set.elements set in
        List.map (fun id -> (id, Stog_types.article stog id)) l
  in
  let arts = List.rev (Stog_types.sort_ids_articles_by_date arts) in
  let arts =
    match max with
      None -> arts
    | Some n -> Stog_misc.list_chop n arts
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "article_list.tmpl" in
  let f_article (_, art) =
    let url = article_url stog art in
    Stog_tmpl.apply_file
    ([
       "article-date", (fun _ -> Stog_types.string_of_date art.art_date) ;
       "article-title", (fun _ -> art.art_title );
       "article-url", (fun _ -> url );
       "article-intro", (fun _ -> intro_of_article art) ;
     ] @ (default_commands outdir tmpl ~from:`Index stog))
    tmpl
  in
  let html = String.concat "" (List.map f_article arts) in
  match rss with
    None -> html
  | Some link ->
      Printf.sprintf
      "<div class=\"rss-button\"><a href=\"%s\"><img src=\"rss.png\" alt=\"Rss feed\"/></a></div>%s"
      link html
;;

let generate_by_word_indexes outdir stog tmpl map f_html_file =
  let f word set =
    let base_html_file = f_html_file word in
    let html_file = Filename.concat outdir base_html_file in
    let tmpl = Filename.concat stog.stog_tmpl_dir tmpl in
    let rss_basefile = (Filename.chop_extension base_html_file)^".rss" in
    let rss_file = Filename.concat outdir rss_basefile in
    generate_rss_feed_file stog ~title: word base_html_file
    (List.map (Stog_types.article stog) (Stog_types.Art_set.elements set))
    rss_file;
    Stog_tmpl.apply
    ([
       "stylefile", (fun _ -> "style.css") ;
       "blog-title", (fun _ -> stog.stog_title) ;
       "blog-description", (fun _ -> stog.stog_desc) ;
       "articles", (article_list outdir ~set ~rss: rss_basefile stog);
       "title", (fun _ -> word) ;
     ] @ (default_commands outdir tmpl ~from:`Index ~rss: rss_basefile stog))
    tmpl html_file
  in
  Stog_types.Str_map.iter f map
;;

let generate_topic_indexes outdir stog =
  generate_by_word_indexes outdir stog
  "by_topic.tmpl" stog.stog_arts_by_topic
  topic_index_file
;;

let generate_keyword_indexes outdir stog =
  generate_by_word_indexes outdir stog
  "by_kw.tmpl" stog.stog_arts_by_kw
  keyword_index_file
;;

let generate_archive_index outdir stog =
  let f_month year month set =
    let tmpl = Filename.concat stog.stog_tmpl_dir "archive_month.tmpl" in
    let html_file = Filename.concat outdir (month_index_file ~year ~month) in
    Stog_tmpl.apply
    ([
       "stylefile", (fun _ -> "style.css") ;
       "blog-title", (fun _ -> stog.stog_title) ;
       "blog-description", (fun _ -> stog.stog_desc) ;
       "articles", (article_list outdir ~set stog);
       "title", (fun _ -> Printf.sprintf "%s %d" months.(month-1) year) ;
     ] @ (default_commands outdir tmpl ~from:`Index stog))
    tmpl html_file
  in
  let f_year year mmap =
    Stog_types.Int_map.iter (f_month year) mmap
  in
  Stog_types.Int_map.iter f_year stog.stog_archives
;;

let generate_index_file outdir stog =
  let basefile = "index.html" in
  let html_file = Filename.concat outdir basefile in
  let tmpl = Filename.concat stog.stog_tmpl_dir "index.tmpl" in
  let rss_basefile = "index.rss" in
  let rss_file = Filename.concat outdir rss_basefile in
  generate_rss_feed_file stog basefile
    (List.map snd (Stog_types.article_list stog)) rss_file;
  Stog_tmpl.apply
  ([
    "stylefile", (fun _ -> "style.css") ;
    "blog-title", (fun _ -> stog.stog_title) ;
    "blog-body", (fun _ -> stog.stog_body);
    "blog-description", (fun _ -> stog.stog_desc) ;
    "blog-url", (fun _ -> stog.stog_base_url) ;
    "articles", (article_list outdir ~rss: rss_basefile stog);
  ] @ (default_commands outdir tmpl ~from:`Index ~rss: rss_basefile stog))
  tmpl html_file
;;

let generate_index outdir stog =
  Stog_misc.mkdir outdir;
  copy_file (Filename.concat stog.stog_tmpl_dir "style.css") outdir;
  copy_file ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.png") outdir;
  generate_index_file outdir stog;
  generate_topic_indexes outdir stog;
  generate_keyword_indexes outdir stog;
  generate_archive_index outdir stog
;;

let generate outdir stog =
  generate_index outdir stog ;
  Stog_tmap.iter (generate_article outdir stog)
    stog.stog_articles
;;

  