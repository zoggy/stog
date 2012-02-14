(** *)

open Stog_types;;

let languages = ["fr" ; "en" ];;

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

let html_file stog name =
  let ext_pref =
    match stog.stog_lang with
      None -> ""
    | Some lang -> "."^lang
  in
  Printf.sprintf "%s%s.html" name ext_pref
;;

let tag_sep = "<sep_/>";;

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

let link_to_article stog ?(from=`Article) article =
  link_to ~from
  (Printf.sprintf "%s/%s"
   article.art_human_id (html_file stog "index"))
;;

let topic_index_file stog topic =
  url_compat (html_file stog (Printf.sprintf "topic_%s" topic))
;;
let keyword_index_file stog kw =
  url_compat (html_file stog (Printf.sprintf "kw_%s" kw))
;;
let month_index_file stog ~year ~month =
  url_compat (html_file stog (Printf.sprintf "%04d_%02d" year month))
;;

let page_file stog page = html_file stog page.page_human_id;;

let make_lang_funs stog =
  match stog.stog_lang with
    None -> []
  | Some lang ->
      let to_remove = List.filter ((<>) lang) languages in
      let f_keep _env _args subs = subs in
      let f_remove _env _args _subs = [] in
      (lang, f_keep) ::
      (List.map (fun lang -> (lang, f_remove)) to_remove)
;;

let fun_include tmpl_dir _env args _ =
  match Stog_xtmpl.get_arg args "file" with
    None -> failwith "Missing 'file' argument for include command";
  | Some file ->
      let file =
        if Filename.is_relative file then
          Filename.concat tmpl_dir file
        else
          file
      in
      let xml = [Stog_xtmpl.xml_of_string (Stog_misc.string_of_file file)] in
      [Stog_xtmpl.T (Stog_xtmpl.tag_env, args, xml)]
;;

let fun_image _env args legend =
  let width = Stog_xtmpl.opt_arg args "width" in
  let src = Stog_xtmpl.opt_arg args "src" in
  let cls = Printf.sprintf "img%s"
    (match Stog_xtmpl.get_arg args "float" with
       Some "left" -> "-float-left"
     | Some "right" -> "-float-right"
     | Some s -> failwith (Printf.sprintf "unhandled image position: %s" s)
     | None -> ""
    )
  in
  [
    Stog_xtmpl.T ("div", [ "class", cls ],
     (Stog_xtmpl.T ("img", [ "class", "img" ; "src", src; "width", width ], [])) ::
     (match legend with
        [] -> []
      | xml -> [ Stog_xtmpl.T ("div", ["class", "legend"], xml) ]
     )
    )
  ]
;;


let fun_article_href hid ?from stog env args _ =
  let article, text =
    let a =
      try
        let (_, a) = Stog_types.article_by_human_id stog hid in
        Some a
      with
        Not_found ->
          prerr_endline (Printf.sprintf "Unknown article '%s'" hid);
          None
    in
    let text =
      match a, Stog_xtmpl.get_arg args "text" with
        None, _ -> "??"
            | Some a, None -> Printf.sprintf "\"%s\"" a.art_title
      | Some _, Some text -> text
    in
    (a, text)
  in
  match article with
    None -> [Stog_xtmpl.T ("span", ["class", "unknown-ref"], [Stog_xtmpl.D text])]
  | Some a ->
      [
        Stog_xtmpl.T ("a", ["href", (link_to_article stog ?from a)], [Stog_xtmpl.D text])
      ]
;;

let fun_article ?from stog env args subs =
  let hid =
    match Stog_xtmpl.get_arg args "href" with
      None -> failwith "Missing href for <article>"
    | Some id -> id
  in
  fun_article_href hid ?from stog env args subs
;;


let fun_archive_tree ?from stog _env _ =
  let mk_months map =
    List.sort (fun (m1, _) (m2, _) -> compare m2 m1)
    (Stog_types.Int_map.fold
     (fun month data acc -> (month, data) :: acc)
     map
     []
    )
  in
  let years =
    Stog_types.Int_map.fold
      (fun year data acc -> (year, mk_months data) :: acc)
      stog.stog_archives
      []
  in
  let years = List.sort (fun (y1,_) (y2,_) -> compare y2 y1) years in

  let f_mon year (month, set) =
    let link = link_to ?from (month_index_file stog ~year ~month) in
    Stog_xtmpl.T ("li", [], [
       Stog_xtmpl.T ("a", ["href", link], [ Stog_xtmpl.D (months.(month-1)) ]) ;
       Stog_xtmpl.D (Printf.sprintf "(%d)" (Stog_types.Art_set.cardinal set))
     ]
    )
  in
  let f_year (year, data) =
    Stog_xtmpl.T ("li", [], [
       Stog_xtmpl.D (string_of_int year) ;
       Stog_xtmpl.T ("ul", [], List.map (f_mon year) data) ;
      ]
    )
  in
  [ Stog_xtmpl.T ("ul", [], List.map f_year years) ]
;;

let fun_rss_feed file args _env _ =
  [
    Stog_xtmpl.T ("link",
     [ "href", file ; "type", "application/rss+xml" ; "rel", "alternate" ; "title", "RSS feed"],
     [])
  ]
;;

let fun_code ?lang _env args code =
  let language =
    match lang with
      None -> Stog_xtmpl.opt_arg args ~def: "txt" "lang"
    | Some lang -> lang
  in
  let code =
    match code with
      [ Stog_xtmpl.D code ] -> code
    | _ -> failwith (Printf.sprintf "Invalid code: %s"
         (String.concat "" (List.map Stog_xtmpl.string_of_xml code)))
  in
  let temp_file = Filename.temp_file "stog" "highlight" in
  let com = Printf.sprintf
    "echo %s | highlight --syntax=%s -f > %s"
    (Filename.quote code) language (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let code = Stog_misc.string_of_file temp_file in
      Sys.remove temp_file;
      [ Stog_xtmpl.T ("pre",
         ["class", Printf.sprintf "code-%s" language],
         [Stog_xtmpl.xml_of_string code]
        )
      ]
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let fun_ocaml = fun_code ~lang: "ocaml";;

let fun_section cls _env args body =
  let title =
    match Stog_xtmpl.get_arg args "title" with
      None -> []
    | Some t ->
        [Stog_xtmpl.T ("div", ["class", cls^"-title"], [Stog_xtmpl.xml_of_string t])]
  in
  [ Stog_xtmpl.T ("div", ["class", cls], title @ body) ]
;;

let fun_subsection = fun_section "subsection";;
let fun_section = fun_section "section";;

let fun_search_form stog _env _ _ =
  let tmpl = Filename.concat stog.stog_tmpl_dir "search.tmpl" in
  [ Stog_xtmpl.xml_of_string (Stog_misc.string_of_file tmpl) ]
;;

let fun_blog_url stog _env _ _ = [ Stog_xtmpl.D stog.stog_base_url ];;

let fun_graph =
  let generated = ref false in
  fun outdir ?from stog _env _ _ ->
    let name = "site-graph.png" in
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
          let com = Printf.sprintf "dot -Gcharset=utf-8 -Tpng -o %s %s"
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
    [
      Stog_xtmpl.T ("a", ["href", src], [
        Stog_xtmpl.T ("img", ["src", small_src ; "alt", "Graph"], [])
       ])
    ]
;;

let fun_if env args subs =
  let pred (att, v) =
    let s = Stog_xtmpl.apply env (Printf.sprintf "<%s/>" att) in
    (*prerr_endline (Printf.sprintf "fun_if: pred: att=%s, s=%s, v=%s" att s v);*)
    s = v
  in
  let cond = List.for_all pred args in
  match cond, subs with
  | true, [] -> failwith "<if>: missing children"
  | true, h :: _
  | false, _ :: h :: _ -> [h]
  | false, []
  | false, [_] -> []
;;

let get_page stog hid =
    try snd(Stog_types.page_by_human_id stog hid)
    with Not_found -> failwith (Printf.sprintf "No such page: %s" hid)
;;

let generate_page stog env contents =
  let tmpl = Filename.concat stog.stog_tmpl_dir "page.tmpl" in
  let f env args body = contents in
  let env = Stog_xtmpl.env_of_list ~env ["contents", f] in
  Stog_xtmpl.apply env (Stog_misc.string_of_file tmpl)
;;

let fun_page_ref hid stog env args subs =
  let page = get_page stog hid in
  let file = page_file stog page in
  let link = link_to ~from: `Index file in
  let text =
    match subs with
      [] -> [ Stog_xtmpl.xml_of_string page.page_title ]
    | l -> l
  in
  [ Stog_xtmpl.T ("a", ["href", link], text) ]
;;

let env_add_langswitch env stog html_f =
  let name = "langswitch" in
  match stog.stog_lang with
    None ->
      Stog_xtmpl.env_add name (fun _ _ _ -> []) env
  | Some lang ->
      let map_lang lang =
         let basename = Filename.chop_extension (Filename.chop_extension html_f) in
         let html_file = html_file { stog with stog_lang = Some lang } basename in
         let html_file = Filename.basename html_file in
         let img_url = Printf.sprintf "%s/%s.png" stog.stog_base_url lang in
         Stog_xtmpl.T ("a", ["href", html_file], [
           Stog_xtmpl.T ("img", ["src", img_url ; "title", lang ; "alt", lang], [])])
      in
      let f _env args _subs =
        let languages =
          match Stog_xtmpl.get_arg args "languages" with
            Some s -> Stog_misc.split_string s [','; ';' ; ' ']
          | None -> languages
        in
        let languages = List.filter ((<>) lang) languages in
        List.map map_lang languages
      in
      Stog_xtmpl.env_add name f env
;;

let fun_twocolumns env args subs =
  prerr_endline (Printf.sprintf "two-columns, length(subs)=%d" (List.length subs));
  let empty = [] in
  let subs = List.fold_right
    (fun xml acc ->
       match xml with
         Stog_xtmpl.D _ -> acc
       | Stog_xtmpl.T (_,_,subs)
       | Stog_xtmpl.E (_, subs) -> subs :: acc
    ) subs []
  in
  let left, right =
    match subs with
      [] -> empty, empty
    | [left] -> left, empty
    | left :: right :: _ -> left, right
  in
  [ Stog_xtmpl.T ("table", ["class", "two-columns"],
     [ Stog_xtmpl.T ("tr", [],
        [ Stog_xtmpl.T ("td", ["class", "two-columns-left"], left) ;
          Stog_xtmpl.T ("td", ["class", "two-columns-right"], right) ;
        ]);
     ])
  ]
;;

let rec fun_page_id hid outdir stog env args subs =
  let page = get_page stog hid in
  let file = Filename.concat outdir (page_file stog page) in
  let xml = Stog_xtmpl.xml_of_string page.page_body in
  let env = Stog_xtmpl.env_of_list ~env (default_commands ~outdir ~from: `Index stog) in
  let env = env_add_langswitch env stog file in
  let env = List.fold_left
    (fun env (s,v) -> Stog_xtmpl.env_add_att s v env)
    env
    (("page-title", page.page_title) :: args)
  in
  let s = generate_page stog env [xml] in
  Stog_xtmpl.apply_string_to_file ~head: "<!DOCTYPE html>" env s file;
  []

and fun_page outdir stog env args subs =
    match Stog_xtmpl.get_arg args "id" with
    | Some hid -> fun_page_id hid outdir stog env args subs
    | None ->
      match Stog_xtmpl.get_arg args "href" with
      | Some id -> fun_page_ref id stog env args subs
      | None -> failwith "Missing id or href for <page>"

and default_commands ?outdir ?from ?rss stog =
  let l =
    [
      "if", fun_if ;
      "include", fun_include stog.stog_tmpl_dir ;
      "image", fun_image ;
      "archive-tree", (fun _ -> fun_archive_tree ?from stog) ;
      "code", fun_code ?lang: None;
      "ocaml", fun_ocaml ;
      "article", fun_article ?from stog;
      "section", fun_section ;
      "subsection", fun_subsection ;
      "rssfeed", (match rss with None -> fun _env _ _ -> [] | Some file -> fun_rss_feed file);
      "site-url", fun_blog_url stog ;
      "search-form", fun_search_form stog ;
      "site-title", (fun _ _ _ -> [ Stog_xtmpl.D stog.stog_title ]) ;
      "site-description", (fun _ _ _ -> [ Stog_xtmpl.xml_of_string stog.stog_desc ]) ;
      "two-columns", fun_twocolumns ;
    ]
  in
  let l =
     match outdir with
            | None -> l
            | Some outdir ->
                l @ ["graph", fun_graph outdir ?from stog ;
                  "page", (fun_page outdir stog)
                ]
  in
  (make_lang_funs stog) @ l
;;

let intro_of_article stog art =
  let re_sep = Str.regexp_string tag_sep in
  try
    let p = Str.search_forward re_sep art.art_body 0 in
    [ Stog_xtmpl.xml_of_string (String.sub art.art_body 0 p) ;
     Stog_xtmpl.T ("a", ["href", Printf.sprintf "%s/%s" stog.stog_base_url art.art_human_id],
       [ Stog_xtmpl.T ("img", [ "src", Printf.sprintf "%s/next.png" stog.stog_base_url; "alt", "next"], [])])
    ]
  with
    Not_found -> [ Stog_xtmpl.xml_of_string art.art_body ]
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
  let link = link_to_article stog ~from: `Index article in
  let link = Printf.sprintf "%s/%s" stog.stog_base_url link in
  let pubdate = rss_date_of_article article in
  let f_word w =
    { Rss.cat_name = w ; Rss.cat_domain = None }
  in
  let cats =
    (List.map f_word article.art_topics) @
    (List.map f_word article.art_keywords)
  in
  let desc = intro_of_article stog article in
  let desc =
    Stog_xtmpl.apply_to_xmls
    (Stog_xtmpl.env_of_list (default_commands stog))
    desc
  in
  let desc = String.concat "" (List.map Stog_xtmpl.string_of_xml desc) in
  Rss.item ~title: article.art_title
  ~desc
  ~link
  ~pubdate
  ~cats
  ~guid: { Rss.guid_name = link ; guid_permalink = true }
  ()
;;

let generate_rss_feed_file stog ?title link articles file =
  let arts = List.rev (Stog_types.sort_articles_by_date articles) in
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

let copy_file ?(ignerr=false) ?(quote_src=true) ?(quote_dst=true) src dest =
  let com = Printf.sprintf "cp -f %s %s"
    (if quote_src then Filename.quote src else src)
    (if quote_dst then Filename.quote dest else dest)
  in
  match Sys.command com with
    0 -> ()
  | _ ->
      let msg = Printf.sprintf "command failed: %s" com in
      (if ignerr then prerr_endline else failwith) msg
;;

let xml_of_article_body s =
  let s = Str.global_replace (Str.regexp_string tag_sep) "" s in
  Stog_xtmpl.xml_of_string s
;;

let html_of_topics stog art env args _ =
  let sep = Stog_xtmpl.xml_of_string (Stog_xtmpl.opt_arg args ~def: ", " "set") in
  let tmpl = Filename.concat stog.stog_tmpl_dir "topic.tmpl" in
  let f w =
    let env = Stog_xtmpl.env_of_list ~env [ "topic", (fun _ _ _ -> [Stog_xtmpl.D w]) ] in
    Stog_xtmpl.xml_of_string (Stog_xtmpl.apply_from_file env tmpl)
  in
  Stog_misc.list_concat ~sep
  (List.map (fun w ->
      Stog_xtmpl.T ("a", ["href", link_to (topic_index_file stog w)], [ f w ]))
   art.art_topics
  )
;;

let html_of_keywords stog art env args _ =
  let sep = Stog_xtmpl.xml_of_string (Stog_xtmpl.opt_arg args ~def: ", " "set") in
  let tmpl = Filename.concat stog.stog_tmpl_dir "keyword.tmpl" in
  let f w =
    let env = Stog_xtmpl.env_of_list ~env [ "keyword", (fun _ _ _ -> [Stog_xtmpl.D w]) ] in
    Stog_xtmpl.xml_of_string (Stog_xtmpl.apply_from_file env tmpl)
  in
  Stog_misc.list_concat ~sep
  (List.map (fun w ->
      Stog_xtmpl.T ("a", ["href", link_to (keyword_index_file stog w)], [ f w ]))
   art.art_keywords
  )
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
  let mailto =
    Printf.sprintf
    "mailto:%s?subject=%s&amp;body=%s"
    (Stog_misc.encode_string (escape_mailto_arg (String.concat ", " emails)))
    (escape_mailto_arg subject)
    (escape_mailto_arg body)
  in
  mailto
;;


let html_comment_actions stog article message =
  let href = build_mailto stog ~message article in
  (*let href = Stog_xtmpl.string_of_xml (Stog_xtmpl.D href) in*)
  Stog_xtmpl.T ("a", ["href",  href],
   [ Stog_xtmpl.T ("img", [
        "src", "../comment_reply.png" ;
        "alt", "reply to comment" ;
        "title", "reply to comment" ; ], [])
   ]
  )
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
  Stog_xtmpl.xml_of_string body
;;

let rec html_of_comments outdir stog article tmpl comments env _ _ =
  let f (Node (message, subs)) =
    let env = Stog_xtmpl.env_of_list ~env
      ([
         "date", (fun _ _ _ -> [Stog_xtmpl.D (Stog_date.mk_mail_date (Stog_date.since_epoch message.mes_time))]) ;
         "subject", (fun _ _ _ -> [Stog_xtmpl.D message.mes_subject] );
         "from", (fun _ _ _ -> [Stog_xtmpl.D message.mes_from ]);
         "to", (fun _ _ _ -> [ Stog_xtmpl.D (String.concat ", " message.mes_to)]) ;
         "body", (fun _ _ _ -> [html_of_message_body message.mes_body]) ;
         "comment-actions", (fun _ _ _ -> [html_comment_actions stog article message]) ;
         "comments", html_of_comments outdir stog article tmpl subs ;
       ] @ (default_commands ~outdir ~from:`Index stog)
      )
    in
    Stog_xtmpl.xml_of_string (Stog_xtmpl.apply_from_file env tmpl)
  in
  List.map f comments
;;

let html_of_comments outdir stog article =
  let tmpl = Filename.concat stog.stog_tmpl_dir "comment.tmpl" in
  html_of_comments outdir stog article tmpl article.art_comments
;;

let generate_blogpage stog env contents =
  let tmpl = Filename.concat stog.stog_tmpl_dir "blogpage.tmpl" in
  let f env args body = contents in
  let env = Stog_xtmpl.env_of_list ~env ["contents", f] in
  Stog_xtmpl.apply env (Stog_misc.string_of_file tmpl)
;;

let generate_article outdir stog art_id article =
  let html_file = Filename.concat outdir
    (link_to_article stog ~from: `Index article)
  in
  let tmpl = (*Filename.concat stog.stog_tmpl_dir*) "article.tmpl" in
  let art_dir = Filename.dirname html_file in
  let url = article_url stog article in
  Stog_misc.mkdir art_dir;
  List.iter (fun f -> copy_file f art_dir) article.art_files;

  let next f _ _ _ =
    match f stog art_id with
      None -> []
    | Some id ->
        let a = Stog_types.article stog id in
        let link = link_to_article stog a in
        [ Stog_xtmpl.T ("a", ["href", link], [ Stog_xtmpl.D a.art_title ]) ]
  in
  let comment_actions =
    let href = build_mailto stog article in
    (*let href = Stog_xtmpl.string_of_xml (Stog_xtmpl.D href) in*)
    [
      Stog_xtmpl.T ("a", ["href", href],
       [Stog_xtmpl.T ("img", [
            "src", "../comment.png" ;
            "alt", "Post a comment" ;
            "title", "Post a comment"], [])])
    ]
  in
  let env = Stog_xtmpl.env_of_list
    ([
     "page-title", (fun _ _ _ -> [Stog_xtmpl.D article.art_title]) ;
     "article-title", (fun _ _ _ -> [ Stog_xtmpl.D article.art_title ]) ;
     "article-url", (fun _ _ _ -> [ Stog_xtmpl.D url ]) ;

     "article-body", (fun _ _ _ -> [ xml_of_article_body article.art_body ]);
     "article-date", (fun _ _ _ -> [ Stog_xtmpl.D (Stog_types.string_of_date article.art_date) ]) ;
     "next", (next Stog_info.succ_by_date) ;
     "previous", (next Stog_info.pred_by_date) ;
     "article-keywords", html_of_keywords stog article ;
     "article-topics", html_of_topics stog article ;
     "comment-actions", (fun _ _ _ -> comment_actions);
     "comments", html_of_comments outdir stog article ;
     "navbar", fun _ _ _ -> [Stog_xtmpl.D "true"] ;
   ] @ (default_commands ~outdir stog))
  in
  let env = env_add_langswitch env stog html_file in
  let s = generate_blogpage stog env [Stog_xtmpl.T ("include", ["file", tmpl], [])] in
  Stog_xtmpl.apply_string_to_file ~head: "<!DOCTYPE HTML>" env s html_file
;;


let article_list outdir ?rss ?set stog env args _ =
  let max = Stog_misc.map_opt int_of_string
    (Stog_xtmpl.get_arg args "max")
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
    let env = Stog_xtmpl.env_of_list ~env
    ([
       "article-date", (fun _ _ _ -> [ Stog_xtmpl.D (Stog_types.string_of_date art.art_date) ]) ;
       "article-title", (fun _ _ _ -> [ Stog_xtmpl.D art.art_title ] );
       "article-url", (fun _ _ _ -> [ Stog_xtmpl.D url ]);
       "article-intro", (fun _ _ _ -> intro_of_article stog art) ;
     ] @ (default_commands ~outdir ~from:`Index stog))
    in
    Stog_xtmpl.xml_of_string (Stog_xtmpl.apply_from_file env tmpl)
  in
  let xml = List.map f_article arts in
  match rss with
    None -> xml
  | Some link ->
      (Stog_xtmpl.T ("div", ["class", "rss-button"], [
          Stog_xtmpl.T ("a", ["href", link], [
             Stog_xtmpl.T ("img", ["src", "rss.png" ; "alt", "Rss feed"], [])]) ;
        ])
      ) :: xml
;;

let generate_by_word_indexes outdir stog tmpl map f_html_file =
  let f word set =
    let base_html_file = f_html_file word in
    let html_file = Filename.concat outdir base_html_file in
    let rss_basefile = (Filename.chop_extension base_html_file)^".rss" in
    let rss_file = Filename.concat outdir rss_basefile in
    generate_rss_feed_file stog ~title: word base_html_file
    (List.map (Stog_types.article stog) (Stog_types.Art_set.elements set))
    rss_file;
    let env = Stog_xtmpl.env_of_list
      ([
         "site-title", (fun _ _ _ -> [Stog_xtmpl.D stog.stog_title]) ;
         "site-description", (fun _ _ _ -> [Stog_xtmpl.xml_of_string stog.stog_desc]) ;
         "articles", (article_list outdir ~set ~rss: rss_basefile stog);
         "page-title", (fun _ _ _ -> [Stog_xtmpl.D word]) ;
       ] @ (default_commands ~outdir ~from:`Index ~rss: rss_basefile stog))
    in
    let env = env_add_langswitch env stog html_file in
    let s = generate_blogpage stog env [Stog_xtmpl.T ("include", ["file", tmpl], [])] in
    Stog_xtmpl.apply_string_to_file ~head: "<!DOCTYPE HTML>" env s html_file
  in
  Stog_types.Str_map.iter f map
;;

let generate_topic_indexes outdir stog =
  generate_by_word_indexes outdir stog
  "by_topic.tmpl" stog.stog_arts_by_topic
  (topic_index_file stog)
;;

let generate_keyword_indexes outdir stog =
  generate_by_word_indexes outdir stog
  "by_kw.tmpl" stog.stog_arts_by_kw
  (keyword_index_file stog)
;;

let generate_archive_index outdir stog =
  let f_month year month set =
    let tmpl = "archive_month.tmpl" in
    let html_file = Filename.concat outdir (month_index_file stog ~year ~month) in
    let env = Stog_xtmpl.env_of_list
      ([
         "site-title", (fun _ _ _ -> [Stog_xtmpl.D stog.stog_title]) ;
         "site-description", (fun _ _ _ -> [Stog_xtmpl.xml_of_string stog.stog_desc]) ;
         "articles", (article_list outdir ~set stog);
         "page-title", (fun _ _ _ -> [Stog_xtmpl.D (Printf.sprintf "%s %d" months.(month-1) year)]) ;
       ] @ (default_commands ~outdir ~from:`Index stog))
    in
    let env = env_add_langswitch env stog html_file in
    let s = generate_blogpage stog env [Stog_xtmpl.T ("include", ["file", tmpl], [])] in
    Stog_xtmpl.apply_string_to_file ~head: "<!DOCTYPE HTML>" env s html_file
  in
  let f_year year mmap =
    Stog_types.Int_map.iter (f_month year) mmap
  in
  Stog_types.Int_map.iter f_year stog.stog_archives
;;

let generate_index_file outdir stog =
  let basefile = html_file stog "index" in
  let html_file = Filename.concat outdir basefile in
  let tmpl = Filename.concat stog.stog_tmpl_dir "index.tmpl" in
  let rss_basefile = "index.rss" in
  let rss_file = Filename.concat outdir rss_basefile in
  generate_rss_feed_file stog basefile
    (List.map snd (Stog_types.article_list stog)) rss_file;
  let env = Stog_xtmpl.env_of_list
    ([
       "site-title", (fun _ _ _ -> [Stog_xtmpl.D stog.stog_title]) ;
       "site-body", (fun _ _ _ -> [Stog_xtmpl.xml_of_string stog.stog_body]);
       "site-description", (fun _ _ _ -> [Stog_xtmpl.xml_of_string stog.stog_desc]) ;
       "site-url", (fun _ _ _ -> [Stog_xtmpl.D stog.stog_base_url]) ;
       "articles", (article_list outdir ~rss: rss_basefile stog);
     ] @ (default_commands ~outdir ~from:`Index ~rss: rss_basefile stog))
  in
  let env = env_add_langswitch env stog html_file in
  Stog_xtmpl.apply_to_file ~head: "<!DOCTYPE HTML>" env tmpl html_file
;;

let generate_index outdir stog =
  Stog_misc.mkdir outdir;
  copy_file ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.less") outdir;
  copy_file (Filename.concat stog.stog_tmpl_dir "less.js") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.png") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.jpg") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_dir "*.png") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_dir "*.jpg") outdir;
  generate_index_file outdir stog;
  generate_topic_indexes outdir stog;
  generate_keyword_indexes outdir stog;
  generate_archive_index outdir stog
;;

let generate outdir stog =
  begin
    match stog.stog_lang with
      None -> ()
    | Some lang -> prerr_endline (Printf.sprintf "Generating pages for language %s" lang);
  end;
  generate_index outdir stog ;
  Stog_tmap.iter (generate_article outdir stog)
    stog.stog_articles
;;

  