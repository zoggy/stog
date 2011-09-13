(** *)

open Stog_types;;

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

let fun_photo args =
  if Array.length args < 2 then
    ""
  else
    Printf.sprintf "<img class=\"photo\" src=\"%s\" width=\"%s\"/>" args.(0) args.(1)
;;

let default_commands tmpl_file =
  [ "include", fun_include tmpl_file ;
    "photo", fun_photo ;
  ]
;;

let link_to_article ?(from=`Article) article =
  let pref = match from with
      `Article -> "../"
    | `Index -> ""
  in
  Printf.sprintf "%s%s/index.html"
      pref article.art_human_id
;;

let mkdir dir =
  try Unix.mkdir dir 0o755
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (e, s1, s2) ->
      failwith (Printf.sprintf "%s: %s %s"
       (Unix.error_message e) s1 s2)
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

let days = [| "dimanche" ; "lundi" ; "mardi" ; "mercredi" ; "jeudi" ; "vendredi" ; "samedi" |]
let months = [|
   "janvier" ; "février" ; "mars" ; "avril" ; "mai" ; "juin" ;
   "juillet" ; "août" ; "septembre" ; "octobre" ; "novembre" ; "décembre" |];;

let string_of_date (y,m,d) =
  let tm = { Unix.tm_mday = d ; tm_mon = (m-1) ; tm_year = (y - 1900) ;
             tm_sec = 0 ; tm_min = 0 ; tm_hour = 0 ; tm_wday = 0 ;
             tm_yday = 0 ; tm_isdst = false ; }
  in
  let (_, tm) = Unix.mktime tm in
  Printf.sprintf "%s %d %s %d"
    days.(tm.Unix.tm_wday) d months.(m-1) y
;;

let string_of_body s =
  Str.global_replace (Str.regexp_string "<-->") "" s
;;

let generate_article outdir stog art_id article =
  let html_file = Filename.concat outdir
    (link_to_article ~from: `Index article)
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "article.tmpl" in
  let art_dir = Filename.dirname html_file in
  mkdir art_dir;
  List.iter (fun f -> copy_file f art_dir) article.art_files;

  Stog_tmpl.apply
  ([
     "title", (fun _ -> article.art_title) ;
     "stylefile", (fun _ -> "../style.css") ;
     "blogtitle", (fun _ -> stog.stog_title) ;
     "blogdescription", (fun _ -> stog.stog_desc) ;
     "body", (fun _ -> string_of_body article.art_body);
     "date", (fun _ -> string_of_date article.art_date) ;
   ] @ (default_commands tmpl))
  tmpl html_file
;;

let intro_of_article art =
  let re_sep = Str.regexp_string "<-->" in
  try
    let p = Str.search_forward re_sep art.art_body 0 in
    Printf.sprintf "%s ..." (String.sub art.art_body 0 p)
  with
    Not_found -> art.art_body
;;


let article_list stog =
  let arts = Stog_tmap.fold
    (fun id art acc -> (id, art) :: acc)
    stog.stog_articles []
  in
  let arts = List.sort
    (fun (_,a1) (_,a2) ->
      Pervasives.compare a2.art_date a1.art_date)
    arts
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "article_list.tmpl" in
  let link art =
    Printf.sprintf "<a href=\"%s\">%s</a>"
      (link_to_article ~from: `Index art) art.art_title
  in
  let f_article (_, art) =
    Stog_tmpl.apply_string
    ([
       "date", (fun _ -> string_of_date art.art_date) ;
       "title", (fun _ -> link art );
       "intro", (fun _ -> intro_of_article art) ;
     ] @ (default_commands tmpl))
    tmpl
  in
  String.concat "" (List.map f_article arts)
;;

let generate_index_file outdir stog =
  let html_file = Filename.concat outdir "index.html" in
  let tmpl = Filename.concat stog.stog_tmpl_dir "index.tmpl" in
  Stog_tmpl.apply
  ([
    "stylefile", (fun _ -> "style.css") ;
    "blogtitle", (fun _ -> stog.stog_title) ;
    "blogbody", (fun _ -> stog.stog_body);
    "blogdescription", (fun _ -> stog.stog_desc) ;
    "articles", (fun _ -> article_list stog);
  ] @ (default_commands tmpl))
  tmpl html_file
;;
let generate_index outdir stog =
  mkdir outdir;
  copy_file (Filename.concat stog.stog_tmpl_dir "style.css") outdir;
  copy_file ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.png") outdir;
  generate_index_file outdir stog
;;

let generate outdir stog =
  generate_index outdir stog ;
  Stog_tmap.iter (generate_article outdir stog)
    stog.stog_articles
;;

  