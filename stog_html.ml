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

let copy_file src dest =
  let com = Printf.sprintf "cp -f %s %s"
    (Filename.quote src) (Filename.quote dest)
  in
  match Sys.command com with
    0 -> ()
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let generate_article outdir stog art_id article =
  let html_file = Filename.concat outdir
    (link_to_article ~from: `Index article)
  in
  let tmpl = Filename.concat stog.stog_tmpl_dir "article.tmpl" in
  mkdir (Filename.dirname html_file);
  Stog_tmpl.apply
  [ "include", (fun_include tmpl) ;
    "title", (fun _ -> article.art_title) ;
    "stylefile", (fun _ -> "../style.css") ;
    "blogtitle", (fun _ -> stog.stog_title) ;
    "body", (fun _ -> article.art_body);
  ]
  tmpl html_file
;;


let generate_index outdir stog =
  mkdir outdir;
  copy_file (Filename.concat stog.stog_tmpl_dir "style.css") outdir
;;

let generate outdir stog =
  generate_index outdir stog ;
  Stog_tmap.iter (generate_article outdir stog)
    stog.stog_articles
;;

  