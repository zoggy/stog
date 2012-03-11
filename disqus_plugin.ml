open Stog_types;;

let fun_comments env args subs =
  match !Stog_html.current_stog with
    None -> assert false
  | Some stog ->
      let tmpl = Filename.concat stog.stog_tmpl_dir "disqus.tmpl" in
      [Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)]
;;

let () = Stog_html.plugin_funs :=
  ("comments", fun_comments) :: !Stog_html.plugin_funs
;;