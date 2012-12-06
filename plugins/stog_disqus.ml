open Stog_types;;

let fun_comments env args subs =
  let stog = Stog_plug.stog () in
  let tmpl = Filename.concat stog.stog_tmpl_dir "disqus.tmpl" in
  [Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)]
;;

let () = Stog_plug.register_rule ("", "comments") fun_comments;;
