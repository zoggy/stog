open Stog_types;;

let fun_comments stog env args subs =
  let tmpl = Filename.concat stog.stog_tmpl_dir "disqus.tmpl" in
  Xtmpl.apply_to_file stog env tmpl
;;

let () = Stog_plug.register_html_base_rule ("", "comments") fun_comments;;
