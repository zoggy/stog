open Stog_types;;

let fun_comments stog env args subs =
  let (stog, path) = Stog_engine.get_path stog env in
  let (_, doc) = Stog_types.doc_by_path stog path in
  let tmpl = Stog_tmpl.get_template_file stog doc "disqus.tmpl" in
  Xtmpl.apply_to_file stog env tmpl
;;

let () = Stog_plug.register_html_base_rule ("", "comments") fun_comments;;
