(** *)

let s =
  "hello <b><foo help=\"the help\"><env_ bar=\"world \"><bar/></env_></foo></b> !";;

let s2 = "<env_ sep=\"-- \"><concat sep=\"<sep/>\"><span>coucou</span><span>foo</span><span>bar</span></concat></env_>";;

let s_ = Stog_xtmpl.eval Stog_xtmpl.env_empty s;;

let f_concat atts subs =
  let sep = try List.assoc "sep" atts with Not_found -> "\n" in
  [String.concat sep subs]
;;

let env : Stog_xtmpl.env = Stog_xtmpl.env_add
  "concat" f_concat Stog_xtmpl.env_empty
;;

let s2_ = Stog_xtmpl.eval env s2;;

prerr_endline s_;;
prerr_endline s2_;;