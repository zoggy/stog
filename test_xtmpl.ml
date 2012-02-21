(** *)

let s =
  "hello <b><foo help=\"the help\"><env_ bar=\"world \"><bar/></env_></foo></b> !";;

let s2 = "<env_ sep=\"-- \"><concat sep=\"<sep/>\"><span>coucou</span><span>foo</span><span>bar</span></concat></env_>";;

let s_ = Xtmpl.apply Xtmpl.env_empty s;;

let f_concat env atts subs =
  let sep = try List.assoc "sep" atts with Not_found -> "\n" in
  [String.concat sep subs]
;;

let env : Xtmpl.env = Xtmpl.env_add
  "concat" f_concat Xtmpl.env_empty
;;

let s2_ = Xtmpl.apply env s2;;

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

let s3 = string_of_file "tmpl/index_header.tmpl";;
let s3_ = Xtmpl.apply env s3;;

prerr_endline s_;;
prerr_endline s2_;;
prerr_endline s3_;;
