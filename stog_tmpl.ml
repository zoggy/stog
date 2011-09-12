(** *)

let rec fix_point f x =
  let y = f x in
  if y = x then x else fix_point f y
;;

let apply funs file out_file =
  let s = Stog_misc.string_of_file file in
  let re = Str.regexp
    "<<\\([-a-zA-Z0-9_.\"' ]+\\)>>"
  in
  let subst s matched =
    let com = Str.matched_group 1 s in
    let com = Stog_coms.command_of_string com in
    try
      let f = List.assoc com.(0) funs in
      f (Array.sub com 1 ((Array.length com) - 1))
    with
      Not_found ->
        prerr_endline
        (Printf.sprintf "File %s: command %s not found" file com.(0));
        ""
  in
  let f s =
    let s = Str.global_substitute re (subst s) s in
    s
  in
  let s = fix_point f s in
  Stog_misc.file_of_string ~file: out_file s
;;