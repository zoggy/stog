(** *)

let env_empty = Stog_types.Str_map.empty;;
let env_add = Stog_types.Str_map.add;;
let env_get s env =
  try Stog_types.Str_map.find s env
  with Not_found -> ""
;;

let rec fix_point f x =
  let y = f x in
  if y = x then x else fix_point f y
;;


let fun_set env args =
  if Array.length args < 2 then
    failwith "Missing argument to 'set' command";
  ("", env_add args.(0) args.(1) env)
;;

let fun_if env args =
  if Array.length args < 3 then
    failwith "Missing argument to 'if' command";
  if env_get args.(0) env = args.(1) then
    (args.(2), env)
  else
    ("", env)
;;

let env_funs = ["set", fun_set ; "if", fun_if ];;

let apply_string funs file =
  let s = Stog_misc.string_of_file file in
  let re = Str.regexp
    "<<\\([-a-zA-Z0-9_.\"' ]+\\)>>"
  in
  let re_env = Str.regexp
    "<\\[\\([^]]+\\)\\]>"
  in
  let env = ref env_empty in
  let subst_with_env s matched =
    let com = Str.matched_group 1 s in
    let com = Stog_coms.command_of_string com in
    try
      let f = List.assoc com.(0) env_funs in
      let (s, e) = f !env (Array.sub com 1 ((Array.length com) - 1)) in
      env := e;
      s
    with
      Not_found ->
        prerr_endline
        (Printf.sprintf "File %s: command %s not found" file com.(0));
        ""
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
    let s = Str.global_substitute re_env (subst_with_env s) s in
    let s = Str.global_substitute re (subst s) s in
    s
  in
  fix_point f s
;;

let apply funs file out_file =
  let s = apply_string funs file in
  Stog_misc.file_of_string ~file: out_file s
;;

