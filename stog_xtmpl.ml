(** *)

module Str_map = Map.Make (struct type t = string let compare = compare end);;


let env_empty = Str_map.empty;;
let env_add = Str_map.add;;
let env_get s env =
  try Some (Str_map.find s env)
  with Not_found -> None
;;
let rec fix_point ?(n=0) f x =
  let file = Printf.sprintf "/tmp/fixpoint%d.txt" n in
  Stog_misc.file_of_string ~file x;
  let y = f x in
  if y = x then x else fix_point ~n: (n+1) f y
;;

let string_of_env env =
  String.concat ", " (Str_map.fold (fun s _ acc -> s :: acc) env [])
;;

let tag_main = "main_";;
let tag_env = "env_";;

type tree =
  | E of Xmlm.tag * tree list
  | T of string * (string * string) list * tree list
  | D of string

type env = (env -> (string * string) list -> tree list -> tree list) Str_map.t

let pad = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> "
let pad_len = String.length pad;;

let string_of_xml tree =
  try
    let b = Buffer.create 256 in
    let output = Xmlm.make_output (`Buffer b) in
    let frag = function
    | E (tag, childs) -> `El (tag, childs)
    | T (tag, atts, childs) ->
        let tag = ("", tag) in
        let atts = List.map (fun (s,v) -> (("",s), v)) atts in
        `El ((tag, atts), childs)
    | D d -> `Data d
    in
    Xmlm.output_doc_tree frag output (None, tree);
    let s = Buffer.contents b in
    let len = String.length s in
    String.sub s pad_len (len - pad_len)
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg
;;

let xml_of_string ?(add_main=true) s =
  let s =
    if add_main then
      Printf.sprintf "<%s>%s</%s>" tag_main s tag_main
    else
      s
  in
  try
    let input = Xmlm.make_input ~enc: (Some `UTF_8) (`String (0, s)) in
    let el tag childs = E (tag, childs)  in
    let data d = D d in
    let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
    tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s\n%s"
        line col (Xmlm.error_message error) s
      in
      failwith msg
  | Invalid_argument e ->
      let msg = Printf.sprintf "%s:\n%s" e s in
      failwith msg
;;

let env_add_att a v env =
  env_add a (fun _ _ _ -> [xml_of_string v]) env
;;


let rec eval_env ?(margin="")env atts subs =
  let env = List.fold_left
    (fun acc ((_,s),v) -> env_add_att s v acc)
    env atts
  in
  List.flatten (List.map (eval_xml ~margin env) subs)

and eval_xml ?(margin="") env = function
| (D _) as xml -> [ xml ]
| other ->
    let (tag, atts, subs) =
      match other with
        D _ -> assert false
      | T (tag, atts, subs) ->
        (("", tag), List.map (fun (s,v) -> (("",s), v)) atts, subs)
      | E ((tag, atts), subs) -> (tag, atts, subs)
    in
    let margin = margin^"  " in
    let f = function
      (("",s), v) ->
        let v2 = eval_string ~margin env v in
        (("", s), v2)
    | _ as att -> att
    in
    let atts = List.map f atts in
    match tag with
      ("", t) when t = tag_env -> ((eval_env env atts subs) : tree list)
    | (uri, tag) ->
        match uri, env_get tag env with
        | "", Some f ->
            begin
              let subs = List.flatten
                (List.map (eval_xml ~margin env) subs)
              in
              f env (List.map (fun ((_,s),v) -> (s,v)) atts) subs
            end
        | _ ->
            let subs = List.flatten (List.map (eval_xml ~margin env) subs) in
            [ E (((uri, tag), atts), subs) ]

and eval_string ?(margin="") env s =
  let xml = xml_of_string s in
  let f_main env atts subs = subs in
  let env = env_add tag_main f_main env in
  String.concat "" (List.map string_of_xml (eval_xml env xml))
;;

let apply env s = fix_point (eval_string env) s;;

let apply_from_file env file =
  let s = Stog_misc.string_of_file file in
  apply env s
;;

let apply_to_xmls env l =
  List.flatten (List.map (eval_xml env) l)
;;

let apply_to_file ?(head="") env file out_file =
  let s = apply_from_file env file in
  let s = head^s in
  Stog_misc.file_of_string ~file: out_file s
;;

let apply_string_to_file env s out_file =
  let s = apply env s in
  Stog_misc.file_of_string ~file: out_file s
;;

let get_arg args name =
  try Some (List.assoc name args)
  with Not_found -> None
;;

let string_of_args args =
  String.concat " " (List.map (fun (s,v) -> Printf.sprintf "%s=%S" s v) args)
;;

let opt_arg args ?(def="") name =
  match get_arg args name with None -> def | Some s -> s
;;


let env_of_list ?(env=env_empty) l =
  List.fold_left (fun env (name, f) -> env_add name f env) env l
;;

  