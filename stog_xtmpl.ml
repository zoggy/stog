(** *)

module Str_map = Map.Make (struct type t = string let compare = compare end);;


let env_empty = Str_map.empty;;
let env_add = Str_map.add;;
let env_get s env =
  try Some (Str_map.find s env)
  with Not_found -> None
;;
let rec fix_point f x =
  let y = f x in
  if y = x then x else fix_point f y
;;

let string_of_env env =
  String.concat ", " (Str_map.fold (fun s _ acc -> s :: acc) env [])
;;

let tag_main = "main_";;
let tag_env = "env_";;

type env = (env -> (string * string) list -> Xml.xml list -> Xml.xml list) Str_map.t

let xml_of_string s =
  try
    Xml.parse_string
    (Printf.sprintf "<%s>%s</%s>" tag_main s tag_main )
  with
      Xml.Error e ->
      failwith
      (Printf.sprintf "Xml parse error: %s\n%s" (Xml.error e) s)
;;
let env_add_att a v env =
  env_add a (fun _ _ _ -> [xml_of_string v]) env
;;


let rec eval_env ?(margin="")env atts subs =
  (*prerr_endline (Printf.sprintf "eval_env: env=%s" (string_of_env env));*)
  let env = List.fold_left
    (fun acc (s,v) -> env_add_att s v acc)
    env atts
  in
  (*prerr_endline (Printf.sprintf "eval_env: env2=%s" (string_of_env env));*)
  List.flatten (List.map (eval_xml ~margin env) subs)

and eval_xml ?(margin="") env = function
| (Xml.PCData _) as xml -> [ xml ]
| Xml.Element (tag, atts, subs) ->
    let margin = margin^"  " in
    (*prerr_endline (Printf.sprintf "%s<%s>" margin tag);*)
    let atts = List.map
      (fun (s,v) -> (s, eval_string ~margin env v))
      atts
    in
    match tag with
      t when t = tag_env -> ((eval_env env atts subs) : Xml.xml list)
    | _ ->
        match env_get tag env with
          Some f ->
            begin
              (*prerr_endline (Printf.sprintf "tag %s found" tag);*)
              let subs = List.flatten
                (List.map (eval_xml ~margin env) subs)
              in
              f env atts subs
            end
        | None ->
            (*prerr_endline (Printf.sprintf "tag %s not found" tag);*)
            let subs = List.flatten (List.map (eval_xml ~margin env) subs) in
            (*
               let atts = String.concat " "
               (List.map (fun (s,v) -> Printf.sprintf "%s=%S" s v) atts)
               in
               *)
            [ Xml.Element (tag, atts, subs) ]

and eval_string ?(margin="") env s =
  let xml = xml_of_string s in
  prerr_endline (Printf.sprintf "eval_string s=%s\nxml=%s\n===============\n"
   s (Xml.to_string xml));
  let f_main env atts subs = subs in
  let env = env_add tag_main f_main env in
  String.concat "" (List.map Xml.to_string (eval_xml env xml))
;;

let apply env s = fix_point (eval_string env) s;;

let apply_from_file env file =
  let s = Stog_misc.string_of_file file in
  apply env s
;;

let apply_to_xmls env l =
  List.flatten (List.map (eval_xml env) l)
;;

let apply_to_file env file out_file =
  let s = apply_from_file env file in
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

  