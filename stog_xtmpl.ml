(** *)

module Str_map = Map.Make (struct type t = string let compare = compare end);;


let env_empty = Str_map.empty;;
let env_add = Str_map.add;;
let env_get s env =
  try Some (Str_map.find s env)
  with Not_found -> None
;;

let string_of_env env =
  String.concat ", " (Str_map.fold (fun s _ acc -> s :: acc) env [])
;;

let tag_main = "main_";;
let tag_env = "env_";;

type env = (env -> (string * string) list -> string list -> string list) Str_map.t

let env_add_att a v env =
  env_add a (fun _ _ _ -> [v]) env
;;

let rec eval_env env atts subs =
  prerr_endline (Printf.sprintf "eval_env: env=%s" (string_of_env env));
  let env = List.fold_left
    (fun acc (s,v) -> env_add_att s v acc)
    env atts
  in
  prerr_endline (Printf.sprintf "eval_env: env2=%s" (string_of_env env));
  List.map (fun xml -> String.concat "" (eval_xml env xml)) subs

and eval_string env s =
  try
    let xml = Xml.parse_string
      (Printf.sprintf "<%s>%s</%s>" tag_main s tag_main )
    in
    eval_xml env xml
  with
    Xml.Error e ->
      failwith
      (Printf.sprintf "Xml parse error: %s\n%s" (Xml.error e) s)

and eval_xml env = function
      Xml.PCData s -> ([s] : string list)
  | Xml.Element (tag, atts, subs) ->
      let atts = List.map
        (fun (s,v) -> (s, String.concat "" (eval_string env v)))
        atts
      in
      match tag with
        t when t = tag_env ->
          ((eval_env env atts subs) : string list)
      | _ ->
          match env_get tag env with
            Some f ->
              begin
                prerr_endline (Printf.sprintf "tag %s found" tag);
                let subs = List.map (fun xml -> String.concat "" (eval_xml env xml)) subs in
                f env atts subs
              end
          | None ->
              prerr_endline (Printf.sprintf "tag %s not found" tag);
              let subs = List.flatten (List.map (eval_xml env) subs) in
              let atts = String.concat " "
                (List.map (fun (s,v) -> Printf.sprintf "%s=%S" s v) atts)
              in
              match subs with
                [] -> [Printf.sprintf "<%s %s/>" tag atts]
              | _ -> [Printf.sprintf "<%s %s>%s</%s>" tag atts (String.concat "" subs) tag]
;;

let apply env s =
  let f_main env atts subs = subs in
  let env = env_add tag_main f_main env in
  String.concat "" (eval_string env s)
;;
let apply_from_file env file =
  let s = Stog_misc.string_of_file file in
  apply env s
;;

let apply_to_file env file out_file =
  let s = apply_from_file env file in
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

  