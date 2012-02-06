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

let xml_of_string s =
  let s = Printf.sprintf "<%s>%s</%s>" tag_main s tag_main in
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

(*
let escape_pcdata s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | '>' -> Buffer.add_string b "&gt;"
    | '<' -> Buffer.add_string b "&lt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '\'' -> Buffer.add_string b "&apos;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c
  done;
  let s = Buffer.contents b in
  prerr_endline (Printf.sprintf "#%s#" s);
  s
;;

let escape_quotes s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | '"' -> Buffer.add_string b "\\\""
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b
;;

let string_of_xml =
  let print_att b (s, v) =
    Printf.bprintf b " %s=\"%s\"" s (escape_quotes v)
  in
  let rec iter b = function
    Xml.PCData "" -> ()
  | Xml.PCData s -> Buffer.add_string b (escape_pcdata s)
  | Xml.Element (tag, atts, subs) ->
     Printf.bprintf b "<%s" tag;
     List.iter (print_att b) atts;
     Buffer.add_char b '>';
     iter_list b subs;
     Printf.bprintf b "</%s>" tag
  and iter_list b = function
    | [] -> ()
    | (Xml.PCData p1) :: (Xml.PCData p2) :: q ->
          iter_list b ((Xml.PCData (p1^"\n"^p2)) :: q)
    | h :: q -> iter b h ; iter_list b q
  in
  fun xml ->
    let b = Buffer.create 256 in
    iter b xml;
    Buffer.contents b
;;
*)

let env_add_att a v env =
  env_add a (fun _ _ _ -> [xml_of_string v]) env
;;


let rec eval_env ?(margin="")env atts subs =
  (*prerr_endline (Printf.sprintf "eval_env: env=%s" (string_of_env env));*)
  let env = List.fold_left
    (fun acc ((_,s),v) -> env_add_att s v acc)
    env atts
  in
  (*prerr_endline (Printf.sprintf "eval_env: env2=%s" (string_of_env env));*)
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
    (*prerr_endline (Printf.sprintf "%s<%s>" margin tag);*)
    let f = function
      (("",s), v) ->
        let v2 = eval_string ~margin env v in
        prerr_endline (Printf.sprintf "attribute: %s -> %s" v v2);
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
              (*prerr_endline (Printf.sprintf "tag %s found" tag);*)
              let subs = List.flatten
                (List.map (eval_xml ~margin env) subs)
              in
              f env (List.map (fun ((_,s),v) -> (s,v)) atts) subs
            end
        | _ ->
            (*prerr_endline (Printf.sprintf "tag %s not found" tag);*)
            let subs = List.flatten (List.map (eval_xml ~margin env) subs) in
            (*
               let atts = String.concat " "
               (List.map (fun (s,v) -> Printf.sprintf "%s=%S" s v) atts)
               in
               *)
            [ E (((uri, tag), atts), subs) ]

and eval_string ?(margin="") env s =
  let xml = xml_of_string s in
  prerr_endline (Printf.sprintf "eval_string s=%s\nxml=%s\n===============\n"
   s (string_of_xml xml));
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

  