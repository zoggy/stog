(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** *)

type date = {
  year : int;
  month : int;
  day : int;
}
type time = Stog_date.t ;;

type message = {
    mes_time : time ;
    mes_subject : string ;
    mes_from : string ;
    mes_to : string list ;
    mes_body : string ;
    mes_id : string;
  }

type 'a tree = 'a tree_node
and 'a tree_node = Node of 'a * 'a tree list


type body = Xtmpl.tree list

type human_id = {
    hid_path : string list;
    hid_absolute : bool ;
  }
let string_of_human_id hid =
  Printf.sprintf "%s%s"
  (if hid.hid_absolute then "/" else "")
  (String.concat "/" hid.hid_path)
let human_id_of_string s =
  let len = String.length s in
  if len <= 0 then failwith (Printf.sprintf "Invalid human_id: %S" s);
  let (abs, s) =
    match s.[0] with
      '/' -> (true, String.sub s 1 (len - 1))
    | _ -> (false, s)
  in
  { hid_path = Stog_misc.split_string s ['/'];
    hid_absolute = abs ;
  }
;;

type def = string * (string * string) list * body

let get_def =
  let p name (s,_,_) = s = name in
  fun defs name ->
    try
      let (_,args, body) = List.find (p name) defs in
      Some (args, body)
    with
      Not_found -> None
;;

type elt =
  { elt_human_id : human_id ;
    elt_type : string ;
    elt_body : body ;
    elt_date : date option ;
    elt_title : string ;
    elt_keywords : string list ;
    elt_topics : string list ;
    elt_published : bool ;
    elt_defs : def list ;
    elt_src : string ;
    elt_sets : string list ;
    elt_lang_dep : bool ;
    elt_xml_doctype : string option ;
    elt_out : body option;
  }
and elt_id = elt Stog_tmap.key



let today () =
  let t = Unix.gmtime (Unix.time()) in
  {
    year = t.Unix.tm_year + 1900;
    month = t.Unix.tm_mon+1;
    day = t.Unix.tm_mday
  }
;;

let make_elt ?(typ="dummy") ?(hid={ hid_path = [] ; hid_absolute = false }) () =
  { elt_human_id = hid ;
    elt_type = typ ;
    elt_body = [] ;
    elt_date = None ;
    elt_title = "";
    elt_keywords = [] ;
    elt_topics = [] ;
    elt_published = true ;
    elt_defs = [] ;
    elt_src = "/tmp" ;
    elt_sets = [] ;
    elt_lang_dep = true ;
    elt_xml_doctype = None ;
    elt_out = None ;
  }
;;

module Str_map = Map.Make (struct type t = string let compare = compare end);;
module Str_set = Set.Make (struct type t = string let compare = compare end);;
module Hid_map = Stog_trie.Make (struct type t = string let compare = compare end);;
module Elt_set = Set.Make (struct type t = elt_id let compare = Stog_tmap.compare_key end);;
module Elt_map = Set.Make (struct type t = elt_id let compare = Stog_tmap.compare_key end);;
module Int_map = Map.Make (struct type t = int let compare = compare end);;


type edge_type =
  Date
| Topic of string
| Keyword of string
| Ref
;;

module Graph = Stog_graph.Make_with_map
  (struct
     type t = elt_id
     let compare = Stog_tmap.compare_key
   end
  )
  (struct type t = edge_type let compare = Pervasives.compare end);;

type file_tree =
{ files : Str_set.t ;
  dirs : file_tree Str_map.t ;
}

type stog_mod = {
  mod_requires : Str_set.t ;
  mod_defs : def list ;
}

type stog = {
  stog_dir : string ;
  stog_elts : (elt, elt) Stog_tmap.t ;
  stog_elts_by_human_id : elt_id Hid_map.t ;
  stog_defs : def list ;
  stog_tmpl_dir : string ;
  stog_cache_dir : string ;
  stog_title : string ;
  stog_desc : body ;
  stog_graph : Graph.t ;
  stog_elts_by_kw : Elt_set.t Str_map.t ;
  stog_elts_by_topic : Elt_set.t Str_map.t ;
  stog_archives : Elt_set.t Int_map.t Int_map.t ; (* year -> month -> article set *)
  stog_base_url : string ;
  stog_email : string ;
  stog_rss_length : int ;
  stog_lang : string option ;
  stog_outdir : string ;
  stog_main_elt : elt_id option ;
  stog_files : file_tree ;
  stog_modules : stog_mod Str_map.t ;
  }

let create_stog dir = {
  stog_dir = dir ;
  stog_elts = Stog_tmap.create (make_elt ());
  stog_elts_by_human_id = Hid_map.empty ;
  stog_tmpl_dir = Stog_config.tmpl_dir dir ;
  stog_cache_dir = Stog_config.cache_dir dir ;
  stog_title = "Blog title" ;
  stog_desc = [] ;
  stog_graph = Graph.create () ;
  stog_elts_by_kw = Str_map.empty ;
  stog_elts_by_topic = Str_map.empty ;
  stog_archives = Int_map.empty ;
  stog_base_url = "http://yourblog.net" ;
  stog_email = "foo@bar.com" ;
  stog_rss_length = 10 ;
  stog_defs = [] ;
  stog_lang = None ;
  stog_outdir = "." ;
  stog_main_elt = None ;
  stog_files = { files = Str_set.empty ; dirs = Str_map.empty } ;
  stog_modules = Str_map.empty ;
  }
;;

let elt stog id = Stog_tmap.get stog.stog_elts id;;
let elts_by_human_id ?typ stog h =
  let rev_path = List.rev h.hid_path in
  (*prerr_endline (Printf.sprintf "lookup rev_path=%s" (String.concat "/" rev_path));*)
  let ids = Hid_map.find rev_path stog.stog_elts_by_human_id in
  let l = List.map (fun id -> (id, elt stog id)) ids in
  let pred =
    match h.hid_absolute, typ with
      false, None -> None
    | false, Some typ -> Some (fun (_, elt) -> elt.elt_type = typ)
    | true, None -> Some (fun (_, elt) -> elt.elt_human_id = h)
    | true, Some typ -> Some (fun (_, elt) -> elt.elt_human_id = h && elt.elt_type = typ)
  in
  match pred with None -> l | Some pred -> List.filter pred l
;;

let elt_by_human_id ?typ stog h =
  match elts_by_human_id ?typ stog h with
    [] ->
      failwith (Printf.sprintf "Unknown element %S" (string_of_human_id h))
  | [x] -> x
  | l ->
      let msg = Printf.sprintf "More than one element matches %S%s: %s"
        (string_of_human_id h)
        (match typ with None -> "" | Some t -> Printf.sprintf " of type %S" t)
        (String.concat ", "
          (List.map (fun (id, elt) -> string_of_human_id elt.elt_human_id) l))
      in
      failwith msg
;;

let set_elt stog id elt =
  { stog with
    stog_elts = Stog_tmap.modify stog.stog_elts id elt }
;;

let add_hid stog hid id =
  let rev_path = List.rev hid.hid_path in
  let map = Hid_map.add
    rev_path id
    stog.stog_elts_by_human_id
  in
  let map =
    (*prerr_endline (Printf.sprintf "rev_path=%s" (String.concat "/" rev_path));*)
    match rev_path with
    | "index" :: q ->
        (*prerr_endline (Printf.sprintf "add again %s" (String.concat "/" q));*)
        (* also make this element accessible without "index" *)
        Hid_map.add q id map
    | _ -> map
  in
  { stog with stog_elts_by_human_id = map }
;;

let add_elt stog elt =
  let (id, elts) = Stog_tmap.add stog.stog_elts elt in
  let stog = add_hid stog elt.elt_human_id id in
  { stog with
    stog_elts = elts ;
  }
;;

let sort_elts_by_date elts =
  List.sort
  (fun e1 e2 ->
     Pervasives.compare e1.elt_date e2.elt_date)
  elts
;;

let sort_ids_elts_by_date elts =
  List.sort
  (fun (_,e1) (_,e2) ->
     Pervasives.compare e1.elt_date e2.elt_date)
  elts
;;

let elt_list ?(by_date=false) ?set stog =
  let pred =
    match set with
      None -> (fun _ -> true)
    | Some set -> (fun elt -> List.mem set elt.elt_sets)
  in
  let l =
    Stog_tmap.fold
    (fun id elt acc -> if pred elt then (id, elt) :: acc else acc)
    stog.stog_elts
    []
  in
  if by_date then sort_ids_elts_by_date l else l
;;

let merge_stogs stogs =
  match stogs with
    [] -> assert false
  | stog :: q ->
      let f acc stog =
        Stog_tmap.fold (fun _ elt acc -> add_elt acc elt)
        stog.stog_elts
        acc
      in
      List.fold_left f stog q
;;


let make_human_id stog str =
  let str = Stog_misc.lowercase str in
  let len = String.length str in
  let b = Buffer.create len in
  let rec iter dash i =
    if i >= len then
      Buffer.contents b
    else
      match str.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' ->
          Buffer.add_char b str.[i];
          iter false (i+1)
      | c ->
          if dash then
            iter dash (i+1)
          else
          (Buffer.add_char b '-' ; iter true (i+1))
  in
  let hid0 = iter true 0 in
  let rec iter n =
    let hid = Printf.sprintf "%s%s"
      hid0 (if n = 1 then "" else string_of_int n)
    in
    let hid = [ hid ] in
    match Hid_map.find hid stog.stog_elts_by_human_id with
      [] -> hid
    | _ -> iter (n+1)
  in
  iter 1
;;

exception Block_found of Xtmpl.tree
let find_block_by_id =
  let rec find_in_list id = function
    [] -> raise Not_found
  | xml :: q ->
    try find id xml
    with Not_found ->
      find_in_list id q
  and find id xml =
    match xml with
      Xtmpl.D _ -> raise Not_found
    | Xtmpl.T (_,atts,subs) ->
        begin
          match
            try Some (List.assoc "id" atts)
            with Not_found -> None
          with
            Some s when s = id -> raise (Block_found xml)
          | _ -> find_in_list id subs
        end
    | Xtmpl.E ((_,atts),subs) ->
        match
          try Some (List.assoc ("","id") atts)
          with Not_found -> None
        with
          Some s when s = id -> raise (Block_found xml)
        | _ -> find_in_list id subs
  in
  fun elt id ->
    try
      match elt.elt_out with
        None -> find_in_list id elt.elt_body
      | Some body -> find_in_list id body
    with
      Not_found -> None
    | Block_found xml -> Some xml
;;
