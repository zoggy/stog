(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2015 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
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

module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

type date = Netdate.t

type 'a tree = 'a tree_node
and 'a tree_node = Node of 'a * 'a tree list

type body = XR.tree list


type def = XR.name * XR.attributes * body

let get_def =
  let p name (s,_,_) = s = name in
  fun defs name ->
    try
      let (_,args, body) = List.find (p name) defs in
      Some (args, body)
    with
      Not_found -> None
;;

module Str_map = Map.Make (struct type t = string let compare = String.compare end);;
module Str_set = Set.Make (struct type t = string let compare = String.compare end);;

type doc =
  { doc_path : Stog_path.path ;
    doc_parent : Stog_path.path option ;
    doc_children : Stog_path.path list ;
    doc_type : string ;
    doc_prolog : Xml.prolog option ;
    doc_body : body ;
    doc_date : date option ;
    doc_title : string ;
    doc_keywords : string list ;
    doc_topics : string list ;
    doc_defs : def list ;
    doc_src : string ;
    doc_sets : string list ;
    doc_lang_dep : bool ;
    doc_out : body option;
    doc_used_mods : Str_set.t ;
  }
and doc_id = doc Stog_tmap.key

let today () = Netdate.create (Unix.time()) ;;

let make_doc ?(typ="dummy") ?(path=Stog_path.path [] false) ?prolog () =
  { doc_path = path ;
    doc_parent = None ;
    doc_children = [] ;
    doc_type = typ ;
    doc_prolog = prolog ;
    doc_body = [] ;
    doc_date = None ;
    doc_title = "";
    doc_keywords = [] ;
    doc_topics = [] ;
    doc_defs = [] ;
    doc_src = "/tmp" ;
    doc_sets = [] ;
    doc_lang_dep = true ;
    doc_out = None ;
    doc_used_mods = Str_set.empty ;
  }
;;

module Path_trie = Stog_trie.Make (struct type t = string let compare = compare end);;
module Doc_set = Set.Make (struct type t = doc_id let compare = Stog_tmap.compare_key end);;
module Doc_map = Set.Make (struct type t = doc_id let compare = Stog_tmap.compare_key end);;
module Int_map = Map.Make (struct type t = int let compare = compare end);;
module Int_set = Set.Make (struct type t = int let compare = compare end);;



type edge_type =
  Date
| Topic of string
| Keyword of string
| Ref
;;

module Graph = Stog_graph.Make_with_map
  (struct
     type t = doc_id
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

type 'a dependency = File of string | Doc of 'a;;

module Depset =
  Set.Make (struct type t = string dependency let compare = Pervasives.compare end)

type stog_dependencies = Depset.t Str_map.t;;

type stog = {
  stog_dir : string ;
  stog_docs : (doc, doc) Stog_tmap.t ;
  stog_docs_by_path : doc_id Path_trie.t ;
  stog_defs : def list ;
  stog_tmpl_dirs : string list ;
  stog_mod_dirs : string list ;
  stog_cache_dir : string ;
  stog_title : string ;
  stog_desc : body ;
  stog_graph : Graph.t ;
  stog_docs_by_kw : Doc_set.t Str_map.t ;
  stog_docs_by_topic : Doc_set.t Str_map.t ;
  stog_archives : Doc_set.t Int_map.t Int_map.t ; (* year -> month -> article set *)
  stog_base_url : Stog_url.t ;
  stog_email : string ;
  stog_rss_length : int ;
  stog_lang : string option ;
  stog_outdir : string ;
  stog_main_doc : doc_id option ;
  stog_files : file_tree ;
  stog_modules : stog_mod Str_map.t ;
  stog_used_mods : Str_set.t ;
  stog_depcut : bool ;
  stog_deps : stog_dependencies ;
  stog_id_map : (Stog_path.path * string option) Str_map.t Stog_path.Map.t ;
  stog_levels : (string * int list) list Str_map.t ;
  stog_publish_only : Stog_filter_types.t option ;
  stog_source : [`Dir | `File] ;
}



let create_stog ?(source=`Dir) dir = {
  stog_dir = dir ;
  stog_docs = Stog_tmap.create (make_doc ());
  stog_docs_by_path = Path_trie.empty ;
  stog_tmpl_dirs = [Stog_config.tmpl_dir dir] ;
  stog_mod_dirs = [Stog_config.modules_dir dir ; Stog_install.modules_dir ] ;
  stog_cache_dir = Stog_config.cache_dir dir ;
  stog_title = "" ;
  stog_desc = [] ;
  stog_graph = Graph.create () ;
  stog_docs_by_kw = Str_map.empty ;
  stog_docs_by_topic = Str_map.empty ;
  stog_archives = Int_map.empty ;
  stog_base_url = Stog_url.of_string "http://yoursite.net" ;
  stog_email = "foo@bar.com" ;
  stog_rss_length = 10 ;
  stog_defs = [] ;
  stog_lang = None ;
  stog_outdir = "." ;
  stog_main_doc = None ;
  stog_files = { files = Str_set.empty ; dirs = Str_map.empty } ;
  stog_modules = Str_map.empty ;
  stog_used_mods = Str_set.empty ;
  stog_depcut = false ;
  stog_deps = Str_map.empty ;
  stog_id_map = Stog_path.Map.empty ;
  stog_levels = Str_map.empty ;
  stog_publish_only =
      Some (Stog_filter_types.Not
       (Stog_filter_types.Or
        (Stog_filter_types.Pred (("","published"), "false"),
         Stog_filter_types.Pred (("","published"), "0"))
       )) ;
  stog_source = source ;
  }
;;

let stog_md5 stog =
  let stog =
    { stog with
      stog_docs = Stog_tmap.create (make_doc ());
      stog_docs_by_path = Path_trie.empty ;
      stog_graph = Graph.create ();
      stog_docs_by_kw = Str_map.empty ;
      stog_docs_by_topic = Str_map.empty ;
      stog_archives = Int_map.empty ;
      stog_files = { files = Str_set.empty ; dirs = Str_map.empty } ;
      stog_depcut = false;
    }
  in
  let s = Digest.string (Marshal.to_string stog [Marshal.Closures ; Marshal.No_sharing]) in
  Digest.to_hex s
;;

let doc stog id = Stog_tmap.get stog.stog_docs id;;
let docs_by_path ?typ stog h =
  let rev_path = List.rev h.Stog_path.path in
  (*prerr_endline (Printf.sprintf "lookup rev_path=%s" (String.concat "/" rev_path));*)
  let ids = Path_trie.find rev_path stog.stog_docs_by_path in
  let l = List.map (fun id -> (id, doc stog id)) ids in
  let path_pred (_, doc) =
    doc.doc_path = h ||
      (match Stog_path.chop_extension doc.doc_path with
         None -> true
       | Some p -> p = h)
  in
  let pred =
    match h.Stog_path.path_absolute, typ with
      false, None -> None
    | false, Some typ -> Some (fun (_, doc) -> doc.doc_type = typ)
    | true, None -> Some path_pred
    | true, Some typ -> Some (fun (id, doc) -> path_pred (id,doc) && doc.doc_type = typ)
  in
  match pred with None -> l | Some pred -> List.filter pred l
;;

let doc_by_path ?typ stog h =
  match docs_by_path ?typ stog h with
    [] ->
      (*prerr_endline (Path_trie.to_string (fun x -> x) stog.stog_docs_by_path);*)
      failwith (Printf.sprintf "Unknown document %S" (Stog_path.to_string h))
  | [x] -> x
  | l ->
      let msg = Printf.sprintf "More than one document matches %S%s: %s"
        (Stog_path.to_string h)
        (match typ with None -> "" | Some t -> Printf.sprintf " of type %S" t)
        (String.concat ", "
          (List.map (fun (id, doc) -> Stog_path.to_string doc.doc_path) l))
      in
      failwith msg
;;

let doc_children stog =
  let f path = snd (doc_by_path stog path) in
  fun doc -> List.map f doc.doc_children
;;

let set_doc stog id doc =
  (*prerr_endline (Printf.sprintf "set_doc %d => %s" (Obj.magic id) (Stog_path.to_string doc.doc_path));*)
  { stog with
    stog_docs = Stog_tmap.modify stog.stog_docs id doc }
;;

let add_path =
  let add ~fail stog path id =
    let rev_path = List.rev path.Stog_path.path in
    let map = Path_trie.add ~fail
      rev_path id stog.stog_docs_by_path
    in
    let map =
      (*prerr_endline (Printf.sprintf "rev_path=%s" (String.concat "/" rev_path));*)
      match rev_path with
      | "index.html" :: q
      | "index" :: q when not fail ->
          (* if [fail = false] then we already added the path with index.html,
             so we do not add the path for index. *)

          (*prerr_endline (Printf.sprintf "add again %s" (String.concat "/" q));*)
          (* also make this document accessible without "index" *)
          Path_trie.add ~fail q id map
      | _ -> map
    in
    { stog with stog_docs_by_path = map }
  in
  fun stog path id ->
    let stog = add ~fail: true stog path id in
    match Stog_path.chop_extension path with
      None -> stog
    | Some path -> add ~fail: false stog path id
;;

let add_doc stog doc =
  let (id, docs) = Stog_tmap.add stog.stog_docs doc in
  let stog = add_path stog doc.doc_path id in
  { stog with
    stog_docs = docs ;
  }
;;

let sort_docs_by_date docs =
  List.sort
  (fun e1 e2 ->
     Pervasives.compare e1.doc_date e2.doc_date)
  docs
;;

let sort_ids_docs_by_date docs =
  List.sort
  (fun (_,e1) (_,e2) ->
     Pervasives.compare e1.doc_date e2.doc_date)
  docs
;;


let sort_ids_docs_by_rules =
  let apply_field env (data, acc) field =
    let name = Xtmpl_xml.name_of_string field in
    let xml = [XR.node name []] in
    let (data, xmls) = XR.apply_to_xmls data env xml in
    (data, xmls :: acc)
  in
  let apply_fields fields (data,acc) (id,e,env) =
    let (data, xmls) = List.fold_left (apply_field env) (data,[]) fields in
    let xmls = List.flatten (List.rev xmls) in
    (data, (id,e, xmls) :: acc)
  in
  let compare (_, e1, v1) (_, e2, v2) =
    Pervasives.compare v1 v2
  in
  fun data fields docs ->
    let (data, docs) = List.fold_left (apply_fields fields) (data,[]) docs in
    let docs = List.sort compare docs in
    (data, List.map (fun (id,e,_) -> (id, e)) docs)
;;

let doc_list ?(by_date=false) ?set stog =
  let pred =
    match set with
      None -> (fun _ -> true)
    | Some set -> (fun doc -> List.mem set doc.doc_sets)
  in
  let l =
    Stog_tmap.fold
    (fun id doc acc -> if pred doc then (id, doc) :: acc else acc)
    stog.stog_docs
    []
  in
  if by_date then sort_ids_docs_by_date l else l
;;

let merge_stogs stogs =
  match stogs with
    [] -> assert false
  | stog :: q ->
      let f acc stog =
        Stog_tmap.fold (fun _ doc acc -> add_doc acc doc)
        stog.stog_docs
        acc
      in
      List.fold_left f stog q
;;


let make_path stog str =
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
  let path0 = iter true 0 in
  let rec iter n =
    let path = Printf.sprintf "%s%s"
      path0 (if n = 1 then "" else string_of_int n)
    in
    let path = [ path ] in
    match Path_trie.find path stog.stog_docs_by_path with
      [] -> path
    | _ -> iter (n+1)
  in
  iter 1
;;

exception Block_found of XR.tree
let find_block_by_id =
  let rec find_in_list id = function
    [] -> raise Not_found
  | xml :: q ->
    try find id xml
    with Not_found ->
      find_in_list id q
  and find id xml =
    match xml with
      XR.D _ | XR.C _ | XR.PI _ -> raise Not_found
    | XR.E { XR.atts ; subs } ->
        match XR.get_att_cdata atts ("","id") with
          Some s when s = id -> raise (Block_found xml)
        | _ -> find_in_list id subs
  in
  fun doc id ->
    try
      match doc.doc_out with
        None -> find_in_list id doc.doc_body
      | Some body -> find_in_list id body
    with
      Not_found -> None
    | Block_found xml -> Some xml
;;

let id_map_add stog path id target_path target_id =
  assert path.Stog_path.path_absolute ;
  assert target_path.Stog_path.path_absolute ;
  let map =
    try Stog_path.Map.find path stog.stog_id_map
    with Not_found -> Str_map.empty
  in
  let map = Str_map.add id (target_path, target_id) map in
  { stog with stog_id_map = Stog_path.Map.add path map stog.stog_id_map }
;;

let rec map_href stog path id =
  try
    let map = Stog_path.Map.find path stog.stog_id_map in
    match Str_map.find id map with
      (path, None) -> (path, "")
    | (path, Some id) -> map_href stog path id
  with Not_found -> (path, id)
;;

let map_doc_ref stog doc id =
  let path = doc.doc_path in
  let (path, id) = map_href stog path id in
  let (_, doc) = doc_by_path stog path in
  (doc, id)
;;
