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

type contents_kind = [`Text | `Html | `Xml]
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


type body = Xml of Xtmpl.tree | String of string

type human_id = string list;;

type elt =
  { elt_human_id : human_id ;
    elt_type : string ;
    elt_body : body ;
    elt_date : date option ;
    elt_title : string ;
    elt_keywords : string list ;
    elt_topics : string list ;
    elt_published : bool ;
    elt_vars : (string * string) list ;
    elt_src : string ;
    elt_streams : human_id list ; (* list of streams (blog, etc.) this element belongs to *)
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

let dummy_elt () =
  { elt_human_id = [] ;
    elt_type = "dummy" ;
    elt_body = String "" ;
    elt_date = None ;
    elt_title = "Dummy title";
    elt_keywords = [] ;
    elt_topics = [] ;
    elt_published = true ;
    elt_vars = [] ;
    elt_src = "/tmp" ;
    elt_streams = [] ;
  }
;;

module Str_map = Map.Make (struct type t = string let compare = compare end);;
module Hid_map = Stog_trie.Make (struct type t = string let compare = compare end);;
module Elt_set = Set.Make (struct type t = elt_id let compare = Stog_tmap.compare_key end);;
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

type stog = {
  stog_dir : string ;
  stog_elts : (elt, elt) Stog_tmap.t ;
  stog_elts_by_human_id : elt_id Hid_map.t ;
  stog_vars : (string * string) list ;
  stog_tmpl_dir : string ;
  stog_title : string ;
  stog_body : body ;
  stog_desc : body ;
  stog_graph : Graph.t ;
  stog_elts_by_kw : Elt_set.t Str_map.t ;
  stog_elts_by_topic : Elt_set.t Str_map.t ;
  stog_archives : Elt_set.t Int_map.t Int_map.t ; (* year -> month -> article set *)
  stog_base_url : string ;
  stog_email : string ;
  stog_rss_length : int ;
  stog_lang : string option ;
  }

let create_stog dir = {
  stog_dir = dir ;
  stog_elts = Stog_tmap.create (dummy_elt ());
  stog_elts_by_human_id = Hid_map.empty ;
  stog_tmpl_dir = Filename.concat dir "tmpl" ;
  stog_title = "Blog title" ;
  stog_body = String "" ;
  stog_desc = String "" ;
  stog_graph = Graph.create () ;
  stog_elts_by_kw = Str_map.empty ;
  stog_elts_by_topic = Str_map.empty ;
  stog_archives = Int_map.empty ;
  stog_base_url = "http://yourblog.net" ;
  stog_email = "foo@bar.com" ;
  stog_rss_length = 10 ;
  stog_vars = [] ;
  stog_lang = None ;
  }
;;

let elt stog id = Stog_tmap.get stog.stog_elts id;;
let elts_by_human_id stog h =
  let h = List.rev h in
  let ids = Hid_map.find h stog.stog_elts_by_human_id in
  List.map (fun id -> (id, elt stog id)) ids
;;

let set_elt stog id elt =
  { stog with
    stog_elts = Stog_tmap.modify stog.stog_elts id elt }
;;

let add_elt stog elt =
  let (id, elts) = Stog_tmap.add stog.stog_elts elt in
  let map = Hid_map.add
    (List.rev elt.elt_human_id) id
    stog.stog_elts_by_human_id
  in
  { stog with
    stog_elts = elts ;
    stog_elts_by_human_id = map ;
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

let elt_list ?(by_date=false) stog =
  let l =
    Stog_tmap.fold
    (fun id elt acc -> (id, elt) :: acc)
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
        let (elts, by_hid) =
          Stog_tmap.fold
          (fun _ elt (elts, by_hid) ->
             let (id, elts) = Stog_tmap.add elts elt in
             let by_hid = Hid_map.add elt.elt_human_id id by_hid in
             (elts, by_hid)
          )
          stog.stog_elts
          (acc.stog_elts, acc.stog_elts_by_human_id)
        in
        { acc with
          stog_elts = elts ;
          stog_elts_by_human_id = by_hid ;
        }
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
    try
      ignore(Str_map.find hid stog.stog_art_by_human_id);
      iter (n+1)
    with Not_found -> hid
  in
  iter 1
;;
