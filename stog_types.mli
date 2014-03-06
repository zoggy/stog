(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
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

(** Types. *)

type date = Netdate.t

type body = Xtmpl.tree list

type path = { path : string list; path_absolute : bool; }

val string_of_path : path -> string
val path_of_string : string -> path

type def = Xtmpl.name * Xtmpl.attributes * body

val get_def : def list -> Xmlm.name -> (Xtmpl.attributes * body) option

module Str_map : Map.S with type key = string
module Str_set : Set.S with type elt = string

type elt = {
  elt_path : path ;
  elt_parent : path option ;
  elt_children : path list ;
  elt_type : string ;
  elt_body : body ;
  elt_date : date option ;
  elt_title : string ;
  elt_keywords : string list ;
  elt_topics : string list ;
  elt_published : bool ;
  elt_defs : def list ;
  elt_src : string ;
  elt_sets : string list ; (** list of sets ("blog", "foo", etc.) this element belongs to *)
  elt_lang_dep : bool ; (** whether a file must be generated for each language *)
  elt_xml_doctype : string option ;
  elt_out : body option ;
  elt_used_mods : Str_set.t ;
}
type elt_id = elt Stog_tmap.key

val make_elt : ?typ:string -> ?path:path -> unit -> elt


val today : unit -> date

module Hid_trie : Stog_trie.S with type symbol = string
module Elt_set : Set.S with type elt = elt_id
module Int_map : Map.S with type key = int
module Int_set : Set.S with type elt = int
module Hid_map : Map.S with type key = path
module Hid_set : Set.S with type elt = path

type edge_type = Date | Topic of string | Keyword of string | Ref

module Graph : Stog_graph.S with type key = elt_id and type edge_data = edge_type

type file_tree = { files : Str_set.t; dirs : file_tree Str_map.t; }

type stog_mod = {
  mod_requires : Str_set.t ;
  mod_defs : def list ;
}
type 'a dependency = File of string | Elt of 'a;;
module Depset : Set.S with type elt = string dependency;;
type stog_dependencies = Depset.t Str_map.t;;

type stog = {
  stog_dir : string;
  stog_elts : (elt, elt) Stog_tmap.t;
  stog_elts_by_path : elt_id Hid_trie.t;
  stog_defs : def list;
  stog_tmpl_dir : string;
  stog_cache_dir : string;
  stog_title : string;
  stog_desc : body;
  stog_graph : Graph.t;
  stog_elts_by_kw : Elt_set.t Str_map.t;
  stog_elts_by_topic : Elt_set.t Str_map.t;
  stog_archives : Elt_set.t Int_map.t Int_map.t;
  stog_base_url : Neturl.url ;
  stog_email : string;
  stog_rss_length : int;
  stog_lang : string option;
  stog_outdir : string;
  stog_main_elt : elt_id option;
  stog_files : file_tree;
  stog_modules : stog_mod Str_map.t ;
  stog_used_mods : Str_set.t ;
  stog_depcut : bool ;
  stog_deps : stog_dependencies ;
  stog_id_map : (path * string option) Str_map.t Hid_map.t ;
  stog_levels : (string * int list) list Str_map.t ;
  }

val url_of_string : string -> Neturl.url
val string_of_url : Neturl.url -> string
val url_concat : Neturl.url -> string -> Neturl.url

val create_stog : string -> stog
val stog_md5 : stog -> string

val elt : stog -> elt Stog_tmap.key -> elt

val elts_by_path : ?typ:string -> stog -> path -> (elt_id * elt) list
val elt_by_path : ?typ:string -> stog -> path -> elt_id * elt
val elt_children : stog -> elt -> elt list

val set_elt : stog -> elt Stog_tmap.key -> elt -> stog
val add_path : stog -> path -> elt_id -> stog
val add_elt : stog -> elt -> stog

val sort_elts_by_date : elt list -> elt list
val sort_ids_elts_by_date : ('a * elt) list -> ('a * elt) list
val sort_ids_elts_by_rules :
  'b -> string list -> (elt_id * elt * 'b Xtmpl.env) list -> 'b * (elt_id * elt) list

val elt_list :
  ?by_date:bool -> ?set:string -> stog -> (elt Stog_tmap.key * elt) list

val merge_stogs : stog list -> stog
val make_path : stog -> string -> string list

val find_block_by_id : elt -> string -> Xtmpl.tree option

val id_map_add : stog -> Hid_map.key -> Str_map.key -> path -> string option -> stog
val map_href : stog -> Hid_map.key -> Str_map.key -> Hid_map.key * Str_map.key
val map_elt_ref : stog -> elt -> Str_map.key -> elt * Str_map.key
