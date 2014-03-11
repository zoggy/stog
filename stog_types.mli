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
val parent_path : path -> path

type def = Xtmpl.name * Xtmpl.attributes * body

val get_def : def list -> Xmlm.name -> (Xtmpl.attributes * body) option

module Str_map : Map.S with type key = string
module Str_set : Set.S with type elt = string

type doc = {
  doc_path : path ;
  doc_parent : path option ;
  doc_children : path list ;
  doc_type : string ;
  doc_body : body ;
  doc_date : date option ;
  doc_title : string ;
  doc_keywords : string list ;
  doc_topics : string list ;
  doc_published : bool ;
  doc_defs : def list ;
  doc_src : string ;
  doc_sets : string list ; (** list of sets ("blog", "foo", etc.) this document belongs to *)
  doc_lang_dep : bool ; (** whether a file must be generated for each language *)
  doc_xml_doctype : string option ;
  doc_out : body option ;
  doc_used_mods : Str_set.t ;
}
type doc_id = doc Stog_tmap.key

val make_doc : ?typ:string -> ?path:path -> unit -> doc


val today : unit -> date

module Path_trie : Stog_trie.S with type symbol = string
module Doc_set : Set.S with type elt = doc_id
module Int_map : Map.S with type key = int
module Int_set : Set.S with type elt = int
module Path_map : Map.S with type key = path
module Path_set : Set.S with type elt = path

type edge_type = Date | Topic of string | Keyword of string | Ref

module Graph : Stog_graph.S with type key = doc_id and type edge_data = edge_type

type file_tree = { files : Str_set.t; dirs : file_tree Str_map.t; }

type stog_mod = {
  mod_requires : Str_set.t ;
  mod_defs : def list ;
}
type 'a dependency = File of string | Doc of 'a;;
module Depset : Set.S with type elt = string dependency;;
type stog_dependencies = Depset.t Str_map.t;;

type stog = {
  stog_dir : string;
  stog_docs : (doc, doc) Stog_tmap.t;
  stog_docs_by_path : doc_id Path_trie.t;
  stog_defs : def list;
  stog_tmpl_dir : string;
  stog_cache_dir : string;
  stog_title : string;
  stog_desc : body;
  stog_graph : Graph.t;
  stog_docs_by_kw : Doc_set.t Str_map.t;
  stog_docs_by_topic : Doc_set.t Str_map.t;
  stog_archives : Doc_set.t Int_map.t Int_map.t;
  stog_base_url : Neturl.url ;
  stog_email : string;
  stog_rss_length : int;
  stog_lang : string option;
  stog_outdir : string;
  stog_main_doc : doc_id option;
  stog_files : file_tree;
  stog_modules : stog_mod Str_map.t ;
  stog_used_mods : Str_set.t ;
  stog_depcut : bool ;
  stog_deps : stog_dependencies ;
  stog_id_map : (path * string option) Str_map.t Path_map.t ;
  stog_levels : (string * int list) list Str_map.t ;
  }

val url_of_string : string -> Neturl.url
val string_of_url : Neturl.url -> string
val url_concat : Neturl.url -> string -> Neturl.url

val create_stog : string -> stog
val stog_md5 : stog -> string

val doc : stog -> doc Stog_tmap.key -> doc

val docs_by_path : ?typ:string -> stog -> path -> (doc_id * doc) list
val doc_by_path : ?typ:string -> stog -> path -> doc_id * doc
val doc_children : stog -> doc -> doc list

val set_doc : stog -> doc Stog_tmap.key -> doc -> stog
val add_path : stog -> path -> doc_id -> stog
val add_doc : stog -> doc -> stog

val sort_docs_by_date : doc list -> doc list
val sort_ids_docs_by_date : ('a * doc) list -> ('a * doc) list
val sort_ids_docs_by_rules :
  'b -> string list -> (doc_id * doc * 'b Xtmpl.env) list -> 'b * (doc_id * doc) list

val doc_list :
  ?by_date:bool -> ?set:string -> stog -> (doc Stog_tmap.key * doc) list

val merge_stogs : stog list -> stog
val make_path : stog -> string -> string list

val find_block_by_id : doc -> string -> Xtmpl.tree option

val id_map_add : stog -> Path_map.key -> Str_map.key -> path -> string option -> stog
val map_href : stog -> Path_map.key -> Str_map.key -> Path_map.key * Str_map.key
val map_doc_ref : stog -> doc -> Str_map.key -> doc * Str_map.key
