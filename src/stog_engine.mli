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

(** Rewrite engine. *)

open Stog_types;;

module XR = Xtmpl_rewrite

(** Exception raised when a cache file of a loaded plugin could not be open.*)
exception Cant_open_cache_file of string

(** A function associated to a level. All functions take in parameter
  the environment, the stog and structure and the list of documents
  to rewrite. They return the new stog structure. Some
  function ([Fun_data], [Fun_stog_data]) also handle an additional data
  structure: the callbacks in the environment also handle it, and the
  function returns it with the stog structure.
*)
type 'a level_fun =
  | Fun_stog of (stog XR.env -> stog -> Doc_set.t -> stog)
  | Fun_data of ('a XR.env -> stog * 'a -> Doc_set.t -> stog * 'a)
  | Fun_stog_data of ((stog * 'a) XR.env -> stog * 'a -> Doc_set.t -> stog * 'a)

(** A structure containing data and functions associated to levels.
  Contains also the module name. *)
type 'a modul = {
      mod_data : 'a ;
      mod_levels : 'a level_fun Stog_types.Int_map.t ;
      mod_name : string ;
    }

(** A module. *)
module type Module = sig
    (** Abstract data type *)
    type data
    val modul : data modul
    type cache_data
    val cache_load : Stog_types.stog -> data -> doc -> cache_data -> data
    val cache_store : Stog_types.stog -> data -> doc -> cache_data
  end

type stog_state =
  { st_stog : stog ;
    st_modules : (module Module) list ;
    st_docs : Doc_set.t ;
  };;

val run : ?use_cache:bool ->
  ?default_style: XR.tree list -> stog_state -> stog_state

(** Generate the target files, with the following steps:
  - create the output directory,
  - build the base environment from the site global attributes,
  - compute by-topic, by-keyword and by-month documents,
  - compute documents,
  - for each level, for each document, apply level functions on the document
  - output {!Stog_types.doc.doc_out} field in the destination file.
  @use_cache reuse documents from cache, default is [true]
  @gen_cache update cache, default is [true]
*)
val generate :
  ?use_cache: bool -> ?gen_cache: bool ->
    ?default_style: XR.tree list ->
    ?only_docs:string list -> Stog_types.stog ->
    (module Module) list -> unit

(** Build the final file where the given document will be generated. *)
val doc_dst_file : Stog_types.stog -> Stog_types.doc -> string

(** Build the final url of the given document. *)
val doc_url : Stog_types.stog -> Stog_types.doc -> Stog_url.t

val env_of_defs : ?env:'a XR.env -> Stog_types.def list -> 'a XR.env
val env_of_used_mods : Stog_types.stog ->
  ?env:'a XR.env -> Stog_types.Str_set.t -> 'a XR.env

type 'a stog_doc_rules =
  Stog_types.stog -> Stog_types.doc_id -> (XR.name * 'a XR.callback) list

val get_in_env : 'a -> 'a XR.env -> Xmlm.name -> 'a * XR.tree list
val opt_in_env : 'a -> 'a XR.env -> Xmlm.name -> 'a * XR.tree list option

(** [get_in_args_or_env env args s] returns the value associated to [s]
 in [args] of else return the result of [get_in_env env s]. *)
val get_in_args_or_env : 'a -> 'a XR.env -> XR.attributes -> Xmlm.name -> 'a * XR.tree list

(** [get_path env] returns the path associated to ["doc-path"] in [env].
  @raise Stog_path.Invalid if ["doc-path"] is not found in the environment or is not a
  valid path.*)
val get_path : 'a -> 'a XR.env -> 'a * Stog_path.path

(** Same as {!get_path} but first looks for ["doc-path"] attribute in
  the given args. *)
val get_path_in_args_or_env :
  'a -> 'a XR.env -> XR.attributes -> 'a * Stog_path.path

val doc_env : 'a -> 'a XR.env -> Stog_types.stog -> Stog_types.doc -> 'a * 'a XR.env

val apply_stog_env_doc : Stog_types.stog ->
  Stog_types.stog XR.env -> Stog_types.doc_id -> Stog_types.stog

val apply_stog_data_env_doc : Stog_types.stog * 'a ->
  (Stog_types.stog * 'a) XR.env -> Stog_types.doc_id -> Stog_types.stog * 'a

val apply_data_env_doc : Stog_types.stog * 'a ->
  'a XR.env -> Stog_types.doc_id -> Stog_types.stog * 'a

val fun_apply_stog_doc_rules : Stog_types.stog stog_doc_rules -> 'a level_fun
val fun_apply_stog_data_doc_rules : (Stog_types.stog * 'a) stog_doc_rules -> 'a level_fun
val fun_apply_data_doc_rules : 'a stog_doc_rules -> 'a level_fun


val get_languages : 'a -> 'a XR.env -> 'a * string list

(** {2 Registering modules} *)

type module_fun = Stog_types.stog -> (module Module)

val modules : unit -> (string * module_fun) list
val register_module : string -> module_fun -> unit
val module_by_name : string -> module_fun option
