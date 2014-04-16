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

(** Base module.
*)

(** [doc_by_href ?typ ?src_doc stog env href] returns the document, path and
  optional id matching the given href string, of the form [path[#id]].
  Return None if the document could not be found, of the id could not be found,
  and an error is issued.
  @param src_doc can be used to specify the source document, to improve
  the error message. *)
val doc_by_href : ?typ: string -> ?src_doc: Stog_types.doc ->
  Stog_types.stog -> 'a -> 'a Xtmpl.env -> string ->
    'a * (Stog_types.doc * string * string option) option

(*
(** Adding a known block id for a given path. A short and a long title
  are specified. These registered blocks are used by <doc href="..#id"/> nodes.
  @on_dup specifies what to do when the id to add is already present.
  Default is to issue a warning. [`Fail] will raise a [Failure] exception.
*)
val add_block :
  ?on_dup: [`Ignore | `Fail | `Warn] ->
  path: string -> id: string -> short: Xtmpl.tree -> long: Xtmpl.tree -> unit -> unit
*)

(** [get_in_env env tag] will look for the given string in the environment,
  by building a <tag/> node and evaluating it. If the result is the same node,
  then return [""] else return the reduced value as a string.*)
val get_in_env : 'a -> 'a Xtmpl.env -> Xmlm.name -> 'a * Xtmpl.tree list

(** [get_path env] returns the path associated to ["path"] in [env].
  @raise Assert_failure if ["path"] is not found in the environment.*)
val get_path : 'a -> 'a Xtmpl.env -> 'a * Stog_path.path

(** Escape html code in the given string: change [&] to [&amp;],
  [<] to [&lt;] and [>] to [&gt;].*)
val escape_html : string -> string

val concat_name : ?sep: string -> (string * string) -> string

(** Build an url from the given path.
  This is used for documents created on the fly, like by-word or by-month index. *)
val url_of_path :
  Stog_types.stog -> Stog_path.path -> Neturl.url

(*
(** Generate a RSS file from the given list of documents. The final RSS
  url must be given as it is embedded in the RSS file. *)
val generate_rss_feed_file :
  Stog_types.stog ->
  ?title:string -> Rss.url -> (Stog_types.doc_id * Stog_types.doc) list -> string -> unit
*)

(** Build the base rules, using the default ones and the base rules register
  by plugins. *)
val build_base_rules : Stog_types.stog ->
  Stog_types.doc_id -> (Xmlm.name * Stog_types.stog Xtmpl.callback) list

(** The calllback to insert a list of documents. Can be called directly
  if provided an additional environment, argument and children nodes. *)
val doc_list :
  Stog_types.doc ->
  ?rss:Neturl.url ->
  ?set:Stog_types.Doc_set.t -> Stog_types.stog Xtmpl.callback

val get_sectionning_tags : Stog_types.stog -> Stog_types.doc -> Xtmpl.name list

(** [mk_levels modname funs default_levels] returns a function
  to create the level map from an optional list of pairs [(funname, [level1 ; level2; ...])].
  [funs] is a list of pairs [(funname, Stog_engine.level_fun)] and [default_levels]
  is the default list of associations between funnames and levels.*)
val mk_levels : string -> (string * 'a Stog_engine.level_fun) list ->
  int list Stog_types.Str_map.t ->
    ?levels:(string * int list) list -> unit ->
    'a Stog_engine.level_fun Stog_types.Int_map.t

val make_module : ?levels:(string * int list) list -> unit -> (module Stog_engine.Module)

val module_name : string

val register_base_rule : Xtmpl.name -> Stog_types.stog Xtmpl.callback -> unit
