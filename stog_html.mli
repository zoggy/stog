(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
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

(** Html generation.

  While the only function called from "main" is {!val:generate},
  some values are made available for the {!Stog_plug} module.
*)

(** [elt_by_href ?typ stog env href] returns the element, hid and
  optional id matching the given href string, of the form [hid[#id]].
  Return None if the element could not be found, of the id could not be found,
  and an error is issued. *)
val elt_by_href : ?typ: string -> Stog_types.stog -> 'a -> 'a Xtmpl.env -> string ->
  'a * (Stog_types.elt * string * string option) option

(*
(** Adding a known block id for a given hid. A short and a long title
  are specified. These registered blocks are used by <elt href="..#id"/> nodes.
  @on_dup specifies what to do when the id to add is already present.
  Default is to issue a warning. [`Fail] will raise a [Failure] exception.
*)
val add_block :
  ?on_dup: [`Ignore | `Fail | `Warn] ->
  hid: string -> id: string -> short: Xtmpl.tree -> long: Xtmpl.tree -> unit -> unit
*)

(** [get_in_env env tag] will look for the given string in the environment,
  by building a <tag/> node and evaluating it. If the result is the same node,
  then return [""] else return the reduced value as a string.*)
val get_in_env : 'a -> 'a Xtmpl.env -> Xmlm.name -> 'a * string

(** [get_in_args_or_env env args s] returns the value associated to [s]
 in [args] of else return the result of [get_in_env env s]. *)
val get_in_args_or_env : 'a -> 'a Xtmpl.env -> Xmlm.attribute list -> Xmlm.name -> 'a * string

(** [get_hid env] returns the value associated to ["hid"] in [env].
  @raise Assert_failure if ["hid"] is not found in the environment.*)
val get_hid : 'a -> 'a Xtmpl.env -> 'a * string

(** Escape html code in the given string: change [&] to [&amp;],
  [<] to [&lt;] and [>] to [&gt;].*)
val escape_html : string -> string

val concat_name : ?sep: string -> (string * string) -> string

(** Call the highlight command on the given string and make it produce xhtml code.
  Options are passed to the highlight command. *)
val highlight : opts:string -> string -> string

(** Build an url from the given hid, using the given optional extension.
  This is used for elements created on the fly, like by-word or by-month index. *)
val url_of_hid :
  Stog_types.stog -> ?ext:string -> Stog_types.human_id -> Neturl.url

val rss_date_of_date : Stog_types.date -> Rss.date

(*
(** Generate a RSS file from the given list of elements. The final RSS
  url must be given as it is embedded in the RSS file. *)
val generate_rss_feed_file :
  Stog_types.stog ->
  ?title:string -> Rss.url -> (Stog_types.elt_id * Stog_types.elt) list -> string -> unit
*)

(** Build the base rules, using the default ones and the {!plugin_rules}. *)
val build_base_rules : Stog_types.stog ->
  Stog_types.elt_id -> (Xmlm.name * Stog_types.stog Xtmpl.callback) list

(** The calllback to insert a list of elements. Can be called directly
  if provided an additional environment, argument and children nodes. *)
val elt_list :
  Stog_types.elt ->
  ?rss:Neturl.url ->
  ?set:Stog_types.Elt_set.t -> Stog_types.stog Xtmpl.callback

val get_sectionning_tags : Stog_types.stog -> Stog_types.elt -> Xtmpl.name list


(** [mk_levels modname funs default_levels] returns a function
  to create the level map from an optional list of pairs [(funname, [level1 ; level2; ...])].
  [funs] is a list of pairs [(funname, Stog_engine.level_fun)] and [default_levels]
  is the default list of associations between funnames and levels.*)
val mk_levels : string -> (string * 'a Stog_engine.level_fun) list ->
  int list Stog_types.Str_map.t ->
    ?levels:(string * int list) list -> unit ->
    'a Stog_engine.level_fun Stog_types.Int_map.t

val make_module : ?levels:(string * int list) list -> unit -> (module Stog_engine.Stog_engine)
