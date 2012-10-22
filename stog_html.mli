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

(** Html generation.

  While the only function called from "main" is {!val:generate},
  some values are made available for the {!Stog_plug} module.
*)

(** Access to the current stog structure. It is made available
   by the {!val: generate} function. *)
val current_stog : Stog_types.stog option ref

(** The rewrite rules registered by plugins. *)
val plugin_rules : (string * Xtmpl.callback) list ref

(** Stage 0 functions registered by plugins. *)
val stage0_funs : (Stog_types.stog -> Stog_types.stog) list ref

(** Adding a known block id for a given hid. A short and a long title
  are specified. These registered blocks are used by <elt href="..#id"/> nodes. *)
val add_block :
  hid: string -> id: string -> short: Xtmpl.tree -> long: Xtmpl.tree -> unit

(** [get_in_env env tag] will look for the given string in the environment,
  by building a <tag/> node and evaluating it. If the result is the same node,
  then return [""] else return the reduced value as a string.*)
val get_in_env : Xtmpl.env -> string -> string

(** [get_in_args_or_env env args s] returns the value associated to [s]
 in [args] of else return the result of [get_in_env env s]. *)
val get_in_args_or_env : Xtmpl.env -> (string * string) list -> string -> string

(** [get_hid env] returns the value associated to ["hid"] in [env].
  @raise Assert_failure if ["hid"] is not found in the environment.*)
val get_hid : Xtmpl.env -> string

(** Escape html code in the given string: change [&] to [&amp;],
  [<] to [&lt;] and [>] to [&gt;].*)
val escape_html : string -> string

(** Call the highlight command on the given string and make it produce xhtml code.
  Options are passed to the highlight command. *)
val highlight : opts:string -> string -> string

(** Build the final file where the given element will be generated. *)
val elt_dst_file : Stog_types.stog -> Stog_types.elt -> string

(** Build the final url of the given element. *)
val elt_url : Stog_types.stog -> Stog_types.elt -> string

(** Build an url from the given hid, using the given optional extension.
  This is used for elements created on the fly, like by-word or by-month index. *)
val url_of_hid :
  Stog_types.stog -> ?ext:string -> Stog_types.human_id -> string

val rss_date_of_date : Stog_types.date -> Rss.date
val elt_to_rss_item : Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> Rss.item

(** Generate a RSS file from the given list of elements. The final RSS
  url must be given as it is embedded in the RSS file. *)
val generate_rss_feed_file :
  Stog_types.stog ->
  ?title:string -> Rss.url -> (Stog_types.elt_id * Stog_types.elt) list -> string -> unit

(** Build the base rules, using the default ones and the {!plugin_rules}. *)
val build_base_rules : Stog_types.stog ->
  Stog_types.elt_id -> Stog_types.elt -> (string * Xtmpl.callback) list

(** The calllback to insert a list of elements. Can be called directly
  if provided an additional environment, argument and children nodes. *)
val elt_list :
  ?rss:string ->
  ?set:Stog_types.Elt_set.t -> Stog_types.stog -> Xtmpl.callback

(** Generate the target files, with the following steps:
  - apply registered stage0 functions to the read stog structure.
  - create the output directory,
  - build the base environment from the site global attributes,
  - compute by-topic, by-keyword and by-month elements,
  - compute elements,
  - for each level, for each element, apply level functions on the element
  - output {!Stog_types.elt.elt_out} field in the destination file.
*)
val generate : ?only_elt:string -> Stog_types.stog -> unit

type rule_build =
  Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> (string * Xtmpl.callback) list
type level_fun =
  Xtmpl.env -> Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> Stog_types.elt
;;


val register_level_fun : int -> level_fun -> unit
val compute_elt : rule_build -> level_fun
