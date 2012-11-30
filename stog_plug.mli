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

(** Interface for plugins.

  Even if all modules are accessible from dynamically loaded code,
  this {!Stog_plug} module should remain compatible from one release to
  another.
*)

val register_lang : Stog_intl.lang_abbrev -> Stog_intl.lang_data -> unit
val register_rule : string -> Xtmpl.callback -> unit

(** Unregister the rule with the given name from the plugin rules.
  It only register the last registered rule with this name.
  If a rule is unregistered, it is returned, else [None] is returned.*)
val unregister_rule : string -> Xtmpl.callback option

val stog : unit -> Stog_types.stog

(** [elt_by_href ?typ stog env href] returns the element, hid and
  optional if matching the given href string, of the form [hid[#id]].
  Return None if the element could not be found, of the id could not be found,
  and an error is issued. *)
val elt_by_href : ?typ: string -> Stog_types.stog -> Xtmpl.env -> string ->
  (Stog_types.elt * string * string option) option

(** Adding a known block id for a given hid. A short and a long title
  are specified. These registered blocks are used by <elt href="..#id"/> nodes.
  @on_dup specifies what to do when the id to add is already present.
  Default is to issue a warning. [`Fail] will raise a [Failure] exception.
*)
val add_block :
  ?on_dup: [`Ignore | `Fail | `Warn] ->
  hid: string -> id: string -> short: Xtmpl.tree -> long: Xtmpl.tree -> unit -> unit

val verbose : ?info:string -> ?level: int -> string -> unit
val set_print_verbose : (string -> unit) -> unit

val warning : ?info:string -> string -> unit
val set_print_warning : (string -> unit) -> unit

val error : ?info:string -> ?fatal: int -> string -> unit
val set_print_error : (string -> unit) -> unit

val register_stage0_fun : (Stog_types.stog -> Stog_types.stog) -> unit

type rule_build =
  Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> (string * Xtmpl.callback) list
type level_fun =
  Xtmpl.env -> Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> Stog_types.elt
;;

val register_level_fun : int -> level_fun -> unit
val compute_elt : rule_build -> level_fun
