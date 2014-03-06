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

(** Interface for plugins.

  Even if all modules are accessible from dynamically loaded code,
  this {!Stog_plug} module should remain compatible from one release to
  another.
*)

(** [plugin_config_file stog plugin_name] returns the configuration file
  for this plugin name, for consistency purpose. *)
val plugin_config_file : Stog_types.stog -> string -> string

val register_lang : Stog_intl.lang_abbrev -> Stog_intl.lang_data -> unit

(** [register_html_base_rule name f] registers a new function associated
     to [name] in the set of base rules of the "html" predefined module. *)
val register_html_base_rule : Xmlm.name -> Stog_types.stog Xtmpl.callback -> unit

(** [elt_by_href ?typ ?src_elt stog env href] returns the element, path and
  optional if matching the given href string, of the form [path[#id]].
  Return None if the element could not be found, of the id could not be found,
  and an error is issued.
  @param src_elt can be used to specify the source element, to improve
  the error message. *)
val elt_by_href : ?typ: string -> ?src_elt: Stog_types.elt ->
  Stog_types.stog -> 'a -> 'a Xtmpl.env -> string ->
    'a * (Stog_types.elt * string * string option) option

(** [mk_block_node ...] creates a [<block ...] with the given information.*)
val mk_block_node :
  id: string -> ?label: Xtmpl.tree list -> ?clas: string ->
    title: Xtmpl.tree list -> ?counter: string ->
    short_fmt: Xtmpl.tree list -> long_fmt: Xtmpl.tree list -> Xtmpl.tree list -> Xtmpl.tree

(** {2 Outputting message.} *)

val verbose : ?info:string -> ?level: int -> string -> unit
val set_print_verbose : (string -> unit) -> unit

val warning : ?info:string -> string -> unit
val set_print_warning : (string -> unit) -> unit

val error : ?info:string -> ?fatal: int -> string -> unit
val set_print_error : (string -> unit) -> unit

(** {2 Dependencies} *)

type dependency = Stog_types.elt Stog_types.dependency

(** For a given element, add a dependency on a file or another element. *)
val add_dep : Stog_types.stog -> Stog_types.elt -> dependency -> Stog_types.stog
