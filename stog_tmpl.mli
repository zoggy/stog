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

(** Default templates. *)

type contents = Stog_types.stog -> Stog_types.stog * Xtmpl.tree

(** [get_template_file stog elt file] returns absolute filename of the given
  template filename.
  If [file] is relative and implicit then it is concatenated to
  the stog template directory.
  If [file] is relative and not implicit (i.e. it starts with . or ..), it
  is concatenated to the element source file directory.
  Else (the filename is absolute), it is returned as is.
*)
val get_template_file : Stog_types.stog -> Stog_types.elt -> string -> string

(** [read_template_file stog elt file] returns the content of the given
  template filename, after calling {!get_template_file} to get the final
  filename.
  @param raw indicate whether to read the template as XML ([false]) or
  as CData ([true]). Default is [false].
  @param depend indicate whether to add a dependency from the element
  on the file. Default is [true].
  *)
val read_template_file : Stog_types.stog -> Stog_types.elt ->
  ?depend: bool -> ?raw: bool -> string -> Stog_types.stog * Xtmpl.tree

val get_template : Stog_types.stog -> ?elt: Stog_types.elt ->
  contents -> string -> Stog_types.stog * Xtmpl.tree

val page : contents
val by_keyword : contents
val by_topic : contents
val by_month : contents
val elt_in_list : contents
val keyword : contents
val topic : contents
