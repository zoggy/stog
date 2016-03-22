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

(** Default templates. *)

type contents = Stog_types.stog -> Stog_types.stog * Xtmpl_rewrite.tree list

(** [get_template_file stog doc file] returns absolute filename of the given
  template filename.
  If [file] is relative and implicit then the file is looked up the
  stog include directories.
  If [file] is relative and not implicit (i.e. it starts with . or ..), it
  is concatenated to the document source file directory.
  Else (the filename is absolute), it is returned as is.
  @raise Template_file_not_found if no include directory contains the
  given file (when the file is relative an implicit).
*)
val get_template_file : Stog_types.stog -> Stog_types.doc ->
  ?loc: Xtmpl_xml.loc -> string -> string

(** [read_template_file stog doc file] returns the content of the given
  template filename, after calling {!get_template_file} to get the final
  filename.
  @param raw indicate whether to read the template as XML ([false]) or
  as CData ([true]). Default is [false].
  @param depend indicate whether to add a dependency from the document
  on the file. Default is [true].
  @raise Template_file_not_found (see {!get_template_file})
  *)
val read_template_file : Stog_types.stog -> Stog_types.doc ->
  ?depend: bool -> ?raw: bool ->
  ?loc: Xtmpl_xml.loc -> string -> Stog_types.stog * Xtmpl_rewrite.tree list

val get_template : Stog_types.stog -> ?doc: Stog_types.doc ->
  contents -> string -> Stog_types.stog * Xtmpl_rewrite.tree list

val get_template_doc : Stog_types.stog -> ?doc: Stog_types.doc ->
  contents -> string -> Stog_types.stog * Xtmpl_rewrite.tree Xtmpl_xml.doc

val page : contents
val by_keyword : contents
val by_topic : contents
val by_month : contents
val doc_in_list : contents
val keyword : contents
val topic : contents
val rss : contents
val rss_item : contents
val doc_list : contents