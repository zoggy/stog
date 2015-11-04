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

(** Building a stog structure from a project tree. *)

(** Parse a date. @raise Failure if the date format is incorrect.
  See Netdate module documentation for details about accepted formats.
*)
val date_of_string : string -> Netdate.t

(** [bool_of_string "0" = false] and [bool_of_string "false" = false].
  [bool_of_string] returns [true] for any other value. *)
val bool_of_string : string -> bool

(** [read_modules stog] read all modules found in directories of the
  list [stog.stog_mod_dirs] and return the modified stog structure. *)
val read_modules : Stog_types.stog -> Stog_types.stog

(** Read an document from the given absolute filename. The file
     must be "under" the directory in [stog_dir] of the [stog] parameter. *)
val doc_of_file : Stog_types.stog -> string -> Stog_types.doc

(** Build a {!Stog_types.stog} structure from the given directory. *)
val read_stog : string -> Stog_types.stog

(** *)
val fill_doc_from_atts_and_subs : Stog_types.doc ->
  Xtmpl_rewrite.attributes -> Stog_types.body -> Stog_types.doc