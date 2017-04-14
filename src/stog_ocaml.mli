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

(** Functions to eval and display ocaml in generated pages. *)

val stog_ocaml_session : string ref

val close_sessions : unit -> unit

val eval_ocaml_phrase : ?session_name:string -> ?directory:string ->
  string -> Stog_ocaml_types.result

val fun_eval : Stog_types.stog Xtmpl_rewrite.callback
val fun_printf : Stog_types.stog Xtmpl_rewrite.callback

val concat_nl : Xtmpl_rewrite.tree -> Xtmpl_rewrite.tree list -> Xtmpl_rewrite.tree list
val list_concat_nl : Xtmpl_rewrite.tree list -> Xtmpl_rewrite.tree list -> Xtmpl_rewrite.tree list
