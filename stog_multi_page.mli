(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 INRIA All rights reserved.                         *)
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

(** *)

val url_ : Stog_multi_config.t -> string list -> string
val path_login : string list
val path_sessions : string list
val url_login : Stog_multi_config.t -> string
val url_sessions : Stog_multi_config.t -> string

type block = [`Msg of string | `Block of Xtmpl.tree list]

val page :
  Stog_multi_config.t ->
    Stog_multi_config.account option ->
    ?empty: bool ->
    title:string ->
    ?error: block ->
    ?message: block ->
    ?js: string list ->
    Xtmpl.tree list -> Xtmpl.tree list

module Form_login :
  sig
    type form =
        ?env:unit Xtmpl.env ->
        ?submit:string ->
        ?password:string ->
        ?meth:Cohttp.Code.meth ->
        ?login:string ->
        ?error_msg:Xtmpl.tree list ->
        ?action:string -> unit -> Xtmpl.tree list
    type template =
        ?env:unit Xtmpl.env ->
        ?meth:Cohttp.Code.meth ->
        ?error_msg:Xtmpl.tree list ->
        ?action:string -> unit -> Xtmpl.tree list
    exception Error of (template * string list)
    type t = { submit : string option; password : string; login : string; }
    val form :
      ?env:unit Xtmpl.env ->
      ?submit:string ->
      ?password:string ->
      ?meth:Cohttp.Code.meth ->
      ?login:string ->
      ?error_msg:Xtmpl.tree list -> ?action:string -> unit -> Xtmpl.tree list
    val read_form : (string -> string option) -> template * t
  end

val param_of_body : string -> string -> string option
