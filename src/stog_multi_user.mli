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

(** User page in multi server interface *)

module H = Xtmpl_xhtml

val page :
  Stog_multi_config.t ->
  Stog_multi_gs.global_state ->
  ?message:Stog_multi_page.block ->
  ?error:Stog_multi_page.block ->
  Stog_multi_config.account -> Xtmpl.tree list

val handle_sessions_post :
  Stog_multi_config.t ->
  Stog_multi_gs.global_state ->
  Stog_multi_config.account -> 'a -> 'b -> Xtmpl.tree list Lwt.t

val handle_sessions_get :
  Stog_multi_config.t ->
  Stog_multi_gs.global_state ->
  Stog_multi_config.account -> 'a -> 'b -> Xtmpl.tree list Lwt.t
