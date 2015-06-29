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

(** Multi server editor. *)

module S = Cohttp_lwt_unix.Server
module J = Yojson.Safe

module Server : Ojs_server.S
 with type Rpc.app_server_msg = Stog_multi_ed_types.App_msg.app_server_msg
  and type Rpc.app_client_msg = Stog_multi_ed_types.App_msg.app_client_msg

val init :
  ?sshkey:string ->
  stog_dir:Ojs_path.t ->
  git:Stog_git_server.git_repo -> Server.connection_group

val http_handler :
  Stog_multi_config.t ->
  Stog_multi_config.account ->
  http_url: Stog_url.url_config ->
  ws_url: Stog_url.url_config ->
  string list ->
  string ->
  'a ->
  'b ->
  string list -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
