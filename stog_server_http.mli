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

(** HTTP connexion handler for preview server *)

(** [preview_url http_cfg path] forges the preview URL using the
  pub part of [http_cfg]. *)
val preview_url : Stog_url.url_config -> string list -> Stog_url.t

(** [handler current_state host port base_path req body] handles
  a HTTP query [req]. [current_state] is a reference to current stog state.
  [host], [port] and [base_path] are used to forge urls to refer
  to the service. *)
val handler :
  Stog_server_run.state option ref ->
  http_url: Stog_url.url_config ->
  ws_url: Stog_url.url_config ->
  string list ->
  Cohttp.Request.t ->
  (Stog_server_preview.S.Response.t * Cohttp_lwt_body.t) Lwt.t
