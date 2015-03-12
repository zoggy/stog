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

(** Stog preview *)

module S = Cohttp_lwt_unix.Server

(** The javascript file containing client code for preview *)
val client_js : string

(** The default server CSS file in files embedded in server *)
val default_css : string

(** Respond the given string with CSS mime-type. *)
val respond_css : string -> (S.Response.t * Cohttp_lwt_body.t) Lwt.t

(** Respond the content of the given javascript file, looked up in
  server embedded files. *)
val respond_server_js : string -> (S.Response.t * Cohttp_lwt_body.t) Lwt.t

(** Respond the default server CSS file *)
val respond_default_css : unit -> (S.Response.t * Cohttp_lwt_body.t) Lwt.t

(** [handle_preview http_url ws_url current_state req path] responds the
  preview page, which contains a reference to client javascript code
  which, when loaded, will ask for stog document with path [path]. *)
val handle_preview :
  http_url: Stog_types.url_config ->
  ws_url: Stog_types.url_config ->
  Stog_server_run.state option ref ->
  Cohttp.Request.t ->
  string list -> (S.Response.t * Cohttp_lwt_body.t) Lwt.t

(** [new_stog_session stog stog_base_url] returns a fresh stog state reference
  and a fresh connection list reference. *)
val new_stog_session :
  Stog_types.stog ->
  Neturl.url ->
  Stog_server_run.state option ref *
  ('a * (Websocket.Frame.t option -> 'b)) list ref
