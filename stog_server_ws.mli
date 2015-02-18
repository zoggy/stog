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

(** Handling websocket connections in preview server *)

(** A never-ending function *)
val wait_forever : unit -> 'a Lwt.t

(** Send errors and warnings to a list of client connections. *)
val send_errors :
  ('a * (Websocket.Frame.t option -> 'b)) list ref ->
  errors:string list -> warnings:string list -> unit Lwt.t

(** [send_patchs  active_cons old_stog stog doc_id] sends, to
  the current list of client connections, a patch from the
  differences in the document [doc_id] between [old_stog] and [stog].*)
val send_patch :
  ('a * (Websocket.Frame.t option -> 'b)) list ref ->
  Stog_types.stog ->
  Stog_types.stog -> Stog_types.doc Stog_tmap.key -> unit Lwt.t

(** [handle_messages read_stog current_state active_cons base_path stream push]
  handle client messages on websocket [(stream, push)]. *)
val handle_messages :
  (unit -> Stog_types.stog) ->
  Stog_server_run.state option ref ->
  ('a * (Websocket.Frame.t option -> unit)) list ref ->
  string list ->
  Websocket.Frame.t Lwt_stream.t ->
  (Websocket.Frame.t option -> unit) -> unit Lwt.t

(** [sockaddr_of_dns host port] create an address from the given host and service name
  or port number. *)
val sockaddr_of_dns : string -> string -> Lwt_unix.sockaddr Lwt.t

(** [read_stog current_state active_cons host port base_path] creates a server
  for previewing the documents in the current state. *)
val run_server :
  (unit -> Stog_types.stog) ->
  Stog_server_run.state option ref ->
  (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit)) list ref ->
    string -> int -> string list -> Websocket.server Lwt.t

