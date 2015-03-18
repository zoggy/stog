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

(** Handling multi server sessions *)

type session_state = Live | Stopped | Error of string [@@deriving yojson]

type stored = {
  session_create_date : float;
  mutable session_state : session_state;
  session_author : string;
  session_git : Stog_git_server.git_repo;
} [@@deriving yojson]

type stog_info = {
  stog_dir : string;
  mutable stog_state : Stog_server_run.state option ref;
  mutable stog_ws_cons :
    (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit))
    list ref;
  stog_preview_url : Stog_url.t ;
}

type editor_info = {
  mutable editor_ws_cons : Stog_multi_ed.Server.connection_group;
  editor_url : Stog_url.t ;
}

type session = {
  session_id : string;
  session_dir : string;
  session_stored : stored;
  session_stog : stog_info;
  session_editor : editor_info;
}

val new_id : unit -> string

(** Read stog structure from the given directory. *)
val read_stog : string -> Stog_types.stog

val mk_edit_branch_name : string -> string

val store_stored : session -> unit

val start_session : ?sshkey:string -> session -> unit

val create :
  Stog_multi_config.t -> Stog_multi_config.account -> session Lwt.t

val load_previous_sessions : Stog_multi_config.t -> session list
