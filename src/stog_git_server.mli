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

(** Git operations on server side. *)

type git_repo = {
  repo_dir : Ojs_path.t;
  origin_url : string;
  origin_branch : string;
  edit_branch : string;
} [@@deriving yojson]

val git_repo :
  origin_url:string ->
  origin_branch:string -> edit_branch:string -> dir:Ojs_path.t -> git_repo

val clone : ?sshkey:string -> git_repo -> unit Lwt.t
val set_user_info : git_repo -> name:string -> email:string -> unit
val current_branch : git_repo -> string
val create_edit_branch : git_repo -> unit

module Make :
  functor (P : Stog_git_types.P) ->
    sig
      class repo :
        (P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.server_msg -> unit Lwt.t) ->
        id:string ->
        ?sshkey:string ->
        git_repo ->
        object
          val git_mutex : Lwt_mutex.t
          method do_git : git_repo -> (git_repo -> 'a Lwt.t) -> 'a Lwt.t
          method git : git_repo
          method git_action :
            (P.server_msg -> unit Lwt.t) -> (unit -> string) -> unit Lwt.t
          method handle_call :
            (P.server_msg -> unit Lwt.t) -> P.client_msg -> unit Lwt.t
          method handle_commit :
            (P.server_msg -> unit Lwt.t) ->
            Stog_git_types.path list -> string -> unit Lwt.t
          method handle_get_status :
            (P.server_msg -> unit Lwt.t) -> unit Lwt.t
          method handle_message :
            (P.server_msg -> unit Lwt.t) -> P.client_msg -> unit Lwt.t
          method handle_push : (P.server_msg -> unit Lwt.t) -> unit Lwt.t
          method handle_rebase_from_origin :
            (P.server_msg -> unit Lwt.t) -> unit Lwt.t
          method id : string
          method sshkey : string option
        end
      class repos :
        (P.app_server_msg -> (P.app_client_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_server_msg -> unit Lwt.t) ->
        ((P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.server_msg -> unit Lwt.t) ->
         id:string -> ?sshkey:string -> git_repo -> repo) ->
        object
          val mutable repos : repo Ojs_server.SMap.t
          method add_repo :
            id:Ojs_server.SMap.key -> ?sshkey:string -> git_repo -> repo
          method handle_call :
            (P.app_server_msg -> unit Lwt.t) ->
            P.app_client_msg -> unit Lwt.t
          method handle_message :
            (P.app_server_msg -> unit Lwt.t) ->
            P.app_client_msg -> unit Lwt.t
          method repo : Ojs_server.SMap.key -> repo
        end
    end
