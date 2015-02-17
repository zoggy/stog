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

(** Browser client code for git operations on server. *)

class type editor = object method changed_files : Ojs_path.t list end

module Make :
  functor (P : Stog_git_types.P) ->
    sig
      val display_status_box :
        (P.client_msg -> 'a) ->
        string ->
        (Stog_git_types.status * Stog_git_types.status *
         Stog_git_types.path * Ojs_path.t option)
        list -> unit Lwt.t
      class repo :
        (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.client_msg -> unit Lwt.t) ->
        msg_id:string ->
        < changed_files : Ojs_path.t list; .. > ->
        string ->
        object
          method display_error : string -> unit
          method display_message : string -> unit
          method handle_message : P.server_msg -> bool Js.t
          method id : string
          method msg_id : string
          method pull : unit Lwt.t
          method push : unit Lwt.t
          method simple_call : P.client_msg -> unit Lwt.t
          method status_commit : unit Lwt.t
        end
      class repos :
        (P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_client_msg -> unit Lwt.t) ->
        ((P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) ->
         msg_id:string -> editor -> string -> repo) ->
        object
          method get_msg_id : Ojs_js.SMap.key -> string
          method get_repo : Ojs_js.SMap.key -> repo
          method handle_message : P.app_server_msg -> bool Js.t
          method setup_repo :
            msg_id:string -> editor -> Ojs_js.SMap.key -> unit
        end
    end
