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

open Ojs_server

type git_repo = {
  repo_dir : Ojs_path.t ;
  origin_url : string ;
  origin_branch : string ;
  edit_branch : string ;
  } [@@deriving yojson]

let git_repo ~origin_url ~origin_branch ~edit_branch ~dir =
  { repo_dir = dir ; origin_url ; origin_branch ; edit_branch }

module Make (P: Stog_git_types.P) =
  struct
    class repo
      (broadcall : P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t)
        (broadcast : P.server_msg -> unit Lwt.t) ~id root =
    object(self)
      method id = (id : string)
      method root = (root : Ojs_path.t)

      method handle_get_status reply_msg =
        reply_msg (P.SStatus [])

      method handle_commit reply_msg paths msg =
        reply_msg (P.SError "Commit not handled yet")

      method handle_message
            (send_msg : P.server_msg -> unit Lwt.t) (msg : P.client_msg) =
        self#handle_call send_msg msg

      method handle_call
            (reply_msg : P.server_msg -> unit Lwt.t) (msg : P.client_msg) =
        match msg with
        | P.Status ->
            self#handle_get_status reply_msg
        | P.Commit (paths, msg) ->
            self#handle_commit reply_msg paths msg
        | _ ->
            reply_msg (P.SError "Unhandled message")
      end

class repos
  (broadcall : P.app_server_msg -> (P.app_client_msg -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : P.app_server_msg -> unit Lwt.t)
    (spawn : (P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
     (P.server_msg -> unit Lwt.t) ->
       id: string -> Ojs_path.t -> repo
    )
    =
    object(self)
      val mutable repos = (SMap.empty : repo SMap.t)

      method repo id =
        try SMap.find id repos
        with Not_found -> failwith (Printf.sprintf "No repository with id %S" id)

      method add_repo ~id root =
        let broadcall msg cb =
          let cb msg =
             match P.unpack_client_msg msg with
             | Some (_, msg) -> cb msg
             | None -> Lwt.return_unit
          in
          broadcall (P.pack_server_msg id msg) cb
        in
        let broadcast msg = broadcast (P.pack_server_msg id msg) in
        let repo = spawn broadcall broadcast ~id root in
        repos <- SMap.add id repo repos;
        repo

      method handle_message
        (send_msg : P.app_server_msg -> unit Lwt.t) (msg : P.app_client_msg) =
          match P.unpack_client_msg msg with
          | Some (id, msg) ->
              let send_msg msg = send_msg (P.pack_server_msg id msg) in
              (self#repo id)#handle_message send_msg msg
          | None -> Lwt.return_unit

      method handle_call
         (return : P.app_server_msg -> unit Lwt.t) (msg : P.app_client_msg) =
        match P.unpack_client_msg msg with
        | Some (id, msg) ->
            let reply_msg msg = return (P.pack_server_msg id msg) in
            (self#repo id)#handle_call reply_msg msg
        | None -> Lwt.return_unit
  end
end