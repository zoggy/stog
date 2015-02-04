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


open Ojs_js
let (>>=) = Lwt.(>>=)

module Make(P:Stog_git_types.P) =
  struct
    class repo call (send : P.client_msg -> unit Lwt.t)
      ~msg_id repo_id =
    object(self)
      method id : string = repo_id
      method msg_id = msg_id

      method display_error msg = Ojsmsg_js.display_text_error msg_id msg
      method display_message msg = Ojsmsg_js.display_text_message msg_id msg

      method simple_call : 'clt -> unit Lwt.t = fun msg ->
        call msg
          (fun msg -> Lwt.return
             (match msg with
              | P.SError msg -> self#display_error msg
              | P.SOk msg -> self#display_message msg
              | _ -> ()
             )
          )

      method commit =
        let paths, msg = ([], "Commit") in
        self#simple_call (P.Commit (paths, msg))

      method handle_message (msg : 'srv) =
        try
          (match msg with
           | P.SStatus l -> ()
           | P.SOk msg -> self#display_message msg
           | P.SError msg -> self#display_error msg
           | _ -> failwith "Unhandled message received from server"
          );
          Js._false
        with
          e ->
            log (Printexc.to_string e);
            Js._false

      initializer
        (*Ojs_js.set_onclick button (fun _ -> self#save);*)
        ()
    end

    class repos
      (call : P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t)
        (send : P.app_client_msg -> unit Lwt.t)
        (spawn : (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) ->
           msg_id: string -> string -> repo) =
        object(self)
          val mutable repos = (SMap.empty : repo SMap.t)

          method get_repo id =
            try SMap.find id repos
            with Not_found -> failwith (Printf.sprintf "Invalid repository id %S" id)

          method get_msg_id id = (self#get_repo id)#msg_id

          method handle_message (msg : P.app_server_msg) =
            match P.unpack_server_msg msg with
            | Some (id, msg) -> (self#get_repo id)#handle_message msg
            | None -> Js._false

          method setup_repo ~msg_id repo_id =
            let send msg = send (P.pack_client_msg repo_id msg) in
            let call msg cb =
              let cb msg =
                match P.unpack_server_msg msg with
                | Some (_, msg) -> cb msg
                | None -> Lwt.return_unit
              in
              call (P.pack_client_msg repo_id msg) cb
            in
            let repo = spawn call send ~msg_id repo_id in
            repos <- SMap.add repo_id repo repos
        end
end