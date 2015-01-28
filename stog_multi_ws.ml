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


open Stog_types
open Stog_multi_config
open Stog_multi_session
open Stog_multi_gs

let (>>=) = Lwt.bind

let handle_con gs base_path uri (stream, push) =
  prerr_endline "new connection";
  let path = Stog_misc.split_string (Uri.path uri) ['/'] in
  match path with
  | "sessions" :: id :: p ->
      begin
        match Str_map.find id !(gs.sessions) with
        | exception Not_found ->
            failwith (Printf.sprintf "Invalid session %S" id)
        | session ->
            match p with
            | "editor" :: _ ->
                session.session_editor.editor_ws_cons#add_connection stream push
            | _ ->
                session.session_stog.stog_ws_cons := (stream, push) :: !(session.session_stog.stog_ws_cons) ;
                let read_stog () = Stog_multi_session.read_stog session.session_stog.stog_dir in
                Stog_server_ws.handle_messages read_stog
                  session.session_stog.stog_state session.session_stog.stog_ws_cons (base_path @ [id])
                  stream push
      end
  | _ -> failwith "Invalid path"

;;

let server cfg gs sockaddr =
  Websocket.establish_server sockaddr
    (handle_con gs (Neturl.url_path cfg.app_url))
;;

let run_server cfg gs =
  let host = Neturl.url_host cfg.app_url in
  let port = Neturl.url_port cfg.app_url + 1 in
  prerr_endline ("Setting up websocket server on host="^host^", port="^(string_of_int port));
  Lwt_io_ext.sockaddr_of_dns host (string_of_int port) >>= fun sa ->
    Lwt.return (server cfg gs sa)
;;