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

(** *)


open Stog_url
open Stog_multi_config
open Stog_multi_session
open Stog_multi_gs

let (>>=) = Lwt.bind

let handle_con gs base_path _id req recv push =
  prerr_endline "new connection";
  let stream = Websocket_lwt.mk_frame_stream recv in
  let uri = Cohttp.Request.uri req in
  let path = Stog_misc.split_string (Uri.path uri) ['/'] in
  match path with
  | "sessions" :: id :: p ->
      begin
        match Stog_types.Str_map.find id !(gs.sessions) with
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
  Websocket_lwt.establish_standard_server

;;

let run_server cfg gs =
  let host = Stog_url.host cfg.ws_url.priv in
  let port = Stog_url.port cfg.ws_url.priv in
  prerr_endline ("Setting up websocket server on host="^host^", port="^(string_of_int port));  
  (* set scheme to http to be resolved correctly *)
  let uri =
    let u = Uri.of_string (Stog_url.to_string cfg.ws_url.priv) in
    Uri.with_scheme u (Some "http")
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let ctx = Conduit_lwt_unix.default_ctx in
  Nocrypto_entropy_lwt.initialize () >>
  Conduit_lwt_unix.endp_to_server ~ctx endp >>= fun server ->
  let handler = handle_con gs (Stog_url.path cfg.ws_url.pub) in
  Websocket_lwt.establish_standard_server ~ctx ~mode: server 
    ~g: !Nocrypto.Rng.generator handler
;;