(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
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
module S = Cohttp_lwt_unix.Server
let (>>=) = Lwt.bind

type space =
  { space_state : Stog_server_run.state option ref ;
    space_ws_cons : (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit)) list ref ;
    space_auth : string ;
  }

let handler spaces host port sock req body =
  let uri = Cohttp.Request.uri req in
  let path = Stog_misc.split_string (Uri.path uri) ['/'] in
  match path with
  | id :: _ ->
      begin
        match Str_map.find id !spaces with
          space ->
            Stog_server_http.handler space.space_state host port [id] sock req body
        | exception Not_found ->
            let body =
              "<html><header><title>Stog-server</title></header>"^
                "<body>Space "^id^" not found</body></html>"
            in
            S.respond_error ~status:`Not_found ~body ()
      end
  | _ ->
      let body = Printf.sprintf "<html><header><title>Stog-server</title></header>
    <body>Hello world !</body></html>"
      in
      S.respond_string ~status:`OK ~body ()

let start_server spaces host port =
  Lwt_io.write Lwt_io.stdout
    (Printf.sprintf "Listening for HTTP request on: %s:%d\n" host port)
  >>= fun _ ->
  let conn_closed id () =
    ignore(Lwt_io.write Lwt_io.stdout
      (Printf.sprintf "connection %s closed\n%!" (Cohttp.Connection.to_string id)))
  in
  let config = { S.callback = handler spaces host port ; conn_closed } in
  S.create ~address:host ~port config

let launch host port =
  let spaces = ref (Str_map.empty : space Str_map.t) in
  start_server spaces host port

let () =
  let run host port = Lwt_unix.run (launch host port) in
  Stog_server_mode.set_multi run

