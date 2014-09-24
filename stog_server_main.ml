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

open Stog_server_run

module S = Cohttp_lwt_unix.Server
let (>>=) = Lwt.bind

let http_url host port = Printf.sprintf "http://%s:%d/preview" host port
let ws_url host port = Printf.sprintf "ws://%s:%d/" host port

let handler host port sock req body =
  let uri = Cohttp.Request.uri req in
  let path = Stog_misc.split_string (Uri.path uri) ['/'] in
  match path with
    "preview" :: path ->
      let http_url = http_url host port in
      let ws_url = ws_url host (port+1) in
      Stog_server_preview.handle_preview http_url ws_url sock req body path
  | "editor" :: path ->
      let http_url = http_url host port in
      let ws_url = ws_url host (port+1) in
      Stog_server_editor.handle http_url ws_url sock req body path
  | ["status"] ->
     Stog_server_run.state () >>= fun state ->
        let b = Buffer.create 256 in
        let p = Buffer.add_string b in
        p "<html><header><meta charset=\"utf-8\"><title>Stog-server : Status</title></header>";
        p "<body>";
        p "<h1>Status</h1>" ;
        p "<h2>Errors</h2><ul>";
        List.iter (fun s -> p ("<li>"^s^"</li>")) state.stog_errors ;
        p "</ul>";
        p "<h2>Warnings</h2><ul>";
        List.iter (fun s -> p ("<li>"^s^"</li>")) state.stog_warnings ;
        p "</ul>";
        p "</body></html>";
        let body = Buffer.contents b in
        S.respond_string ~status: `OK ~body ()
  | _ ->
      let body = Printf.sprintf "<html><header><title>Stog-server</title></header>
    <body>Hello world !</body></html>"
      in
      S.respond_string ~status:`OK ~body ()

let start_server host port =
  Lwt_io.write Lwt_io.stdout
    (Printf.sprintf "Listening for HTTP request on: %s:%d\n" host port)
  >>= fun _ ->
  let conn_closed id () =
    ignore(Lwt_io.write Lwt_io.stdout
      (Printf.sprintf "connection %s closed\n%!" (Cohttp.Connection.to_string id)))
  in
  let config = { S.callback = handler host port; conn_closed } in
  S.create ~address:host ~port config


let launch stog host port =
  let stog =
    let stog_base_url =
      Stog_types.url_of_string (Printf.sprintf "http://%s:%d/preview" host port)
    in
    { stog with Stog_types.stog_base_url }
  in
  let on_update = Stog_server_ws.send_patch in
  let on_error = Stog_server_ws.send_errors in
  let _watcher = Stog_server_run.watch stog ~on_update ~on_error in
  Stog_server_ws.run_server host (port+1) >>=
    fun _ -> start_server host port

let () =
  let run stog host port = Lwt_unix.run (launch stog host port) in
  Stog_server_mode.launch := Some run

