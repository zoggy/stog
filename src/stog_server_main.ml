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

open Stog_server_run
open Stog_url
open Stog_types

module S = Cohttp_lwt_unix.Server
let (>>=) = Lwt.bind

let new_stog_session stog ~http_url base_path =
  let stog_base_url = Stog_server_http.preview_url http_url base_path in
  Stog_server_preview.new_stog_session stog stog_base_url

let start_server current_state ~http_url ~ws_url base_path =
  let host = Stog_url.host http_url.priv in
  let port = Stog_url.port http_url.priv in
  Lwt_io.write Lwt_io.stdout
    (Printf.sprintf "Listening for HTTP request on: %s:%d\n" host port)
  >>= fun _ ->
  let conn_closed (_,id) =
    ignore(Lwt_io.write Lwt_io.stdout
      (Printf.sprintf "connection %s closed\n%!" (Cohttp.Connection.to_string id)))
  in
  let config = S.make
    ~callback: (fun sock req body ->
       Stog_server_http.handler current_state ~http_url ~ws_url base_path req)
    ~conn_closed
    ()
  in
  Conduit_lwt_unix.init ~src:host () >>=
  fun ctx ->
      let ctx = Cohttp_lwt_unix_net.init ~ctx () in
      let mode = `TCP (`Port port) in
      S.create ~ctx ~mode config


let launch read_stog stog ~http_url ~ws_url base_path =
  let read_stog () =
    let stog = read_stog () in
    let stog = { stog with Stog_types.stog_outdir = "stog-output" } in
    stog
  in
  let (current_state, active_cons) = new_stog_session stog ~http_url base_path in
  let _ws_server =
    Stog_server_ws.run_server read_stog current_state active_cons ws_url base_path
  in
  start_server current_state ~http_url ~ws_url base_path

let () =
  let run read_stog stog ~http_url ~ws_url =
    Lwt_main.run (launch read_stog stog ~http_url ~ws_url [])
  in
  Stog_server_mode.set_single run

