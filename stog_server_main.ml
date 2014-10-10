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

let new_stog_session stog host port base_path =
  let stog =
    let stog_base_url =
      let s = Printf.sprintf "http://%s:%d/%spreview"
        host port (Stog_server_http.base_path_string base_path)
      in
      Stog_types.url_of_string s
    in
    { stog with Stog_types.stog_base_url }
  in
  let current_state = ref None in
  let active_cons = ref [] in
  let on_update = Stog_server_ws.send_patch active_cons in
  let on_error = Stog_server_ws.send_errors active_cons in
  let _watcher = Stog_server_run.watch stog current_state ~on_update ~on_error in
  (current_state, active_cons)

let start_server current_state host port base_path =
  Lwt_io.write Lwt_io.stdout
    (Printf.sprintf "Listening for HTTP request on: %s:%d\n" host port)
  >>= fun _ ->
  let conn_closed id () =
    ignore(Lwt_io.write Lwt_io.stdout
      (Printf.sprintf "connection %s closed\n%!" (Cohttp.Connection.to_string id)))
  in
  let config =
    { S.callback = Stog_server_http.handler current_state host port base_path;
      conn_closed ;
    }
  in
  S.create ~address:host ~port config


let launch stog host port base_path =
  let (current_state, active_cons) = new_stog_session stog host port base_path in
  Stog_server_ws.run_server current_state active_cons host (port+1) base_path >>=
    fun _ -> start_server current_state host port base_path

let () =
  let run stog host port = Lwt_unix.run (launch stog host port []) in
  Stog_server_mode.set_single run

