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
open Stog_types
open Stog_server_types
open Stog_server_run
module S = Cohttp_lwt_unix.Server
let (>>=) = Lwt.bind

let preview_url http_url base_path =
  let url = Stog_url.append http_url.pub base_path in
  Stog_url.append url ["preview"]

let path_after_base base path =
  let rec iter = function
  | [], p -> p
  | h1 :: q1, h2 :: q2 when h1 = h2 -> iter (q1, q2)
  | _ -> []
  in
  iter (base, path)

let handler current_state ~http_url ~ws_url base_path req =
  let uri = Cohttp.Request.uri req in
  let path = Stog_misc.split_string (Uri.pct_decode (Uri.path uri)) ['/'] in
  let path = path_after_base base_path path in
  match path with
  | [ "styles" ; file ] when file = Stog_server_preview.default_css ->
      Stog_server_preview.respond_default_css

  | "preview" :: path ->
      let http_url =
        { http_url with pub = preview_url http_url base_path }
      in
      let ws_url =
        { ws_url with pub = preview_url ws_url base_path }
      in
      Stog_server_preview.handle_preview http_url ws_url current_state req path

  | ["status"] ->
      begin
        match !current_state with
          None -> Lwt.fail (Failure "No state yet!")
        | Some state ->
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
      end

  | _ ->
      let body = Printf.sprintf "<html><header><title>Stog-server</title></header>
    <body>Hello world !</body></html>"
      in
      S.respond_string ~status:`OK ~body ()
