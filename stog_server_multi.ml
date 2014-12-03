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
open Xtmpl
open Stog_multi_config
open Stog_multi_session

module S = Cohttp_lwt_unix.Server
let (>>=) = Lwt.bind

let sha256 s = String.lowercase (Sha256.to_hex (Sha256.string s))


let create_session cfg sessions account =
  let session = Stog_multi_session.create cfg account in
  sessions := Stog_types.Str_map.add session.session_id session !sessions;
  (* return cookie + url of editor and preview *)
  assert false

let handle_new_session cfg sessions req body =
  Cohttp_lwt_body.to_string body >>= fun body ->
    match
      let form = Stog_multi_page.read_form_new_session req body in
      match
        let account = List.find (fun acc -> acc.login = form.Stog_multi_page.login) cfg.accounts in
        let pwd = sha256 form.Stog_multi_page.passwd in
        if pwd = sha256 account.passwd then
          account
        else
          raise Not_found
      with
      | exception Not_found -> failwith "Invalid user/password"
      | account ->
          create_session cfg sessions account

    with
    | exception (Failure err) ->
        let contents = Stog_multi_page.form_new_session ~err cfg.app_url in
        let page = Stog_multi_page.page ~title: "New session" contents in
        let body = Xtmpl.string_of_xml page in
        S.respond_string ~status:`OK ~body ()
    | form ->
        let page = Stog_multi_page.page ~title: "New session" [D "ok"] in
        let body = Xtmpl.string_of_xml page in
        S.respond_string ~status:`OK ~body ()


let handler cfg sessions host port sock req body =
  let uri = Cohttp.Request.uri req in
  let path = Stog_misc.split_string (Uri.path uri) ['/'] in
  match path with
  | p when p = Stog_multi_page.path_sessions && req.S.Request.meth = `POST ->
      handle_new_session cfg sessions req body

  | p when p = Stog_multi_page.path_sessions && req.S.Request.meth = `GET ->
      let body =
        "<html><header><title>Stog-server</title></header>"^
          "<body>List of sessions: not implemented yet</body></html>"
      in
      S.respond_string ~status:`OK ~body ()

  | id :: _ ->
      begin
        match Str_map.find id !sessions with
          session ->
            Stog_server_http.handler session.session_state host port [id] sock req body
        | exception Not_found ->
            let body =
              "<html><header><title>Stog-server</title></header>"^
                "<body>Space "^id^" not found</body></html>"
            in
            S.respond_error ~status:`Not_found ~body ()
      end
  | _ ->
      let contents = Stog_multi_page.form_new_session cfg.app_url in
      let page = Stog_multi_page.page ~title: "New session" contents in
      let body = Xtmpl.string_of_xml page in
      S.respond_string ~status:`OK ~body ()

let start_server cfg sessions host port =
  Lwt_io.write Lwt_io.stdout
    (Printf.sprintf "Listening for HTTP request on: %s:%d\n" host port)
  >>= fun _ ->
  let conn_closed id () =
    ignore(Lwt_io.write Lwt_io.stdout
      (Printf.sprintf "connection %s closed\n%!" (Cohttp.Connection.to_string id)))
  in
  let config = { S.callback = handler cfg sessions host port ; conn_closed } in
  S.create ~address:host ~port config

let launch host port args =
  let cfg =
    match args with
      [] -> failwith "Please give a configuration file"
    | file :: _ -> Stog_multi_config.read file
  in
  let sessions = ref (Str_map.empty : session Str_map.t) in
  start_server cfg sessions host port

let () =
  let run host port args = Lwt_unix.run (launch host port args) in
  Stog_server_mode.set_multi run

