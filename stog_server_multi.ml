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

let add_session session sessions =
  sessions := Stog_types.Str_map.add session.session_id session !sessions

let create_session cfg sessions account =
  let session = Stog_multi_session.create cfg account in
  add_session session sessions ;
  session

let restart_previous_sessions cfg sessions =
  List.iter
    (fun session ->
       try
         prerr_endline ("restarting "^(session.session_id));
         start_session session ;
         add_session session sessions
       with e -> prerr_endline (Printexc.to_string e)
    )
    (Stog_multi_session.load_previous_sessions cfg)

let handle_new_session cfg sessions req body =
  Cohttp_lwt_body.to_string body >>= fun body ->
    match
      let form = Stog_multi_page.read_form_new_session req body in
      match
        let account = List.find (fun acc -> acc.login = form.Stog_multi_page.login) cfg.accounts in
        prerr_endline (Printf.sprintf "account found: %s\npasswd=%s" account.login account.passwd);
        let pwd = sha256 form.Stog_multi_page.passwd in
        prerr_endline (Printf.sprintf "sha256(pwd)=%s" pwd);
        if pwd = String.lowercase account.passwd then
          account
        else
          raise Not_found
      with
      | exception Not_found -> failwith "Invalid user/password"
      | account -> create_session cfg sessions account
    with
    | exception (Failure err) ->
        let contents = Stog_multi_page.form_new_session ~err cfg.app_url in
        let page = Stog_multi_page.page ~title: "New session" contents in
        let body = Xtmpl.string_of_xml page in
        S.respond_string ~status:`OK ~body ()
    | session ->
        let preview_url = Stog_types.string_of_url session.session_stog.stog_preview_url in
          let contents =
          [
            Xtmpl.E (("","p"), Xtmpl.atts_empty, [
               Xtmpl.D "Preview URL: ";
               Xtmpl.E (("","a"),
                Xtmpl.atts_of_list
                  [("","href"), [ Xtmpl.D preview_url ]],
                [Xtmpl.D preview_url]) ;
             ])
          ]
        in
        let title = Printf.sprintf "Session %s created" session.session_id in
        let page = Stog_multi_page.page ~title contents in
        let body = Xtmpl.string_of_xml page in
        S.respond_string ~status:`OK ~body ()

let req_path_from_app cfg req =
  let app_path = Neturl.url_path cfg.app_url in
  let req_uri = Cohttp.Request.uri req in
  let req_path = Stog_misc.split_string (Uri.path req_uri) ['/'] in
  let rec iter = function
  | [], p -> p
  | h1::q1, h2::q2 when h1 = h2 -> iter (q1, q2)
  | _, _ ->
      let msg = Printf.sprintf  "bad query path: %S is not under %S"
        (Uri.to_string req_uri)
        (Stog_types.string_of_url cfg.app_url)
      in
      failwith msg
  in
  iter (app_path, req_path)

let handler cfg sessions host port sock req body =
  let path = req_path_from_app cfg req in
  match path with
  | [] ->
      let contents = Stog_multi_page.form_new_session cfg.app_url in
      let page = Stog_multi_page.page ~title: "New session" contents in
      let body = Xtmpl.string_of_xml page in
      S.respond_string ~status:`OK ~body ()

  | ["styles" ; s] when s = Stog_server_preview.default_css ->
      Stog_server_preview.respond_default_css ()

  | p when p = Stog_multi_page.path_sessions && req.S.Request.meth = `POST ->
      handle_new_session cfg sessions req body

  | p when p = Stog_multi_page.path_sessions && req.S.Request.meth = `GET ->
      let body =
        "<html><header><title>Stog-server</title></header>"^
          "<body>List of sessions: not implemented yet</body></html>"
      in
      S.respond_string ~status:`OK ~body ()

  | _ ->
      match path with
      | "sessions" :: session_id :: q when req.S.Request.meth = `GET ->
          begin
            match Str_map.find session_id !sessions with
            | exception Not_found ->
                let body = Printf.sprintf "Session %S not found" session_id in
                S.respond_error ~status:`Not_found ~body ()
            | session ->
                match q with
                | ["styles" ; s] when s = Stog_server_preview.default_css ->
                    Stog_server_preview.respond_default_css ()

                | "preview" :: _ ->
                    let base_path =
                      (Neturl.url_path cfg.app_url) @
                        Stog_multi_page.path_sessions @ [session_id]
                    in
                    Stog_server_http.handler session.session_stog.stog_state
                      host port base_path sock req body

                | "editor" :: p  ->
                    let base_path =
                      (Neturl.url_path cfg.app_url) @
                        Stog_multi_page.path_sessions @ [session_id]
                    in
                    Stog_multi_ed.http_handler host port base_path session_id req body p

                | _ -> S.respond_error ~status:`Not_found ~body:"" ()
          end
      | _ ->
          let body =
            "<html><header><title>Stog-server</title></header>"^
              "<body>404 Not found</body></html>"
          in
          S.respond_error ~status:`Not_found ~body ()

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
  restart_previous_sessions cfg sessions ;
  Stog_multi_ws.run_server cfg sessions >>=
  fun _ -> start_server cfg sessions host port

let () =
  let run host port args = Lwt_unix.run (launch host port args) in
  Stog_server_mode.set_multi run

