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
open Stog_multi_gs

module S = Cohttp_lwt_unix.Server
let (>>=) = Lwt.bind

let restart_previous_sessions cfg sessions =
  List.iter
    (fun session ->
       try
         prerr_endline ("restarting "^(session.session_id));
         start_session session ;
         Stog_multi_gs.add_session session sessions
       with e -> prerr_endline (Printexc.to_string e)
    )
    (Stog_multi_session.load_previous_sessions cfg)

let add_logged gs user_token account =
  gs.logged := Str_map.add user_token account !(gs.logged)

let new_token () = Stog_multi_session.new_id ()
let token_cookie = "STOGMULTILOGINTOKEN"

let action_form_login app_url =
  let url = List.fold_left Stog_types.url_concat app_url
    Stog_multi_page.path_sessions
  in
  Stog_types.string_of_url url

let sha256 s = String.lowercase (Sha256.to_hex (Sha256.string s))

let respond_page page =
  let body = Xtmpl.string_of_xmls page in
  S.respond_string ~status:`OK ~body ()

let handle_login_post cfg gs req body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let module F = Stog_multi_page.Form_login in
  match
    let (tmpl, form) = F.read_form (Stog_multi_page.param_of_body body) in
    try
      let account = List.find (fun acc -> acc.login = form.F.login) cfg.accounts in
      prerr_endline (Printf.sprintf "account found: %s\npasswd=%s" account.login account.passwd);
      let pwd = sha256 form.F.password in
      prerr_endline (Printf.sprintf "sha256(pwd)=%s" pwd);
      if pwd = String.lowercase account.passwd then
        account
      else
        raise Not_found
    with
    | Not_found -> raise (F.Error (tmpl, ["Invalid user/password"]))
  with
  | exception (F.Error (tmpl, errors)) ->
      let error_msg =
        Stog_multi_page.error_block
          (`Block (List.map
            (fun msg -> Xtmpl.E (("","div"), Xtmpl.atts_empty, [Xtmpl.D msg]))
              errors))
      in
      let contents = tmpl ~error_msg ~action: (Stog_multi_page.url_login cfg) () in
      let page = Stog_multi_page.page cfg None ~title: "Login" contents in
      respond_page page

  | account ->
      let token = new_token () in
      add_logged gs token account;
      let cookie = Cohttp.Cookie.Set_cookie_hdr.make
          ~expiration: `Session
          ~path: ("/"^(String.concat "/" (Neturl.url_path cfg.app_url)))
          ~http_only: true
          (token_cookie, token)
      in
      let page = Stog_multi_user.page cfg gs account in
      let body = Xtmpl.string_of_xmls page in
      let headers =
        let (h,s) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
        Cohttp.Header.init_with h s
      in
      S.respond_string ~headers ~status:`OK ~body ()

let handle_login_get cfg gs opt_user =
  match opt_user with
    Some user -> respond_page (Stog_multi_user.page cfg gs user)
  | None ->
      let module F = Stog_multi_page.Form_login in
      let contents = F.form ~action: (Stog_multi_page.url_login cfg) () in
      let page = Stog_multi_page.page cfg None ~title: "Login" contents in
      respond_page page

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

let get_opt_user gs req =
  let h = Cohttp.Request.headers req in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract h in
  try
    let c = List.assoc token_cookie cookies in
    Some (Str_map.find c !(gs.logged))
  with Not_found ->
    None

let require_user cfg opt_user f =
  match opt_user with
    None ->
      let error = `Msg "You must be connected. Please log in" in
      respond_page (Stog_multi_page.page cfg None ~title: "Error" ~error [])
  | Some user -> f user


let handle_path cfg gs host port sock opt_user req body = function
| [] ->
    let contents = Stog_multi_page.Form_login.form
      ~action: (Stog_multi_page.url_login cfg) ()
    in
    let page = Stog_multi_page.page cfg None ~title: "Login" contents in
    let body = Xtmpl.string_of_xmls page in
    S.respond_string ~status:`OK ~body ()

| ["styles" ; s] when s = Stog_server_preview.default_css ->
    Stog_server_preview.respond_default_css ()

| p when p = Stog_multi_page.path_login && req.S.Request.meth = `GET->
    handle_login_get cfg gs opt_user

| p when p = Stog_multi_page.path_login && req.S.Request.meth = `POST ->
    handle_login_post cfg gs req body

| p when p = Stog_multi_page.path_sessions && req.S.Request.meth = `GET ->
    require_user cfg opt_user
      (fun user ->
         Stog_multi_user.handle_sessions_get cfg gs user req body >>= respond_page)

| p when p = Stog_multi_page.path_sessions && req.S.Request.meth = `POST ->
    require_user cfg opt_user
      (fun user ->
         Stog_multi_user.handle_sessions_post cfg gs user req body >>= respond_page)

| p when p = Stog_multi_page.path_session_push ->
    require_user cfg opt_user
      (fun user ->
         (match req.S.Request.meth with
            `POST -> Stog_multi_user.handle_session_push cfg gs user req body
          | _ -> Stog_multi_user.handle_sessions_get cfg gs user req body
         ) >>= respond_page
      )

| p when p = Stog_multi_page.path_session_pull ->
    require_user cfg opt_user
      (fun user ->
         (match req.S.Request.meth with
            `POST -> Stog_multi_user.handle_session_pull cfg gs user req body
          | _ -> Stog_multi_user.handle_sessions_get cfg gs user req body
         ) >>= respond_page
      )

| path ->
    match path with
    | "sessions" :: session_id :: q when req.S.Request.meth = `GET ->
        begin
          match Str_map.find session_id !(gs.sessions) with
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
                  require_user cfg opt_user
                    (fun user ->
                       Stog_multi_ed.http_handler cfg user host port
                         base_path session_id req body p)

              | _ -> S.respond_error ~status:`Not_found ~body:"" ()
        end
    | _ ->
        let body =
          "<html><header><title>Stog-server</title></header>"^
            "<body>404 Not found</body></html>"
        in
        S.respond_error ~status:`Not_found ~body ()

let handler cfg gs host port sock req body =
  let path = req_path_from_app cfg req in
  let opt_user = get_opt_user gs req in
  Lwt.catch
    (fun () -> handle_path cfg gs host port sock opt_user req body path)
    (fun e ->
       let msg =
         match e with
           Failure msg | Sys_error msg -> msg
         | _ -> Printexc.to_string e
       in
       S.respond_error ~status: `Internal_server_error ~body: msg ()
    )

let start_server cfg gs host port =
  Lwt_io.write Lwt_io.stdout
    (Printf.sprintf "Listening for HTTP request on: %s:%d\n" host port)
  >>= fun _ ->
  let conn_closed id () =
    ignore(Lwt_io.write Lwt_io.stdout
      (Printf.sprintf "connection %s closed\n%!" (Cohttp.Connection.to_string id)))
  in
  let config = { S.callback = handler cfg gs host port ; conn_closed } in
  S.create ~address:host ~port config

let launch host port args =
  let cfg =
    match args with
      [] -> failwith "Please give a configuration file"
    | file :: _ -> Stog_multi_config.read file
  in
  let gs = {
    sessions = ref (Str_map.empty : session Str_map.t) ;
    logged = ref (Str_map.empty : account Str_map.t) ;
    }
  in
  restart_previous_sessions cfg gs.sessions ;
  Stog_multi_ws.run_server cfg gs >>=
  fun _ -> start_server cfg gs host port

let () =
  let run host port args = Lwt_unix.run (launch host port args) in
  Stog_server_mode.set_multi run

