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

module S = Cohttp_lwt_unix.Server
module J = Yojson.Safe
open Stog_url

let (>>=) = Lwt.bind

module Server_P = struct
  include Ojs_rpc.Base(Stog_multi_ed_types.App_msg)
  let wsdata_of_msg msg = J.to_string (Stog_multi_ed_types.server_msg_to_yojson msg)
  let msg_of_wsdata = Ojs_server.mk_msg_of_wsdata Stog_multi_ed_types.client_msg_of_yojson
  end
module Server = Ojs_server.Make(Server_P)
module SFT = Ojsft_server.Make(Stog_multi_ed_types.FT)
module SED = Ojsed_server.Make(Stog_multi_ed_types.ED)
module Git = Stog_git_server.Make(Stog_multi_ed_types.Git)

(*
class myft broadcall broadcast ~id root =
  object(self)
    inherit SFT.filetree broadcall broadcast ~id root as super
    method handle_message msg =
      prerr_endline "message!";
      super#handle_message msg
  end
*)


let init ?sshkey ~stog_dir ~git =
  let connections = new Server.connection_group in
  let filetrees = new SFT.filetrees connections#broadcall connections#broadcast
    (new SFT.filetree)
  in
  let editors = new SED.editors connections#broadcall connections#broadcast
    (new SED.editor)
  in
  let git_repos = new Git.repos connections#broadcall connections#broadcast
    (new Git.repo)
  in
  let _ft = filetrees#add_filetree Stog_multi_ed_types.ft_id stog_dir in
  let _ed = editors#add_editor Stog_multi_ed_types.ed_id stog_dir in
  let _repo = git_repos#add_repo ~id: Stog_multi_ed_types.gitrepo_id ?sshkey git in
  let handle_message send_msg rpc msg =
    match msg with
    | Stog_multi_ed_types.ED.Editor _ -> editors#handle_message send_msg msg
    | Stog_multi_ed_types.FT.Filetree _ -> filetrees#handle_message send_msg msg
    | Stog_multi_ed_types.Git.Git _ -> git_repos#handle_message send_msg msg
    | Server_P.Call (call_id, ((Stog_multi_ed_types.FT.Filetree _) as msg))->
        let return msg = Server.Rpc.return rpc call_id msg in
        filetrees#handle_call return msg
    | Server_P.Call (call_id, ((Stog_multi_ed_types.ED.Editor _) as msg)) ->
        let return msg = Server.Rpc.return rpc call_id msg in
        editors#handle_call return msg
    | Server_P.Call (call_id, ((Stog_multi_ed_types.Git.Git _) as msg)) ->
        let return msg = Server.Rpc.return rpc call_id msg in
        git_repos#handle_call return msg
    | _ ->
        let str = Printf.sprintf
          "Unhandled message (Stog_multi_ed.handle_message): %s"
          (Printexc.to_string (Obj.magic msg))
        in
        failwith str
  in
  connections#set_handle_message handle_message;
  connections

let body_tmpl = [%xtmpl "templates/multi_ed.tmpl"]
let page cfg user ~ws_url ~title ~client_js_url =
  let client_js_url = Stog_url.to_string client_js_url in
  let js = [ "stog_server = { wsUrl: '"^(Stog_url.to_string ws_url)^"' } ;" ] in
  let body = body_tmpl
    ~client_js_url
    ~ft_id: Stog_multi_ed_types.ft_id
      ~ojs_msg_id: Stog_multi_ed_types.ojs_msg_id
      ~bar_id: Stog_multi_ed_types.bar_id
      ~git_id: Stog_multi_ed_types.gitrepo_id
      ~ed_id: Stog_multi_ed_types.ed_id
      ()
  in
  Stog_multi_page.page cfg user ~empty: true ~title ~js body

let client_js = "stog_multi_ed.js"

let editor_page cfg user ~http_url ~ws_url base_path session_id =
  let client_js_path = base_path @ [ "editor" ; client_js ] in
  (* FIXME: port number when we will be able to change an
    http connection into a websocket one manually *)
  let client_js_url = Stog_url.append http_url.pub client_js_path in
  let ws_url = Stog_url.append ws_url.pub (base_path @ ["editor"]) in
  let title = Printf.sprintf "Session %S" session_id in
  page cfg (Some user) ~ws_url ~title ~client_js_url

let http_handler cfg user ~http_url ~ws_url base_path session_id req body = function
| [s] when s = client_js -> Stog_server_preview.respond_server_js client_js
| [] | [""] ->
    let body = editor_page cfg user ~http_url ~ws_url base_path session_id in
    let body = Xtmpl.string_of_xmls body in
    S.respond_string ~status: `OK ~body ()
| _ ->
    S.respond_error ~status:`Not_found ~body: "" ()
