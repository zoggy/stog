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

let (>>=) = Lwt.bind

module Server_P = struct
  include Ojs_rpc.Base(Stog_multi_ed_types.App_msg)
  let wsdata_of_msg msg = J.to_string (Stog_multi_ed_types.server_msg_to_yojson msg)
  let msg_of_wsdata = Ojs_server.mk_msg_of_wsdata Stog_multi_ed_types.client_msg_of_yojson
  end
module Server = Ojs_server.Make(Server_P)
module SFT = Ojsft_server.Make(Stog_multi_ed_types.FT)
module SED = Ojsed_server.Make(Stog_multi_ed_types.ED)


let init root_dir =
  let connections = new Server.connection_group in
  let filetrees = new SFT.filetrees connections#broadcall connections#broadcast
    (new SFT.filetree)
  in
  let editors = new SED.editors connections#broadcall connections#broadcast
    (new SED.editor)
  in
  let _ft = filetrees#add_filetree "ft" root_dir in
  let _ed = editors#add_editor "ed" root_dir in
  let handle_message send_msg rpc msg =
    match msg with
    | Stog_multi_ed_types.ED.Editor _ -> editors#handle_message send_msg msg
    | Stog_multi_ed_types.FT.Filetree _ -> filetrees#handle_message  send_msg msg
    | Server_P.Call (call_id, ((Stog_multi_ed_types.FT.Filetree _) as msg))->
        let return msg = Server.Rpc.return rpc call_id msg in
        filetrees#handle_call return msg
    | Server_P.Call (call_id, ((Stog_multi_ed_types.ED.Editor _) as msg)) ->
        let return msg = Server.Rpc.return rpc call_id msg in
        editors#handle_call return msg
    | _ ->
        failwith "Unhandled message"
  in
  connections#set_handle_message handle_message;
  connections

let page_template = "stog-multi-ed.tmpl"
let client_js = "stog_multi_ed.js"

let editor_page host port base_path session_id =
  let style_path = "/" ^ String.concat "/"
    (base_path @ [ "styles" ; Stog_server_preview.default_css ])
  in
  let client_js_path = "/" ^ String.concat "/"
    (base_path @ [ "editor" ; client_js ])
  in
  let tmpl = match Stog_server_files.read page_template with
      None -> failwith (Printf.sprintf "Template %S unavailable" page_template)
    | Some s -> Xtmpl.xml_of_string ~add_main: false s
  in
  (* FIXME: port number when we will be able to change an http connection into a websocket one
     manually *)
  let ws_url = Printf.sprintf "ws://%s:%d/%s/editor" host (port+1) (String.concat "/" base_path) in
  let add = Xtmpl.env_add_att in
  let env = Xtmpl.env_empty () in
  let env = add "page-title" [Xtmpl.D (Printf.sprintf "Stog-server: session %S" session_id)] env in
  let env = add "style-path" [Xtmpl.D style_path] env in
  let env = add "client-js-path" [Xtmpl.D client_js_path] env in
  let env = add "ws-url" [ Xtmpl.D ws_url ] env in
  let (_, xmls) = Xtmpl.apply_to_xmls () env [tmpl] in
  Xtmpl.string_of_xmls xmls

let http_handler host port base_path session_id req body = function
| [s] when s = client_js -> Stog_server_preview.respond_server_js client_js
| [] | [""] ->
    let body = editor_page host port base_path session_id in
    S.respond_string ~status: `OK ~body ()
| _ ->
    S.respond_error ~status:`Not_found ~body: "" ()
