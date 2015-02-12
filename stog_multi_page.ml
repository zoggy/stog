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

open Stog_multi_config
module S = Cohttp_lwt_unix.Server
module H = Xtmpl_xhtml

let url_ cfg path =
  let url = List.fold_left Stog_types.url_concat cfg.app_url path in
  Stog_types.string_of_url url

let path_login = ["login"]
let path_sessions = ["sessions"]

let url_login cfg = url_ cfg path_login
let url_sessions cfg = url_ cfg path_sessions

let page_tmpl = [%xtmpl "templates/multi_page.tmpl"]
let page_body_tmpl = [%xtmpl "templates/multi_page_body.tmpl"]

let app_name = "Stog-multi-server"

type block = [`Msg of string | `Block of Xtmpl.tree list]

let xmls_of_block = function
| (`Msg str) -> [Xtmpl.D str]
| (`Block xmls) -> xmls

let error_block b =
  let xmls =  xmls_of_block b in
  let atts = Xtmpl.atts_one ("","class") [Xtmpl.D "alert alert-error"] in
  [ Xtmpl.E (("","div"), atts, xmls) ]

let message_block b =
  let xmls =  xmls_of_block b in
  let atts = Xtmpl.atts_one ("","class") [Xtmpl.D "alert alert-info"] in
  [ Xtmpl.E (("","div"), atts, xmls) ]

let nbsp = List.hd ([%xtmpl.string "&#xa0;"] ())

let mk_js_script code =
  H.script ~type_: "text/javascript" [ Xtmpl.D code ]

let page cfg account_opt ?(empty=false) ~title ?error ?message ?(js=[]) body =
  let topbar = [] in
  let css_url = url_ cfg ["styles" ; Stog_server_preview.default_css ] in
  let js = List.map mk_js_script js in
  let headers = (Ojs_tmpl.link_css css_url) :: js in
  let page_error =
    match error with
      None -> None
    | Some e -> Some (error_block e)
  in
  let page_message =
    match message with
      None -> None
    | Some e -> Some (message_block e)
  in
  let body =
    if empty then
      body
    else
      page_body_tmpl ~title ~topbar ?page_error ?page_message ~body ()
  in
  page_tmpl ~app_name ~title ~headers ~body ()

module Form_login = [%ojs.form "templates/form_login.tmpl"]

let param_of_body body =
  let params = Uri.query_of_encoded body in
  fun s ->
    match List.assoc s params with
    | exception Not_found -> None
    | [] | "" :: _ -> None
    | s :: _ -> Some s



