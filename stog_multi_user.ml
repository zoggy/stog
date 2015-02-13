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
open Stog_multi_config
open Stog_git_server
open Stog_multi_session
open Stog_multi_gs
let (>>=) = Lwt.bind
module H = Xtmpl_xhtml

let user_page_tmpl = [%xtmpl "templates/multi_user_page.tmpl"]

let create_session cfg sessions account =
  let%lwt session = Stog_multi_session.create cfg account in
  Stog_multi_gs.add_session session sessions ;
  Lwt.return session

let string_of_date = Netdate.mk_date ~fmt: "%F %R"

let session_list cfg gs user =
  let sessions =
    Str_map.fold
      (fun session_id session acc ->
        if session.session_stored.session_author = user.login then
          session :: acc
         else
           acc
      )
      !(gs.sessions)
      []
  in
  let td x = Xtmpl.E(("","td"), Xtmpl.atts_empty, x) in
  let nbsp = Stog_multi_page.nbsp in
  let tds_of_session s =
    let preview_url = Stog_types.string_of_url s.session_stog.stog_preview_url in
    let editor_url = Stog_types.string_of_url s.session_editor.editor_url in
    let st = s.session_stored in
    [
      td [Xtmpl.D (string_of_date st.session_create_date) ] ;
      td [Xtmpl.D st.session_git.origin_url ; H.br ;
          Xtmpl.D ("{ " ^ st.session_git.origin_branch ^ " }") ] ;
      td ([
        Xtmpl.D st.session_git.edit_branch ;
        H.br ;
        H.a ~href: preview_url [Xtmpl.D "preview"] ; nbsp ;
        H.a ~href: editor_url [Xtmpl.D "editor"] ; nbsp
        ]
      )
    ]
  in
  let trs = List.map
    (fun s -> Xtmpl.E (("","tr"), Xtmpl.atts_empty, tds_of_session s))
    sessions
  in
  let headers =
    let th s = Xtmpl.E(("","th"), Xtmpl.atts_empty, [Xtmpl.D s]) in
    Xtmpl.E (("","tr"), Xtmpl.atts_empty,
     [ th "Creation date" ; th "Origin {branch}" ; th "Current branch" ;
       th "" ; th "" ;
     ]
    )
  in
  [ Xtmpl.E (("","table"), Xtmpl.atts_one ("","class") [Xtmpl.D "table"], headers :: trs) ]


module Form_session = [%ojs.form "templates/form_session.tmpl"]

let new_session_button cfg =
  let action = Stog_multi_page.url_sessions cfg in
  Form_session.form ~action ()

let page cfg gs ?message ?error user =
  let sessions = session_list cfg gs user in
  let sessions = (new_session_button cfg) @ sessions in
  let body = user_page_tmpl ~name: user.name ~sessions () in
  Stog_multi_page.page cfg (Some user) ~title: user.name ?message ?error body

let handle_sessions_post cfg gs user req body =
  let%lwt session = create_session cfg gs.sessions user in
  let message = `Msg (Printf.sprintf "Session %s created" session.session_id) in
  Lwt.return (page cfg gs ~message user)

let handle_sessions_get cfg gs user req body =
  Lwt.return (page cfg gs user)

    