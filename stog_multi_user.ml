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
open Stog_multi_session
open Stog_multi_gs
let (>>=) = Lwt.bind

let user_page_tmpl = [%xtmpl "templates/multi_user_page.tmpl"]

let page cfg account gs =
  let body = [] in
  Stog_multi_page.page cfg (Some account) ~title: account.name body

let create_session cfg sessions account =
  let session = Stog_multi_session.create cfg account in
  Stog_multi_gs.add_session session sessions ;
  session

let string_of_date = Netdate.mk_mail_date  
  
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
  let tds_of_session s = 
    let preview_url = Stog_types.string_of_url s.session_stog.stog_preview_url in
    let editor_url = Stog_types.string_of_url s.session_editor.editor_url in
    let st = s.session_stored in
    [
      td [Xtmpl.D (string_of_date st.session_create_date) ] ;
      td [Xtmpl.D st.session_orig_branch ] ;
      td [Xtmpl.D st.session_branch ] ;
      td [Xtmpl.E (("","a"), Xtmpl.atts_one ("","href") [Xtmpl.D preview_url], [Xtmpl.D "preview"])] ; 
      td [Xtmpl.E (("","a"), Xtmpl.atts_one ("","href") [Xtmpl.D editor_url], [Xtmpl.D "editor"])] ; 
    ]      
  in
  let trs = List.map
    (fun s -> Xtmpl.E (("","tr"), Xtmpl.atts_empty, tds_of_session s))
    sessions
  in
  [ Xtmpl.E (("","table"), Xtmpl.atts_empty, trs) ]
   

let handle_sessions_post cfg gs user req body =
  let session = create_session cfg gs.sessions user in
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
  Lwt.return (Stog_multi_page.page cfg (Some user) ~title contents)

let handle_sessions_get cfg gs user req body =
  let contents = session_list cfg gs user in
  let title = "Your sessions" in
  Lwt.return (Stog_multi_page.page cfg (Some user) ~title contents)
