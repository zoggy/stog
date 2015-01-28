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
open Stog_multi_session

let user_page_tmpl = [%xtmpl "templates/multi_user_page.tmpl"]

let create_session cfg sessions account =
  let session = Stog_multi_session.create cfg account in
  Stog_multi_gs.add_session session sessions ;
  session

(*
let handle_new_session cfg gs req body =
  Cohttp_lwt_body.to_string body >>= fun body ->
    let module F = Stog_multi_page.Form_login in
    match
      let (tmpl, form) = F.read_form (Stog_multi_page.param_of_body body) in
      match
        let account = List.find (fun acc -> acc.login = form.F.login) cfg.accounts in
        prerr_endline (Printf.sprintf "account found: %s\npasswd=%s" account.login account.passwd);
        let pwd = sha256 form.F.password in
        prerr_endline (Printf.sprintf "sha256(pwd)=%s" pwd);
        if pwd = String.lowercase account.passwd then
          account
        else
          raise Not_found
      with
      | exception Not_found -> raise (F.Error (tmpl, ["Invalid user/password"]))
      | account -> create_session cfg gs.sessions account
    with
    | exception (F.Error (tmpl, errors)) ->
        let error_msg = List.map
          (fun msg -> Xtmpl.E (("","div"), Xtmpl.atts_empty, [Xtmpl.D msg]))
            errors
        in
        let contents = tmpl ~error_msg ~action: (action_form_login cfg.app_url) () in
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

*)

let page cfg account gs =
  let body = [] in
  Stog_multi_page.page cfg (Some account) ~title: account.name body