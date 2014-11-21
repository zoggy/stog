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
open Xtmpl
let noatts = Xtmpl.atts_empty

let path_sessions = ["sessions"]

let page ~title contents =
  E (("","html"), noatts, [
     E(("","header"), noatts, [
        E(("","title"), noatts, [D title])
      ]) ;
     E(("","body"), noatts, [
       E(("","div"), atts_of_list [("","id"), [D "page"]],
         contents
        )
     ])
   ]
  )


type form_new_session = {
    login : string ;
    passwd : string ;
  }

let form_new_session ?err app_url  =
  let field ~id ~typ ~label ~placeholder =
    E (("","div"), noatts, [
       E (("","label"), Xtmpl.atts_of_list [ ("", "for"), [ D id ]],
        [ D label ]) ;
       E (("","input"),
        Xtmpl.atts_of_list
          [
            ("","name"), [ D id ] ;
            ("","type"), [ D typ ] ;
            ("","placeholder"), [D placeholder] ;
            ("","required"), [ D "required"] ;
          ],
          [])
        ])
  in
  let action_url =
    let url = List.fold_left Stog_types.url_concat app_url path_sessions in
    Stog_types.string_of_url url
  in
  [E (("", "form"),
     Xtmpl.atts_of_list
     [ ("","id"), [ D "form-session"] ;
         ("","action"), [ D action_url ] ;
         ("","method"), [ D "POST" ] ;
       ],
     [ E (("","fieldset"), noatts, [
          field ~id: "login" ~typ:"text" ~placeholder:"Your login" ~label: "Login:" ;
          field ~id: "passwd" ~typ:"password" ~placeholder:"Your password" ~label: "Password:" ;
        ] @
        (match err with
           None -> []
         | Some msg ->
             [E (("","div"), Xtmpl.atts_of_list
                [ ("","class"), [D "alert alert-error"] ],
                [ D msg]
               )]
        ) @
          [ E (("","input"),
             Xtmpl.atts_of_list [
               ("","name"), [D "submit"] ;
               ("","type"), [D "submit"] ;
               ("","value"), [D "Open session"] ;
             ],
             []);
        ])
     ])]

let read_form_new_session req body =
  let params = Uri.query_of_encoded body in
  (*List.iter
    (fun (s,vals) -> prerr_endline (Printf.sprintf "%s=%s" s (String.concat "," vals)))
    params;*)
  match List.assoc "login" params with
  | exception Not_found -> failwith "Missing login"
  | [] | "" :: _ -> failwith "Missing login"
  | login :: _ ->
      match List.assoc "passwd" params with
      | exception Not_found -> failwith "Missing password"
      | [] | "" :: _ -> failwith "Missing password"
      | passwd :: _ -> { login ; passwd }



