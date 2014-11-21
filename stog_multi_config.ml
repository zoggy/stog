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

module CF = Config_file

type sha256 = string (* hexadecimal representation of SHA256 digest *)
type account = {
    user: string ;
    name : string ;
    email: string ;
    passwd: sha256 ;
  }
type t = {
    accounts : account list ;
    ssh_priv_key : string ;
    git_repo_url : string ;
    editable_files : Str.regexp list ;
    not_editable_files : Str.regexp list ;
  }

let read_config file =
  let group = new CF.group in
  let o_accounts = new CF.list_cp
    (CF.tuple4_wrappers CF.string_wrappers CF.string_wrappers CF.string_wrappers CF.string_wrappers)
      ~group
      ["accounts"] [ ] "triples (login, name, email, sha256-hashed password)"
  in
  let o_ssh = new CF.string_cp ~group
    ["ssh-priv-key"] "" "Private SSH key to access repository"
  in
  let o_git_repo = new CF.string_cp ~group
    ["repository-url"] "" "URL of git repository"
  in
  let o_editable = new CF.list_cp CF.string_wrappers ~group
    ["editable-files"] []
    "Regexps of files to be able to edit"
  in
  let o_not_editable = new CF.list_cp CF.string_wrappers ~group
    ["not-editable-files"] []
    "Regexps of files not to be able to edit"
  in
  if not (Sys.file_exists file) then
     failwith (Printf.sprintf "Cannot file file %S" file);

  group#read file;
  let accounts = List.map
    (fun (user, name, email, passwd) -> { user ; name ; email ; passwd })
    o_accounts#get
  in
  { accounts ;
    ssh_priv_key = o_ssh#get ;
    git_repo_url = o_git_repo#get ;
    editable_files = List.map Str.regexp o_editable#get ;
    not_editable_files = List.map Str.regexp o_not_editable#get ;
  }
;;