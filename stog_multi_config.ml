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
open Stog_url

type sha256 = string (* hexadecimal representation of SHA256 digest *)
type account = {
    login: string ;
    name : string ;
    email: string ;
    passwd: sha256 ;
  }
type t = {
    accounts : account list ;
    ssh_priv_key : string option;
    git_repo_url : string ;
    dir : string ;
    stog_dir : string option ;
    editable_files : Str.regexp list ;
    not_editable_files : Str.regexp list ;
    http_url : Stog_url.url_config ;
    ws_url : Stog_url.url_config ;
    css_file : string option ;
  }

let read file =
  let group = new CF.group in
  let o_accounts = new CF.list_cp
    (CF.tuple4_wrappers CF.string_wrappers CF.string_wrappers CF.string_wrappers CF.string_wrappers)
      ~group
      ["accounts"] [ ] "triples (login, name, email, sha256-hashed password)"
  in
  let o_dir = new CF.string_cp ~group
    ["directory"] "" "Directory where to clone repositories"
  in
  let o_stog_dir = new CF.string_cp ~group
    ["stog-directory"] "" "Optional subdirectory where to run stog in a clone"
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
  let o_http_url = new CF.string_cp ~group
    ["http-url"] "http://localhost:8080" "URL of HTTP server"
  in
  let o_ws_url = new CF.string_cp ~group
    ["ws-url"] "http://localhost:8081" "URL of websocket server"
  in
  let o_public_http_url = new CF.option_cp CF.string_wrappers ~group
    ["public-http-url"] None "Public URL of HTTP server"
  in
  let o_public_ws_url = new CF.option_cp CF.string_wrappers ~group
    ["public-ws-url"] None "Public URL of websocket server"
  in
  let o_css_file = new CF.option_cp CF.string_wrappers ~group
    ["css-file"] None "File to serve as default CSS file"
  in
  if not (Sys.file_exists file) then
    begin
      group#write file;
      failwith (Printf.sprintf "Empty configuration file %S created, please edit it" file);
    end;

  try
    group#read file;
    let accounts = List.map
      (fun (login, name, email, passwd) -> { login ; name ; email ; passwd })
        o_accounts#get
    in
    let dir =
      match o_dir#get with
      | "" -> Sys.getcwd ()
      | s when Filename.is_relative s -> Filename.concat (Sys.getcwd ()) s
      | s -> s
    in
    let ssh_priv_key =
      let file = o_ssh#get in
      match file with
        "" -> None
      | _ ->
          let f =
            if Filename.is_relative file then
              Filename.concat (Sys.getcwd ()) file
            else
              file
          in
        Some f
    in
    let map_url str =
      let url = Stog_url.of_string str in
      Stog_url.remove_ending_slash url
    in
    let http_url = map_url o_http_url#get in
    let ws_url = map_url o_ws_url#get in
    let public_http_url =
      match o_public_http_url#get with
        None -> http_url
      | Some u -> map_url u
    in
    let public_ws_url =
      match o_public_ws_url#get with
        None -> ws_url
      | Some u -> map_url u
    in
    (*prerr_endline "app_url path:";
    List.iter prerr_endline (Stog_types.url_path app_url);*)
    { accounts ;
      ssh_priv_key ;
      git_repo_url = o_git_repo#get ;
      dir ;
      stog_dir = (match o_stog_dir#get with "" -> None | s -> Some s);
      editable_files = List.map Str.regexp o_editable#get ;
      not_editable_files = List.map Str.regexp o_not_editable#get ;
      http_url = { pub = public_http_url ; priv = http_url } ;
      ws_url = { pub = public_ws_url ; priv = ws_url } ;
      css_file = o_css_file#get ;
    }
  with
    e ->
      let msg =
        match e with
          Sys_error s | Failure s -> s
        | e -> Printexc.to_string e
      in
      failwith msg
;;
