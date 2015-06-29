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

open Stog_url

module W = Ocf.Wrapper

type sha256 = string (* hexadecimal representation of SHA256 digest *)

type account = {
    login: string [@ocf W.string, ""];
    name : string [@ocf W.string, ""];
    email: string [@ocf W.string, ""];
    passwd: sha256 [@ocf W.string, ""];
  } [@@ocf]

type t = {
    accounts : account list [@ocf W.list account_wrapper, []] ;

    ssh_priv_key : string option
        [@ocf W.option W.string, None]
        [@ocf.label "ssh-priv-key"]
        [@ocf.doc "Private SSH key to access repository"] ;

    git_repo_url : string
        [@ocf W.string, ""]
        [@ocf.label "repository-url"]
        [@ocf.doc "URL of git repository"];

    dir : string
        [@ocf W.string, ""]
        [@ocf.label "directory"]
        [@ocf.doc "Directory where to clone repositories"] ;

    stog_dir : string option
        [@ocf W.option W.string, None]
        [@ocf.label "stog-directory"]
        [@ocf.doc "Optional subdirectory where to run stog in a clone"];

    editable_files : string list
        [@ocf W.list W.string, [] ]
        [@ocf.label "editable-files"]
        [@ocf.doc "Regexps of files to be able to edit"];

    not_editable_files : string list
        [@ocf W.list W.string, [] ]
        [@ocf.label "not-editable-files"]
        [@ocf.doc "Regexps of files not to be able to edit"];

    http_url : Stog_url.url_config
      [@ocf Stog_url.url_config_wrapper,
        Stog_url.default_url_config (Stog_url.of_string "http://localhost:8080")]
        [@ocf.label "http-server"]
        [@ocf.doc "URL of HTTP server"];

    ws_url : Stog_url.url_config
      [@ocf Stog_url.url_config_wrapper,
          Stog_url.default_url_config (Stog_url.of_string "ws://localhost:8081")]
        [@ocf.label "ws-server"]
        [@ocf.doc "URL of websocket server"];

    css_file : string option
        [@ocf W.option W.string, None]
        [@ocf.label "css-file"]
        [@ocf.doc "File to serve as default CSS file"] ;
  } [@@ocf]

let group () =
  let g = Ocf.group in
  let option_t = Ocf.option t_wrapper default_t in
  let g = Ocf.add g [] option_t in
  (g, option_t)

let read file =
  let (group, t) = group () in
  if not (Sys.file_exists file) then
    begin
      Ocf.to_file group file;
      failwith (Printf.sprintf "Empty configuration file %S created, please edit it" file);
    end;
  try
    Ocf.from_file group file ;
    let t = Ocf.get t in
    let t =
      let dir =
        match t.dir with
        | "" -> Sys.getcwd ()
        | s when Filename.is_relative s -> Filename.concat (Sys.getcwd ()) s
        | s -> s
      in
      { t with dir }
    in
    let t =
      match t.ssh_priv_key with
      | None -> t
      | Some file ->
          let f =
            if Filename.is_relative file then
              Filename.concat (Sys.getcwd ()) file
            else
              file
          in
          { t with ssh_priv_key = Some f }
    in
    let map_url c =
      { pub = Stog_url.remove_ending_slash c.Stog_url.pub ;
        priv = Stog_url.remove_ending_slash c.Stog_url.priv ;
      }
    in
    let t = { t with http_url = map_url t.http_url } in
    let t = { t with ws_url = map_url t.ws_url } in
    t
  with
    Ocf.Error e -> failwith (Ocf.string_of_error e)
  | e ->
      let msg =
        match e with
          Sys_error s | Failure s -> s
        | e -> Printexc.to_string e
      in
      failwith msg
;;
