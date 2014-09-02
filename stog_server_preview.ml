(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
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
open Stog_types
open Stog_server_run

let (>>=) = Lwt.bind

let rec preview_file stog path =
  let rec iter tree = function
    [] -> S.respond_file ~fname: "" ()
  | [f] ->
      if Stog_types.Str_set.mem f tree.files then
        let fname = Filename.concat stog.stog_dir (String.concat Filename.dir_sep path) in
        S.respond_file ~fname ()
      else
        S.respond_file ~fname: "" ()
  | d :: q ->
      match
        try Some (Stog_types.Str_map.find d tree.dirs)
        with Not_found -> None
      with
        Some tree -> iter tree q
      | None -> S.respond_file ~fname: "" ()
  in
  iter stog.stog_files path

let handle_preview sock req body path =
  Stog_server_run.state () >>=
    function state ->
        match
          try Some(Stog_types.doc_by_path state.stog (Stog_path.path path true))
          with _ -> None
        with
          Some (doc_id, doc) ->
            let body =
              match doc.doc_out with
                None ->
                  String.concat "\n"
                    [ "Document not computed yet";
                      String.concat "\n" state.stog_errors ;
                      String.concat "\n" state.stog_warnings ;
                    ]
              | Some xmls -> Xtmpl.string_of_xmls xmls
            in
            S.respond_string ~status:`OK ~body ()
        | None -> preview_file state.stog path

