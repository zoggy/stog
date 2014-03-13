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

open Stog_types;;

module Smap = Stog_types.Str_map;;

module Depset = Stog_types.Depset

let add_dep stog doc dep =
  match doc.doc_type with
    "by-keyword" | "by-month" | "by-topic" -> stog
  | _ ->
      (* we make both the doc and its parent and brothers depend on dep;
         this is to force recomputing of parent document when
         one of its children depends on something which changed,
         and in this case the children document must be invalidated too.
         By adding the same dependency for all (parent and childre),
         we ensure that none or all are invalidated and will not be loaded
         from cache.*)
      let parent =
        match doc.doc_parent with
          None -> doc
        | Some path ->
            let (_, doc) = Stog_types.doc_by_path stog path in
            doc
      in
      let srcs = parent :: (Stog_types.doc_children stog parent) in

      let dep =
        match dep with
          File f -> File f
        | Doc doc ->
            (* need the stog to get parent document eventually *)
            let dst_path =
              match doc.doc_parent with
                None -> doc.doc_path
              | Some path -> path
            in
            let dst_path = Stog_path.to_string dst_path in
            Doc dst_path
        in

      let src_paths = List.map
        (fun doc -> Stog_path.to_string doc.doc_path) srcs
      in
      let f_doc stog doc =
        let src_path = Stog_path.to_string doc.doc_path in
        let set =
          try Smap.find src_path stog.stog_deps
          with Not_found -> Depset.empty
        in
        let set =
          match dep with
            File f ->
              (*prerr_endline ("add dep "^src_path^" -> "^f);*)
              Depset.add dep set
          | Doc dst_path ->
              (* do not add deps from an document to its parent, child or brothers *)
              if List.mem dst_path src_paths then
                set
              else
                Depset.add dep set
        in
        { stog with stog_deps = Smap.add src_path set stog.stog_deps }
      in
      List.fold_left f_doc stog srcs
;;

let string_of_file_time f =
  match Stog_misc.file_mtime f with
    None -> "<notime>"
  | Some t -> Stog_misc.string_of_time t
;;

let string_of_doc_time stog doc_by_path path =
  try
    let doc = doc_by_path path in
    let src_file = Filename.concat stog.stog_dir doc.doc_src in
    match Stog_misc.file_mtime src_file with
      None -> "<notime>"
    | Some t -> Stog_misc.string_of_time t
  with
    e -> Printexc.to_string e
;;

let print_dep b doc_by_path stog = function
  File file ->
    Printf.bprintf b "  File %S modified at %s\n" file (string_of_file_time file)
| Doc path ->
    Printf.bprintf b "  Doc %S modified at %s\n" path
      (string_of_doc_time stog doc_by_path path)
;;

let max_deps_date stog doc_by_path path =
  let rec f dep (acc, depth) =
    if Depset.mem dep acc then
      (acc, depth)
    else
      let acc = Depset.add dep acc in
      match dep with
        File file -> (acc, depth)
      | Doc path ->
          try
            if stog.stog_depcut && depth >= 1 then
              (acc, depth)
            else
              (
               let doc_deps = Smap.find path stog.stog_deps in
               Depset.fold f doc_deps (acc, depth+1)
              )
          with Not_found ->
              (acc, depth)
  in
  let (deps,_) = f (Doc path) (Depset.empty,0) in
  Stog_msg.verbose ~level: 5
    (let b = Buffer.create 256 in
     Printf.bprintf b "%S depends on\n" path;
     Depset.iter (print_dep b doc_by_path stog) deps;
     Buffer.contents b
    );
  let max_date dep acc =
    let date_opt =
      match dep with
        File file -> Stog_misc.file_mtime file
      | Doc path ->
          try
            let doc = doc_by_path path in
            let src = Filename.concat stog.stog_dir doc.doc_src in
            Stog_misc.file_mtime src
          with Not_found ->
              None
    in
    match date_opt with
      None ->
        (* the document which we previously depended on does not exist;
           use current date to force recomputing *)
        Unix.time ()
    | Some date ->
        max acc date
  in
  Depset.fold max_date deps 0.
;;