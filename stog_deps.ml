(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
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
type dependency = File of string | Elt of string;;

module Depset =
  Set.Make (struct type t = dependency let compare = Pervasives.compare end)

let deps = ref Smap.empty;;

let add_dep elt dep =
  match elt.elt_type with
    "by-keyword" | "by-month" | "by-topic" -> ()
  | _ ->
      let hid = Stog_types.string_of_human_id elt.elt_human_id in
      let set =
        try Smap.find hid !deps
        with Not_found -> Depset.empty
      in
      let set = Depset.add dep set in
      deps := Smap.add hid set !deps
;;

let string_of_file_time f =
  match Stog_misc.file_mtime f with
    None -> "<notime>"
  | Some t -> Stog_misc.string_of_time t
;;

let string_of_elt_time stog hid =
  try
    let hid = Stog_types.human_id_of_string hid in
    let (_,elt) = Stog_types.elt_by_human_id stog hid in
    let src_file = Filename.concat stog.stog_dir elt.elt_src in
    match Stog_misc.file_mtime src_file with
      None -> "<notime>"
    | Some t -> Stog_misc.string_of_time t
  with
    e -> Printexc.to_string e
;;

let print_dep b stog = function
  File file ->
    Printf.bprintf b "  File %S modified at %s\n" file (string_of_file_time file)
| Elt hid ->
    Printf.bprintf b "  Elt %S modified at %s\n" hid (string_of_elt_time stog hid)
;;

let max_deps_date stog hid =
  let rec f dep (acc, depth) =
    if Depset.mem dep acc then
      (acc, depth)
    else
      let acc = Depset.add dep acc in
      match dep with
        File file -> (acc, depth)
      | Elt hid ->
          try
            if stog.stog_depcut && depth >= 1 then
              (acc, depth)
            else
              (
               let elt_deps = Smap.find hid !deps in
               Depset.fold f elt_deps (acc, depth+1)
              )
          with Not_found ->
              (acc, depth)
  in
  let (deps,_) = f (Elt hid) (Depset.empty,0) in
  Stog_msg.verbose ~level: 5
    (let b = Buffer.create 256 in
     Printf.bprintf b "%S depends on\n" hid;
     Depset.iter (print_dep b stog) deps;
     Buffer.contents b
    );
  let max_date dep acc =
    let date_opt =
      match dep with
        File file -> Stog_misc.file_mtime file
      | Elt hid ->
          try
            let (_,elt) = Stog_types.elt_by_human_id stog
              (Stog_types.human_id_of_string hid)
            in
            let src = Filename.concat stog.stog_dir elt.elt_src in
            Stog_misc.file_mtime src
          with Failure _ ->
              None
    in
    match date_opt with
      None ->
        (* the element which we previously depended on does not exist;
           use current date to force recomputing *)
        Unix.time ()
    | Some date ->
        max acc date
  in
  Depset.fold max_date deps 0.
;;