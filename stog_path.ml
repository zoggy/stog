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


exception Invalid of string

let invalid s = raise (Invalid s);;


type path = {
    path : string list;
    path_absolute : bool ;
  }

let path path path_absolute = { path ; path_absolute };;

let compare = Pervasives.compare;;

module Ordered = struct type t = path let compare = compare end;;
module Map = Map.Make(Ordered)
module Set = Set.Make(Ordered)

let append p l = { p with path = p.path @ l };;

let chop_extension path =
  match List.rev path.path with
    [] -> None
  | s :: q ->
      try
        let s = Filename.chop_extension s in
        Some { path with path = List.rev (s :: q) }
      with
        Invalid_argument _ -> (* no extension *) None
;;

let to_string path =
  Printf.sprintf "%s%s"
  (if path.path_absolute then "/" else "")
  (String.concat "/" path.path)

let of_string s =
  let len = String.length s in
  if len <= 0 then failwith (Printf.sprintf "Invalid path: %S" s);
  let (abs, s) =
    match s.[0] with
      '/' -> (true, String.sub s 1 (len - 1))
    | _ -> (false, s)
  in
  { path = Stog_misc.split_string s ['/'];
    path_absolute = abs ;
  }
;;

let parent p =
  assert p.path_absolute ;
  match List.rev p.path with
    [] -> p
  | _ :: q -> { p with path = List.rev q }
;;
