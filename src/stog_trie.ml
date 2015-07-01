(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2015 INRIA All rights reserved.                         *)
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

module type S =
  sig
    type symbol
    type path = symbol list
    type 'a t
    exception Already_present of path
    val empty : 'a t
    val add : ?fail: bool -> path -> 'a -> 'a t -> 'a t
    val find : path -> 'a t -> 'a list
    val to_string : (symbol -> string) -> 'a t -> string
  end

module Make (P : Map.OrderedType) =
  struct
    module Map = Map.Make (P)
    type symbol = P.t
    type path = symbol list
    type 'a doc = path * 'a list
    type 'a t = Node of 'a t Map.t * 'a list | Leaf of 'a doc
    exception Already_present of path

    let empty = Node (Map.empty, [])
    let is_empty t = t = empty

    let common_prefix l1 l2 =
      let rec iter acc l1 l2 =
        match l1, l2 with
        | [], _
        | _, [] -> (List.rev acc, l1, l2)
        | h1 :: q1, h2 :: q2 ->
            match P.compare h1 h2 with
              0 -> iter (h1 :: acc) q1 q2
            | _ -> (List.rev acc, l1, l2)
      in
      iter [] l1 l2

    let rec add ?(fail=false) ?(backpath=[]) path data t =
      match path with
        [] -> t
      | sym :: q ->
          if is_empty t then
             Leaf (path, [data])
          else
            match t with
              Leaf (orig_path2, data2) ->
                let (pref, path1, path2) = common_prefix path orig_path2 in
                if path1 = [] && path2 = [] then
                  if fail then
                    raise (Already_present ((List.rev backpath) @ orig_path2))
                  else
                    (* if we are here, we just need to add the new data
                       to the existing one, and return the modified leaf *)
                     Leaf (pref, data :: data2)
                else
                  let rec iter = function
                    [] ->
                      let map, data_opt =
                        match path1 with
                          [] -> (Map.empty, [data])
                        | sym :: q -> (Map.add sym (Leaf (q, [data])) Map.empty, [])
                      in
                      let map, data_opt =
                        match path2 with
                          [] ->
                            assert (data_opt = []);
                            (map, data2)
                        | sym :: q -> (Map.add sym (Leaf (q, data2)) map, data_opt)
                      in
                      Node (map, data_opt)
                  | sym :: q ->
                      let t = iter q in
                      Node (Map.add sym t Map.empty, [])
                  in
                  iter pref

            | Node (map, data_opt) ->
                try
                  let t2 = Map.find sym map in
                  match q, data_opt with
                  | [], [] ->
                      Node (map, [data])
                  | [], d ->
                      if fail then
                        raise (Already_present (List.rev (sym :: backpath)))
                      else
                        Node (map, data :: d)
                  | _, _ ->
                      let x = add ~backpath: (sym :: backpath) q data t2 in
                      Node (Map.add sym x map, data_opt)
                with
                  Not_found ->
                    Node (Map.add sym (Leaf (q, [data])) map, data_opt)

    let add ?fail path data t = add ?fail path data t

    let rec is_path_prefix p1 p2 =
      match p1, p2 with
        [], [] -> true
      | [], _ -> true
      | _, [] -> false
      | h1 :: q1, h2 :: q2 ->
          match P.compare h1 h2 with
            0 -> is_path_prefix q1 q2
          | _ -> false

    let rec docs ?(acc=[]) path = function
      Leaf (p, data) ->
        let path =  path @ p in
        List.fold_left (fun acc d -> (path, d) :: acc) acc data
    | Node (map, data_opt) ->
        let acc =
          match data_opt with
            [] -> acc
          | data -> List.fold_left (fun acc d -> (path, d) :: acc) acc data
        in
        Map.fold (fun sym t acc -> docs ~acc (path @ [sym]) t) map acc

    let rec find ?(backpath=[]) path t =
      match path with
        [] -> List.map snd (docs (List.rev backpath) t)
      | sym :: q ->
          match t with
            Leaf (p, data) ->
              if is_path_prefix path p then
                data
              else
                []
          | Node (map, _) ->
              try
                let t = Map.find sym map in
                find ~backpath: (sym :: backpath) q t
              with Not_found ->
                  []

    let find path t = find path t

    let to_string f t =
      let b = Buffer.create 256 in
      let rec iter margin = function
        Leaf (p, data) ->
          Printf.bprintf b "%s[%s(leaf)]\n" margin
            (String.concat "/" (List.map f (List.rev p)))
      | Node (map, opt) ->
          begin
            match opt with
              [] -> ()
            | _ -> Printf.bprintf b "%s[data]\n" margin
          end;
          Map.iter
            (fun k v ->
               Printf.bprintf b "%s%s\n" margin (f k); iter (margin^"  ") v)
            map
      in
      iter "" t;
      Buffer.contents b


(*    let remove path t =
      let rec iter t = function
        [] -> t
      | [sym] ->
          begin
            match t with
              Leaf l when l = [sym] ->
          end
      | sym :: q ->
         *)
  end