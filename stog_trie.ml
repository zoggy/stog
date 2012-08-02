(** *)

module type S =
  sig
    type symbol
    type data
    type path = symbol list
    type 'a elt = path * 'a
    type 'a t
    exception Already_present of path
    val empty : 'a t
    val add : 'a elt -> 'a t -> 'a t
    val find : 'a elt -> 'a t -> 'a elt list
  end

module type P =
  sig
    type t
    val compare : t -> t -> int
  end

module Make (P : P) =
  struct
    module Map = Map.Make (P)
    type symbol = P.t
    type path = symbol list
    type 'a elt = path * 'a
    type 'a t = Node of 'a t Map.t * 'a option | Leaf of 'a elt
    exception Already_present of path

    let empty = Node (Map.empty, None)
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

    let rec add ?(backpath=[]) elt t =
      match elt with
        ([], _) -> t
      | ((sym :: q) as path, data) ->
          if is_empty t then
             Node (Map.add sym (Leaf (q, data)) Map.empty, None)
          else
            match t with
              Leaf (orig_path2,data2) ->
                let (pref, path1, path2) = common_prefix path orig_path2 in
                if path1 = [] && path2 = [] then
                  raise (Already_present ((List.rev backpath) @ orig_path2));
                let rec iter = function
                  [] ->
                    let map, data_opt =
                      match path1 with
                        [] -> (Map.empty, Some data)
                      | sym :: q -> (Map.add sym (Leaf (q, data)) Map.empty, None)
                    in
                    let map, data_opt =
                      match path2 with
                        [] ->
                          assert (data_opt = None);
                          (map,Some data2)
                      | sym :: q -> (Map.add sym (Leaf (q, data2)) map, data_opt)
                    in
                    Node (map, data_opt)
                | sym :: q ->
                    let t = iter q in
                    Node (Map.add sym t Map.empty, None)
                in
                iter pref

            | Node (map, data_opt) ->
                try
                  let t2 = Map.find sym map in
                  match q, data_opt with
                    [], Some d ->
                      raise (Already_present (List.rev backpath))
                  | [], None ->
                      Node (map, Some data)
                  | _, _ ->
                    add ~backpath: (sym :: backpath) (q, data) t2
                with
                  Not_found ->
                    Node (Map.add sym (Leaf (q, data)) map, data_opt)

    let add elt t = add elt t

  end