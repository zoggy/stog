(** *)

module type S =
  sig
    type symbol
    type path = symbol list
    type 'a t
    exception Already_present of path
    val empty : 'a t
    val add : path -> 'a -> 'a t -> 'a t
    val find : path -> 'a t -> 'a list
  end

module Make (P : Map.OrderedType) =
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

    let rec add ?(backpath=[]) path data t =
      match path with
        [] -> t
      | sym :: q ->
          if is_empty t then
             Leaf (path, data) (*Node (Map.add sym (Leaf (q, data)) Map.empty, None)*)
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
                    let x = add ~backpath: (sym :: backpath) q data t2 in
                    Node (Map.add sym x map, data_opt)
                with
                  Not_found ->
                    Node (Map.add sym (Leaf (q, data)) map, data_opt)

    let add path data t = add path data t

    let rec is_path_prefix p1 p2 =
      match p1, p2 with
        [], [] -> true
      | [], _ -> true
      | _, [] -> false
      | h1 :: q1, h2 :: q2 ->
          match P.compare h1 h2 with
            0 -> is_path_prefix q1 q2
          | _ -> false

    let rec elts ?(acc=[]) path = function
      Leaf (p, data) -> (path @ p, data) :: acc
    | Node (map, data_opt) ->
        let acc =
          match data_opt with
            None -> acc
          | Some data -> (path, data) :: acc
        in
        Map.fold (fun sym t acc -> elts ~acc (path @ [sym]) t) map acc

    let rec find ?(backpath=[]) path t =
      match path with
        [] -> List.map snd (elts (List.rev backpath) t)
      | sym :: q ->
          match t with
            Leaf (p, data) ->
              if is_path_prefix path p then
                [data]
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
        Leaf (p, _) -> Printf.bprintf b "%s[%s(leaf)]\n" margin (String.concat "/" (List.map f (List.rev p)))
      | Node (map, opt) ->
          begin
          match opt with
              None -> ()
            | Some _ -> Printf.bprintf b "%s[data]\n" margin
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