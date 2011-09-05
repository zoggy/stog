(** *)

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)
