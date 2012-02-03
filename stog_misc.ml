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

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)


(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)


(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)


(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)


(*c==v=[String.lowercase]=1.0====*)
let lowercase s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c =
      match s.[i] with
      | 'à' | 'â' | 'ä' -> 'a'
      | 'é' | 'è' | 'ê' | 'ë' -> 'e'
      | 'î' | 'ï' -> 'i'
      | 'ô' | 'ö' -> 'o'
      | 'ù' | 'û' | 'ü' -> 'u'
      | 'ç' -> 'c'
      | c -> Char.lowercase c
    in
    Buffer.add_char b c
  done;
  Buffer.contents b
(*/c==v=[String.lowercase]=1.0====*)


(*c==v=[List.list_chop]=1.0====*)
let rec list_chop n = function
    [] -> []
  | h :: q ->
      if n > 0 then
	h :: (list_chop (n-1) q)
      else
	[]
(*/c==v=[List.list_chop]=1.0====*)

let mkdir dir =
  try Unix.mkdir dir 0o755
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (e, s1, s2) ->
      failwith (Printf.sprintf "%s: %s %s"
       (Unix.error_message e) s1 s2)
;;

(*c==v=[String.is_prefix]=1.0====*)
let is_prefix s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  (len1 <= len2) &&
    (String.sub s2 0 len1) = s1
(*/c==v=[String.is_prefix]=1.0====*)


(*c==v=[List.list_remove_doubles]=1.0====*)
let list_remove_doubles ?(pred=(=)) l =
  List.fold_left
    (fun acc e -> if List.exists (pred e) acc then acc else e :: acc)
    []
    (List.rev l)
(*/c==v=[List.list_remove_doubles]=1.0====*)

let md5 s = Digest.to_hex (Digest.string s);;

let count_char s c =
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = c then incr r
  done;
  !r
;;

let encode_char c = Printf.sprintf "&#%03d;" (Char.code c);;
let encode_string s =
  let len = String.length s in
  let b = Buffer.create (6 * len) in
  for i = 0 to len - 1 do
    Buffer.add_string b (encode_char s.[i])
  done;
  Buffer.contents b
;;

let map_opt f = function None -> None | Some x -> Some (f x);;

