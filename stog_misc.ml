(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
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


let strip_blank_lines str =
  let blank_line = function '\n'|'\r' -> true | _ -> false in
  let prefix, suffix = ref 0, ref 0 in
  let len = String.length str in
  while !prefix < len && blank_line str.[!prefix] do
    incr prefix
  done;
  while !suffix > 0 && blank_line str.[!prefix] do
    decr suffix
  done;
  String.sub str !prefix (len - !suffix - !prefix)
;;


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

let list_concat ?sep l =
  match sep with
    None -> l
  | Some sep ->
      let rec iter acc = function
        [] -> List.rev acc
      | h :: q ->
          match acc with
            [] -> iter [h] q
          | acc -> iter (h :: sep :: acc) q
      in
      iter [] l
;;

let dot_to_svg dot =
  let temp_file = Filename.temp_file "genet" "svg" in
  let com = Printf.sprintf "echo %s | dot -Tsvg | tail --lines=+7 > %s"
    (Filename.quote dot) (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let svg = string_of_file temp_file in
      Sys.remove temp_file;
      svg
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;


let rec list_compare comp l1 l2 =
 match l1, l2 with
   [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | h1::q1, h2::q2 ->
      match comp h1 h2 with
        0 -> list_compare comp q1 q2
      | n -> n
;;

let filename_extension s =
  let len = String.length s in
  try
    let p = String.rindex s '.' in
    if p < len - 1 then String.sub s (p+1) (len - p - 1) else ""
  with Not_found -> ""
;;

let safe_mkdir dir =
  let com = Printf.sprintf "mkdir -p %s" (Filename.quote dir) in
  match Sys.command com with
    0 -> ()
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;

(*c==v=[String.string_of_opt]=1.0====*)
let string_of_opt = function
  None -> ""
| Some s -> s
(*/c==v=[String.string_of_opt]=1.0====*)

(*c==v=[String.opt_of_string]=1.0====*)
let opt_of_string = function
  "" -> None
| s -> Some s
(*/c==v=[String.opt_of_string]=1.0====*)

let file_mtime file =
  try Some (Unix.stat file).Unix.st_mtime
  with _ -> None
;;

let path_under ~parent file =
  if is_prefix parent file then
    begin
      let len = String.length parent + 1 in
      String.sub file len (String.length file - len)
    end
  else
    failwith (Printf.sprintf "%s is not under %s" file parent)
;;

let highlight ~opts code =
  let code_file = Filename.temp_file "stog" "code" in
  file_of_string ~file: code_file code;
  let temp_file = Filename.temp_file "stog" "highlight" in
  let com = Printf.sprintf
    "highlight -O xhtml %s -f %s > %s"
    opts (Filename.quote code_file)(Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let code = string_of_file temp_file in
      Sys.remove code_file;
      Sys.remove temp_file;
      strip_string code
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let string_of_time t =
  let d = Unix.gmtime t in
  Printf.sprintf "%04d/%02d/%02d-%02d:%02d:%02d"
    (d.Unix.tm_year + 1900) (d.Unix.tm_mon+1) d.Unix.tm_mday
    d.Unix.tm_hour d.Unix.tm_min d.Unix.tm_sec
;;

