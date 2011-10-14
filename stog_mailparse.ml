
(*c==m=[Mail.Parse]=1.0=t==*)
(***********************************************************************)
(*                                                                     *)
(*                 SpamOracle -- a Bayesian spam filter                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  This file is distributed under the terms of the   *)
(*  GNU Public License version 2, http://www.gnu.org/licenses/gpl.txt  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: smailparse.ml 41 2005-10-13 14:42:53Z guesdon $ *)

type message =
  { headers: (string * string) list;
    body: string;
    parts: message list }

let base64_decode_char c =
  match c with
    'A' .. 'Z' -> Char.code c - 65
  | 'a' .. 'z' -> Char.code c - 97 + 26
  | '0' .. '9' -> Char.code c - 48 + 52
  | '+' -> 62
  | '/' -> 63
  | _ -> -1

let decode_base64 s =
  let d = Buffer.create (String.length s * 3 / 4) in
  let buf = Array.create 4 0 in
  let pos = ref 0 in
  for i = 0 to String.length s - 1 do
    let n = base64_decode_char s.[i] in
    if n >= 0 then begin
      buf.(!pos) <- n;
      incr pos;
      if !pos = 4 then begin
        Buffer.add_char d (Char.chr(buf.(0) lsl 2 + buf.(1) lsr 4));
        Buffer.add_char d (Char.chr((buf.(1) land 15) lsl 4 + buf.(2) lsr 2));
        Buffer.add_char d (Char.chr((buf.(2) land 3) lsl 6 + buf.(3)));
        pos := 0
      end
    end
  done;
  begin match !pos with
    2 ->
      Buffer.add_char d (Char.chr(buf.(0) lsl 2 + buf.(1) lsr 4))
  | 3 ->
      Buffer.add_char d (Char.chr(buf.(0) lsl 2 + buf.(1) lsr 4));
      Buffer.add_char d (Char.chr((buf.(1) land 15) lsl 4 + buf.(2) lsr 2))
  | _ ->
      ()
  end;
  Buffer.contents d

let hexa_digit c =
  if c >= '0' && c <= '9' then Char.code c - 48
  else if c >= 'A' && c <= 'F' then Char.code c - 65 + 10
  else if c >= 'a' && c <= 'f' then Char.code c - 97 + 10
  else raise Not_found

let decode_qp s =
  let len = String.length s in
  let d = Buffer.create (String.length s) in
  let pos = ref 0 in
  while !pos < len do
    let c = s.[!pos] in
    if c = '=' && !pos + 1 < len && s.[!pos + 1] = '\n' then begin
      pos := !pos + 2
    end else if c = '=' && !pos + 2 < len then begin
      try
        let h1 = hexa_digit s.[!pos + 1]
        and h2 = hexa_digit s.[!pos + 2] in
        Buffer.add_char d (Char.chr(h1 lsl 4 + h2));
        pos := !pos + 3
      with Not_found ->
        Buffer.add_char d c;
        incr pos
    end else begin
      Buffer.add_char d c;
      incr pos
    end
  done;
  Buffer.contents d

let re_base64 = Str.regexp_case_fold "base64"
let re_qp = Str.regexp_case_fold "quoted-printable"

let decode encoding s =
  if Str.string_match re_base64 encoding 0 then
    decode_base64 s
  else if Str.string_match re_qp encoding 0 then
    decode_qp s
  else
    s

let re_encoded_header =
  Str.regexp "=\\?[_A-Za-z0-9-]+\\?\\([BbQq]\\)\\?\\([^?]*\\)\\?="

let decode_header s =
  let decode_group s =
    let enc = Str.matched_group 1 s
    and txt = Str.matched_group 2 s in
    match enc with
      "B" | "b" -> decode_base64 txt
    | "Q" | "q" -> decode_qp txt
    | _ -> assert false in
  Str.global_substitute re_encoded_header decode_group s

let re_continuation = Str.regexp "\n[ \t]+"
let re_nl = Str.regexp "\n"
let re_field = Str.regexp "\\([A-Za-z-]+[: ]\\)[ \t]*\\(.*\\)"

let parse_header s =
  let rec parse_field accu = function
    [] -> List.rev accu
  | line :: rem ->
      if Str.string_match re_field line 0 then begin
        let field_name = String.lowercase (Str.matched_group 1 line)
        and field_val  = Str.matched_group 2 line in
        parse_field ((field_name, decode_header field_val) :: accu) rem
      end else
        parse_field accu rem
  in
  parse_field [] (Str.split re_nl (Str.global_replace re_continuation " " s))

let find_header name headers =
  try List.assoc name headers with Not_found -> ""

let re_nl_nl = Str.regexp "\n\n"
let re_multipart =
  Str.regexp_case_fold
    "multipart/.*boundary *= *\\(\"\\([^\"]+\\)\"\\|\\([^ \t]+\\)\\)"

let rec parse_message s =
  try
    let pos_sep = Str.search_forward re_nl_nl s 0 in
    let headers = parse_header (String.sub s 0 pos_sep) in
    let body = String.sub s (pos_sep + 2) (String.length s - pos_sep - 2) in
    let encoding = find_header "content-transfer-encoding:" headers in
    let ctype = find_header "content-type:" headers in
    if Str.string_match re_multipart ctype 0 then begin
      let boundary =
        try
          Str.matched_group 2 ctype
        with Not_found -> try
          Str.matched_group 3 ctype
        with Not_found ->
          assert false in
      let re_bound =
        Str.regexp ("--" ^ Str.quote boundary ^ "[ \t\n]*") in
      match Str.split_delim re_bound body with
        [] ->
          { headers = headers;
            body = decode encoding body;
            parts = [] }
      | blurb :: parts ->
          { headers = headers;
            body = decode encoding blurb;
            parts = List.map parse_message parts }
    end else
      { headers = headers;
        body = decode encoding body;
        parts = [] }
  with Not_found ->
    { headers = [];
      body = s;
      parts = [] }

let header s msg =
  let rec hdr = function
    [] -> []
  | (h,v) :: rem -> if h = s then v :: hdr rem else hdr rem in
  String.concat "\n" (hdr msg.headers)

let header_matches s re msg =
  let rec hmatch = function
    [] -> false
  | (h,v) :: rem -> (h = s && Str.string_match re v 0) || hmatch rem
  in hmatch msg.headers

(*/c==m=[Mail.Parse]=1.0=t==*)

let messages_from_dir dir =
  let files = Stog_find.find_list
    Stog_find.Ignore [dir]
    [Stog_find.Maxdepth 1 ; Stog_find.Type Unix.S_REG]
  in
  List.map
  (fun file ->
     parse_message (Stog_misc.string_of_file file)
  )
  files
;;

module Str_map = Stog_types.Str_map;;
module G = Stog_graph.Make_with_map
  (struct type t = string let compare = compare end)
  (struct type t = unit let compare = compare end)
;;

let re_subject = Str.regexp "\\[\\([0-9a-f]+\\)\\(/\\([0-9a-f]+\\)\\)?\\]";;

let cut_subject m =
  let s = header "subject:" m in
  let (sub, hid,irt) =
    try
      let p = Str.search_forward re_subject s 0 in
      let subject = String.sub s 0 p in
      let hid =
        try Some (Str.matched_group 1 s)
        with _ -> None
      in
      let in_rep =
        try Some (Str.matched_group 3 s)
        with _ -> None
      in
      (subject, hid, in_rep)
    with
      Not_found -> (s, None, None)
  in
  prerr_endline
  (Printf.sprintf "cut(%s)= (%s, %s, %s)"
   s sub
   (match hid with None -> "NONE" | Some h -> h)
   (match irt with None -> "NONE" | Some h -> h)
  );
  (sub, hid, irt)
;;

let get_in_reply_to m =
  match header "in-reply-to:" m with
    "" ->
      begin
        (* get the information in the subject *)
        let (_,_,irt) = cut_subject m in
        irt
      end
  | s -> Some (Stog_misc.md5 s)
;;

let build_message_tree messages =
  let stog_message m =
    let (subject, _, _) = cut_subject m in
    { Stog_types.mes_time = Stog_date.parse (header "date:" m) ;
      mes_subject = subject ;
      mes_from = header "from:" m ;
      mes_to = Stog_misc.split_string (header "to:" m) [','] ;
      mes_body = m.body ;
      mes_id = header "message-id:" m ;
    }
  in
  let f acc m =
    match header "message-id:" m with
      "" -> acc
    | s -> Str_map.add (Stog_misc.md5 s) (stog_message m) acc
  in
  let ids = List.fold_left f Str_map.empty messages in
  let f g m =
    match get_in_reply_to m with
      None ->
        let id = Stog_misc.md5 (header "message-id:" m) in
        (* add and remove node so that it is present in the graph *)
        let g = G.add g (id, id, ()) in
        G.rem_all g (id, id)
    | Some s ->
        try
          ignore(Str_map.find s ids);
          let id = Stog_misc.md5 (header "message-id:" m) in
          G.add g (s, id, ())
        with Not_found ->
            prerr_endline (Printf.sprintf "Not_found in_reply_to id: %s" s);
            g
  in
  let g = List.fold_left f (G.create ()) messages in
  let roots = G.pred_roots g in
  let rec f node =
    prerr_endline (Printf.sprintf "f node = %s" node);
    let succs = G.succ g node in
    let subs = List.map (fun (n, _) -> f n) succs in
    prerr_endline (Printf.sprintf "%d subs" (List.length subs));
    Stog_types.Node (Str_map.find node ids, subs)
  in
  List.map f roots
;;

