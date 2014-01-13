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

(** LaTeX to Stog translator. *)

module SMap = Map.Make (String);;

module SSOrd =
  struct type t = string * string
    let compare (s1,s2) (s3,s4) =
      match String.compare s1 s3 with
        0 -> String.compare s2 s4
      | n -> n
  end
module SSMap = Map.Make(SSOrd)
module SSSet = Set.Make(SSOrd)

type tree =
  Source of string
| Block of block

and block = {
    tag : Xtmpl.name ;
    title : tree list ;
    id : string option ;
    subs : tree list ;
    atts : Xtmpl.attributes ;
  }

let block ?(atts=Xtmpl.atts_empty) ?id ?(title=[]) tag subs =
  { tag ; title ; id ; subs ; atts }
;;

let string_of_tag = function
  ("",s) -> s
| (p,s) -> p^":"^s
;;

let rec string_of_tree = function
  Source s -> "<source>"^s^"</source>"
| Block b ->
  "<"^(string_of_tag b.tag)^" title="^(string_of_tree_list b.title)^" id="^(match b.id with None -> "_" | Some s -> s)^">\n"^
  (string_of_tree_list b.subs)^"\n</"^(string_of_tag b.tag)^">\n"

and string_of_tree_list l =
  String.concat "\n" (List.map string_of_tree l)

type preambule_section = (string option * string)
type preambule = preambule_section list

type tex_file = {
    preambule : preambule ;
    body : tree list ;
  }


let begin_document = "\\begin{document}";;
let re_begin_document = Str.regexp_string begin_document;;

let end_document = "\\end{document}";;
let re_end_document = Str.regexp_string end_document;;

(*
let test = "
%
";;
let re = Str.regexp "[^\\\\]%\\([^<].*$\\)?$";;
let p = Str.search_forward re test 0;;
print_int p;;
*)

let remove_comments s =
  (*let re = Str.regexp "[^\\\\]%\\(\\([^<].*$\\)\\|$\\)" in*)
  let re = Str.regexp "^%+$" in
  let f s = "\n" in
  let s = Str.global_substitute re f s in
  let re = Str.regexp "[^\\\\]%\\([^<\n].*$\\)$" in
  let f s =
    let matched = Str.matched_string s in
    (*prerr_endline ("matched='"^matched^"'");*)
    String.make 1 matched.[0]
  in
  let s = Str.global_substitute re f s in
  (*prerr_endline ("no-comments = "^s);*)
  s
;;

let nbsp = "\xc2\xa0";;  (* \xc2\xa0 : non breakable space *)

let replace_strings s =
  let rules = [
    Str.regexp_string "<<~", "\xc2\xab"^nbsp ;
    Str.regexp_string "~>>", nbsp^"\xc2\xbb" ;
    Str.regexp_string "~\\ref", nbsp^"\\ref" ;
    Str.regexp_string "~\\eqref", nbsp^"\\eqref" ;
    Str.regexp_string "~\\cite", nbsp^"\\cite" ;
    Str.regexp_string "~\\textcite", nbsp^"\\textcite" ;
    Str.regexp_string "~:", nbsp^":" ;
  ]
  in
  let repl s (re, s2) = Str.global_replace re s2 s in
  List.fold_left repl s rules
;;

let str_cut s_re re ?(start=0) ?(fail=true) s =
  try
    let p = Str.search_forward re s start in
    let before = String.sub s 0 p in
    let matched = Str.matched_string s in
    let after =
      let len = String.length s in
      let len_matched = String.length matched in
      if p + len_matched < len then
        String.sub s (p+len_matched) (len - len_matched - p)
      else
        ""
    in
    (before, matched, after)
  with Not_found ->
      if fail then
        failwith ("Could not find "^s_re)
      else
        (s, "", "")
;;
type token =
  Tex of char
| Tex_block of char * token list
| Tex_command of string
| Tex_blank of string
| Tex_dbs (* double backslash *)
| Tex_math1 of string
| Tex_math2 of string

type state = Normal | Blank of string | Escaping | Command of string | Math1 of string | Math2 of string

let close_of_open = function
    '{' -> '}'
  | '[' -> ']'
  | _ -> assert false
;;

let rec string_of_token = function
| Tex c -> String.make 1 c
| Tex_blank s -> s
| Tex_dbs -> "\\\\"
| Tex_block (c, l) ->
    let b = Buffer.create 256 in
    Buffer.add_char b c ;
    List.iter (fun t -> Buffer.add_string b (string_of_token t)) l;
    Buffer.add_char b (close_of_open c);
    Buffer.contents b
| Tex_command name -> "\\"^name
| Tex_math1 s -> "$"^s^"$"
| Tex_math2 s -> "\\["^s^"\\]"
;;

let string_of_token_list l =
  let b = Buffer.create 256 in
  List.iter (fun t -> Buffer.add_string b (string_of_token t)) l;
  Buffer.contents b
;;

let tokenize =
  let is_com_char = function
       'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | ':' | '~' | '*' -> true
     | _ -> false
  in
  let is_blank = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
  in
  let fail source msg i =
    let i = max 0 (i - 1) in
    let len = String.length source in
    let context = 80 in
    let before =
      let p = max 0 (i-context) in
      String.sub source p (i-p)
    in
    let after =
      let i = min (len-1) i in
      let p = min (len-1) (i+context) in
      String.sub source i (p-i)
    in
    let msg = Printf.sprintf "character %d: %s\n%s\n<--HERE-->%s" i msg before after in
    failwith msg
  in
  let rec iter source len state acc bchar i =
     if i >= len then
       match bchar with
         Some c ->
           List.iter (fun t -> prerr_string (string_of_token t)) (List.rev acc);
           let msg = Printf.sprintf "Missing closing '%c'" c in
           fail source msg i
       | None ->
           match state with
             Normal
           | Blank _ -> (List.rev acc, -1)
           | Escaping ->
              fail source "Missing character after '\\'" i
           | Command com ->
              (List.rev ((Tex_command com) :: acc), -1)
           | Math1 s | Math2 s ->
              fail source ("Unterminated math formula: "^s) i
     else
       match state, source.[i] with
       | Math1 s, '$' -> iter source len Normal ((Tex_math1 s) :: acc) bchar (i+1)
       | Math1 s, c -> iter source len (Math1 (s^(String.make 1 c))) acc bchar (i+1)
       | Math2 s, '\\' ->
          if i+1 < len then
            match source.[i+1] with
              ']' -> iter source len Normal ((Tex_math2 s) :: acc) bchar (i+2)
            | _ -> iter source len (Math2 (s^"\\")) acc bchar (i+1)
          else
            iter source len (Math2 s) acc bchar (i+1)
       | Math2 s, c -> iter source len (Math2 (s^(String.make 1 c))) acc bchar (i+1)
       | Escaping, '[' ->
          iter source len (Math2 "") acc bchar (i+1)
       | Escaping, '\\' ->
          iter source len Normal (Tex_dbs :: acc) bchar (i+1)
       | Escaping, '@' ->
          iter source len Normal ((Tex_command "@") :: acc) bchar (i+1)
       | Escaping, c ->
           if is_com_char c then
             iter source len (Command (String.make 1 c)) acc bchar (i+1)
           else
             iter source len Normal ((Tex c) :: (Tex '\\') :: acc) bchar (i+1)
       | Blank s, c when is_blank c ->
          let state = Blank (s^(String.make 1 c)) in
          iter source len state acc bchar (i+1)
       | Blank s, _ ->
          iter source len Normal ((Tex_blank s) :: acc) bchar i
       | Normal, '$' ->
           iter source len (Math1 "") acc bchar (i+1)
       | Normal, c when c = ']' || c = '}' ->
         let close =
           match c, bchar with
           | '}', Some '}' -> true
           | '}', _ ->
             prerr_endline (string_of_token_list (List.rev acc));
             fail source "Too many }'s" i
           | ']', Some ']' -> true
           | ']', _ -> false
           | _ -> assert false
         in
         if close then
           (List.rev acc, i+1)
         else
           iter source len Normal ((Tex c) :: acc) bchar (i+1)
       | Command com, c when c = ']' || c = '}' ->
           let acc = (Tex_command com) :: acc in
           iter source len Normal acc bchar i
       | Normal, '{'
       | Normal, '[' ->
          let c = source.[i] in
          let cc = close_of_open c in
          let (l, next) = iter source len Normal [] (Some cc) (i+1) in
          let acc = (Tex_block (c, l)) :: acc in
          iter source len Normal acc bchar next
       | Normal, '\\' ->
          iter source len Escaping acc bchar (i+1)
       | Normal, c when is_blank c ->
          iter source len (Blank (String.make 1 c)) acc bchar (i+1)
       | Normal, c ->
          let acc = (Tex c) :: acc in
          iter source len Normal acc bchar (i+1)
       | Command com, c ->
          if is_com_char c then
            iter source len (Command (com^(String.make 1 c))) acc bchar (i+1)
          else
            iter source len Normal ((Tex_command com) :: acc) bchar i
  in
  fun source ->
    let len = String.length source in
    let (l, _) = iter source len Normal [] None 0 in
    l
;;

(*type tex_eval = eval_tokens -> token list -> (tree list * token list)*)

let rec get_args com eval_tokens n = function
    [] when n > 0 ->
      let msg = "Command \\"^com^": missing argument" in
      failwith msg
  | ((Tex_blank _) :: q) as l ->
     begin
       match get_args com eval_tokens n q with
         [], _ -> ([], l)
       | x -> x
     end
  | h :: q when n > 0 ->
    let h = eval_tokens [h] in
    let (next, q2) = get_args com eval_tokens (n-1) q in
    (h :: next, q2)
  | l -> ([], l)
;;

let rec get_token_args com n = function
    [] when n > 0 ->
      let msg = "get_token_args: Command \\"^com^": missing argument" in
      failwith msg
  | ((Tex_blank _) :: q) as l ->
     begin
       match get_token_args com n q with
         [], _ -> ([], l)
       | x -> x
     end
  | h :: q when n > 0 ->
    let (next, q2) = get_token_args com (n-1) q in
    (h :: next, q2)
  | l -> ([], l)

let count_newlines =
  let rec iter s len acc i =
    if i >= len then
      acc
    else
      match s.[i] with
        '\n' -> iter s len (acc+1) (i+1)
      | _ -> iter s len acc (i+1)
   in
   fun s ->
     let len = String.length s in
     iter s len 0 0
;;

let rec blocks_of_tokens map tokens =
  let rec add_chars b = function
  | (Tex c) :: q ->
      Buffer.add_char b c;
      add_chars b q
  | ((Tex_blank s) :: q) as l ->
    if count_newlines s >= 2 then
      l
    else
      (
       Buffer.add_string b s;
       add_chars b q
      )
  | l -> l
  in
  let rec iter acc = function
    [] -> List.rev acc
  |(Tex_math1 s) :: q ->
     let s = "$"^s^"$" in
     let acc = (Source s) :: acc in
     iter acc q
  | (Tex_math2 s) :: q ->
     let s = "\\["^s^"\\]" in
     let acc = (Source s) :: acc in
     iter acc q
  | (Tex_blank s) :: q when count_newlines s >= 2 ->
     let acc = (Block (block ("latex", "p") [])) :: acc in
     iter acc q
  | ((Tex_blank _) :: _ | (Tex _) :: _) as l ->
     let b = Buffer.create 256 in
     (*prerr_endline (string_of_token_list l);*)
     let rest = add_chars b l in
     (*prerr_endline (Printf.sprintf "blank|tex => %S" (Buffer.contents b));*)
     iter ((Source (Buffer.contents b)) :: acc) rest
  | Tex_dbs :: q ->
     iter ((Block (block ("","latexnewline") [])) :: acc) q
  | (Tex_command name) :: q ->
     command acc name q
  | (Tex_block (c, subs)) :: q ->
     let subs = iter [] subs in
     let xmls =
       match c with
         '{' -> subs
       | c ->
         let b = block ("", String.make 1 c) subs in
         [ Block b ]
     in
     iter ((List.rev xmls) @ acc) q
  and command acc name tokens =
    match try Some(SMap.find name map) with Not_found -> None with
     None -> iter ((Source ("\\"^name^"{}")) :: acc) tokens
   | Some f ->
       let (res, tokens) = f name (blocks_of_tokens map) tokens in
       iter ((List.rev res) @ acc) tokens
  in
  iter [] tokens
;;

let string_tree = function
  [Source s] -> s
| t ->
  let msg = "Not a simple string: "^(string_of_tree_list t) in
  failwith msg
;;

let rec read_id acc = function
  | [] -> acc
  | (Tex c) :: q -> read_id (acc^(String.make 1 c)) q
  | _ -> failwith ("Invalid label: "^acc^"?")
;;

let rec get_label acc = function
    [] -> (None, List.rev acc)
  | Tex_command "label" :: Tex_block ('{', l) :: q ->
     let id = read_id "" l in
     (Some id, (List.rev acc) @ q)
  | x :: q -> get_label (x :: acc) q
;;

let gen_equation_contents ?id tokens =
  let (id, tokens) =
    match id with
      Some id -> Some id, tokens
    | None -> get_label [] tokens
  in
  let math = Source (string_of_token_list tokens) in
  [ Block (block ?id ("math","equation") [ math ])]
;;

let gen_eqnarray_contents tokens =
  let rec cut_by_newline acc cur = function
    [] ->
      (match cur with
        [] -> List.rev acc
      | _ -> List.rev ((List.rev cur) :: acc)
      )
  | Tex_dbs :: q ->
      (match cur with
         [] -> cut_by_newline acc cur q
       | _ ->
         let acc = (List.rev cur) :: acc in
         cut_by_newline acc [] q
       )
  | x :: q -> cut_by_newline acc (x::cur) q
  in
  let lines = cut_by_newline [] [] tokens in
  let cut_line line =
    let s = string_of_token_list line in
    let l = Stog_misc.split_string s ['&'] in
    List.map tokenize l
  in
  let math ?id tokens =
    match id with
      Some id -> gen_equation_contents ~id tokens
    | None ->
        match string_of_token_list tokens with
          "" -> [Source "" ]
        | s -> [ Source ("$" ^ s ^ "$") ]
  in
  let rec f_col ?id acc = function
    [] -> acc
  | tokens :: q ->
    let subs = math ?id tokens in
    let acc = (Block (block ("","td") subs)) :: acc in
    f_col acc q
  in
  let td cls subs =
    let atts = Xtmpl.atts_one ("","class") [Xtmpl.D cls] in
    let b = block ~atts ("","td") subs in
    Block b
  in
  let f_cols ?id = function
    [ c1 ; c2 ; c3 ] ->
      [ td "right" (math c1) ;
        td "center" (math c2) ;
        td "left" (math ?id c3) ;
    ]
  | l -> f_col ?id [] (List.rev l)
  in
  let f_line line =
    let (id,line) = get_label [] line in
    let cols = cut_line line in
    let tr = block ("","tr") (f_cols ?id cols) in
    Block tr
  in
  List.map f_line lines
;;


let mk_one_arg_fun name tag =
  fun eval tokens ->
    let (arg, rest) = get_args name eval 1 tokens in
    ([Block (block tag (List.hd arg))], rest)
;;

let mk_ignore_opt f x name eval tokens =
  let (arg, rest) = get_token_args name 1 tokens in
  match arg with
    [ Tex_block ('[',_) ] ->
    f name x eval rest
  | _ ->
    f name x eval tokens
;;

let mk_const_fun n res =
  match n with
    0 -> (fun name eval tokens -> (res, tokens))
  | _ ->
    (fun name eval tokens ->
      let (_, rest) = get_args name eval n tokens in
      (res, rest)
    )
;;

let fun_emph com = mk_one_arg_fun com ("","em");;
let fun_bf com = mk_one_arg_fun com ("","strong");;
let fun_texttt com = mk_one_arg_fun com ("","code");;
let fun_superscript com = mk_one_arg_fun com ("","sup");;
let fun_arobas = mk_const_fun 0 [Source " "];;
let fun_caption = mk_ignore_opt mk_one_arg_fun ("","legend");;
let fun_item =
  let f name _ eval tokens = ([Block (block ("latex","li") [])], tokens) in
  mk_ignore_opt f ()

let fun_oe = mk_const_fun 0 [Source "œ"];;

let fun_numprint com = mk_one_arg_fun com ("","span");;

let fun_scalebox com eval tokens =
  let (args,rest) = get_args com eval 2 tokens in
  match args with
    [_ ; a] -> (a, rest)
  | _ -> assert false
;;

let fun_ref com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let label = "#"^(string_tree (List.hd arg)) in
  let atts = Xtmpl.atts_one ("", "href") [Xtmpl.D label] in
  ([Block (block ~atts ("", Stog_tags.elt) [])], rest)
;;

let fun_cite com (eval : token list -> tree list) tokens =
  let (arg, rest) = get_token_args com 1 tokens in
  let (opt, arg, rest) =
    match arg with
      [ Tex_block ('[',b) ] ->
        let b = eval b in
        let (arg, rest) = get_args com eval 1 rest in
        (Some b, arg, rest)
    | _ -> (None, [eval arg], rest)
  in
  let id = string_tree (List.hd arg) in
  let atts = Xtmpl.atts_one ("", "href") [Xtmpl.D id] in
  let res =
    [ Block (block ~atts ("", "cite") []) ] @
    (match opt with
      None -> []
    | Some b -> Source " (" :: b @ [ Source ")" ]
    )
  in
  (res, rest)

let remove_end_star s =
  let len = String.length s in
  if len > 0 && s.[len-1] = '*' then String.sub s 0 (len-1) else s
;;

let fun_section com eval tokens =
  let (arg, rest) =
    let (opt, rest) = get_token_args com 1 tokens in
    match opt with
      [ Tex_block ('[',_) ] -> get_args com eval 1 rest
    | _ -> ([eval opt], rest)
  in
  let tag = remove_end_star com in
  ([Block (block ~title: (List.hd arg) ("latex", tag) [])], rest)
;;

let search_end_ com eval tokens =
  let rec iter acc = function
    [] -> None
  | ((Tex_command "end") as x) :: q ->
    let (arg,rest) = get_args "end" eval 1 q in
    let s = string_tree (List.hd arg) in
    if s = com then
      (
       prerr_endline ("search_end_ com="^com^" found");
       Some ((List.rev acc), rest)
      )
    else
      iter (x :: acc) q
  | x :: q ->
     iter (x :: acc) q
  in
  iter [] tokens
;;

let fun_begin com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let (title, rest) =
    let (opt, rest2) = get_token_args com 1 rest in
    match opt with
      [ Tex_block ('[',b) ] ->
        let opt = eval b in
        (Some opt, rest2)
    | _ -> (None, rest)
  in
  let env = string_tree (List.hd arg) in
  match env with
    "eqnarray" | "eqnarray*"
  | "equation" | "equation*" ->
      begin
        match search_end_ env eval rest with
          None -> failwith ("Could not find \\end{"^env^"}")
        | Some (subs, rest) ->
           let tag = remove_end_star env in
           let contents =
             match tag with
               "eqnarray" ->
                 let subs = gen_eqnarray_contents subs in
                 let b = block ("math",tag) ?title subs in
                 [ Block b ]
             | "equation" -> gen_equation_contents subs
             | _ -> assert false
           in
           (contents, rest)
      end
  | "picture" | "tikzpicture" ->
      begin
        match search_end_ env eval rest with
          None -> failwith ("Could not find \\end{"^env^"}")
        | Some (subs, rest) ->
          let contents =
            let s = string_of_token_list subs in
            let s = "\\begin{"^env^"}"^s^"\\end{"^env^"}" in
            let b = block ("","latex") ?title [Source s] in
            [ Block b ]
          in
          (contents, rest)
      end
  | "asy" ->
      begin
         match search_end_ env eval rest with
           None -> failwith ("Could not find \\end{"^env^"}")
         | Some (subs, rest) ->
             let s = string_of_token_list subs in
             let b = block ("","asy") [Source s] in
             ([ Block b ], rest)
      end
  | _ ->
    ([Block (block ("begin", env) ?title [])], rest)
;;

let fun_end com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let env = string_tree (List.hd arg) in
  ([Block (block ("end", env) [])], rest)
;;

let fun_label com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let id = string_tree (List.hd arg) in
  ([Block (block ~id ("", "label") [])], rest)
;;

let funs sectionning =
  let dummy0 =
    [ "smallskip" ; "medskip" ; "bigskip" ; "sc" ; "newpage" ;
      "frontmatter" ; "mainmatter";
      "dominitoc" ; "tableofcontents" ; "minitoc" ;
      "Huge" ; "huge" ; "Large" ; "large" ;  ]
  in
  let dummy1 =
    [ "vspace" ; "pagestyle" ; "title" ; "date" ; "author" ;
      "printbibliography" ;
    ]
  in
  let funs =
    (List.fold_left
      (fun acc com -> (com, fun_section) :: (com^"*", fun_section) :: acc)
       [] (sectionning)
    ) @
    (List.map (fun com -> (com, mk_const_fun 0 [])) dummy0) @
    (List.map (fun com -> (com, mk_const_fun 1 [])) dummy1) @
    [
      "emph", fun_emph ;
      "bf", fun_bf ;
      "textbf", fun_bf ;
      "texttt", fun_texttt ;
      "textsuperscript", fun_superscript ;
      "@", fun_arobas ;
      "ref", fun_ref ;
      "eqref", fun_ref ;
      "begin", fun_begin ;
      "end", fun_end ;
      "dots", mk_const_fun 0 [Source "..."] ;
      "label", fun_label ;
      "caption", fun_caption ;
      "item", fun_item ;
      "scalebox", fun_scalebox ;
      "cite", fun_cite ;
      "textcite", fun_cite ;
      "footfullcite", fun_cite ;
      "oe", fun_oe ;
      "numprint", fun_numprint ;
      ]
  in
  List.fold_left
    (fun acc (name,f) -> SMap.add name f acc)
    SMap.empty
    funs
;;

let flatten_blocks =
  let rec iter tags acc = function
  | [] -> List.rev acc
  | ((Source _) as x) :: q -> iter tags (x::acc) q
  | (Block b) :: q ->
    if List.mem b.tag tags then
      iter tags acc (b.subs @ q)
    else
      (
       let b = { b with subs = iter tags [] b.subs } in
       iter tags ((Block b) :: acc) q
      )
  in
  fun tags trees -> iter tags [] trees
;;

let mk_envs =
(*
  let rec search_end tag acc = function
    [] -> None
  | ((Source _) as x) :: q -> search_end tag (x::acc) q
  | ((Block b) as x) :: q ->
     if b.tag = tag then
       Some (List.rev acc, q)
     else
       search_end tag (x::acc) q
  in
*)
  let rec iter map acc stack l =
    match l, stack with
    | [], [] -> List.rev acc
    | [], (com,_,_) :: _ -> failwith ("Missing end:"^com)
    | (Block { tag = ("end", command) }) :: q, [] ->
         let msg = "Unexpected end:"^command in
         failwith msg
    | (Block { tag = ("end", command) }) :: q, (c,_,_) :: _ when c <> command ->
         let msg = "Expected end:"^c^" but found end:"^command in
         failwith msg
    | (Block { tag = ("end", command) }) :: q, (_,b,old_acc) :: stack ->
         prerr_endline ("end:"^command);
         let tag = try SMap.find command map with Not_found -> ("",command) in
         let b = { b with tag ; subs = List.rev acc } in
         iter map ((Block b) :: old_acc) stack q

    | (Block ({ tag = ("begin", command) } as b)) :: q, stack ->
        prerr_endline ("begin:"^command);
        iter map [] ((command,b,acc)::stack) q
    | (Block b) :: q, stack ->
        let subs = iter map [] [] b.subs in
        let b = Block { b with subs } in
        iter map (b :: acc) stack q
(*
        match search_end ("end",command) [] q with
        | None -> failwith ("Could not find end:"^command)
        | Some (l,rest) ->
            let l = iter [] l in
            let b = { b with tag = ("",command) ; subs = l } in
            iter ((Block b) :: acc) rest
      end
*)
    | x :: q, stack -> iter map (x :: acc) stack q
  in
  fun map -> iter map [] []
;;

let mk_sections =
  let rec search_end tag stop acc = function
    [] -> None
  | ((Source _) as x) :: q -> search_end tag stop (x::acc) q
  | ((Block b) as x) :: q ->
     if b.tag = tag then
       Some (List.rev acc, x :: q)
     else
       if stop b.tag then
         Some (List.rev acc, x :: q)
       else
         search_end tag stop (x::acc) q
  in
  let rec iter stop command acc = function
    | [] -> List.rev acc
    | (Block ({ tag = ("latex", c) } as b)) :: q when c = command ->
        let (subs, q) =
          match search_end ("latex",command) stop [] q with
          | None -> (q, [])
          | Some (l,rest) -> (l, rest)
        in
        let l = iter stop command [] subs in
        let b = { b with tag = ("",command) ; subs = l } in
        let acc = match b.subs with [] -> acc | _ -> (Block b) :: acc in
        iter stop command acc q
    | (Block b) :: q ->
        let subs = iter stop command [] b.subs in
        let b = Block { b with subs } in
        iter stop command (b :: acc) q
    | x :: q -> iter stop command (x :: acc) q
  in
  fun ?(stop=fun _ -> false) commands body ->
    List.fold_left
      (fun acc com -> iter stop com [] acc)
      body
      commands
;;

let mk_pars sectionning body =
  let set =
     List.fold_left (fun acc s -> SSSet.add ("",s) acc)
       SSSet.empty
       sectionning
  in
  let stop = function
    ("begin",_) | ("end", _) | ("math", _)
  | ("", "eqnarray") -> true
  | t -> SSSet.mem t set
  in
  let body = mk_sections ~stop ["p"] body in
  let stop = function
    ("end","enumerate") | ("end", "itemize")
  | ("begin","enumerate") | ("begin", "itemize") -> true
  | t -> SSSet.mem t set
  in
  mk_sections ~stop ["li"] body
;;

let traversable_tags_for_ids =
  List.fold_left
    (fun set tag -> SSSet.add tag set)
    SSSet.empty
    [
      ("","legend") ;
      ("","center") ;(* FIXME*)
    ]
;;

let add_ids =
  let rec find_label acc = function
    [] -> None
  | (Block { tag = ("", "label") ; id = Some id}) :: q ->
     prerr_endline ("found id="^id);
     Some (id, (List.rev acc) @ q)
  | ((Block { tag = ("","[")}) as x) :: q ->
     find_label (x::acc) q
  | ((Block { tag = ("","latexnewline")}) as x) :: q ->
     find_label (x::acc) q
  | (Block ({ tag } as b)) :: q when SSSet.mem tag traversable_tags_for_ids ->
     prerr_endline ("traversing block "^(fst tag)^":"^(snd tag));
     begin
       match find_label [] b.subs with
       | Some (id, l) ->
            let b = { b with subs = l } in
            let acc = List.rev ((Block b) :: acc) in
            Some (id, acc @ q)
       | None ->
          (* try looking in reverse order, for a \label at the end of block,
             for example in figure *)
            match find_label [] (List.rev b.subs) with
            | None -> find_label ((Block b) :: acc) q
            | Some (id, l) ->
                let b = { b with subs = List.rev l } in
                let acc = List.rev ((Block b) :: acc) in
                Some (id, acc @ q)
     end
  | ((Source s) as x) :: q when Stog_misc.strip_string s = "" ->
     find_label (x :: acc) q
  | x :: q -> None
  in
  let rec iter acc = function
    | [] -> List.rev acc
    | ((Source _) as x) :: q -> iter (x::acc) q
    | (Block b) :: q ->
        let (id, subs) =
          match find_label [] b.subs with
          | Some (id, subs) -> (Some id, subs)
          | None ->
            (* try looking in reverse order, for a \label at the end of block,
               for example in figure *)
              match find_label [] (List.rev b.subs) with
                Some (id, subs) -> (Some id, List.rev subs)
              | None -> (b.id, b.subs)
        in
        let subs = iter [] subs in
        let b = Block { b with id ; subs } in
        iter (b :: acc) q
  in
  iter []
;;

let rec map_block map = function
  (Source _) as x -> [x]
| Block b ->
    let b = { b with subs = map_blocks map b.subs } in
    match
      try Some (SSMap.find b.tag map)
      with Not_found -> None
    with
        None -> [Block b]
      | Some f -> f b

and map_blocks map l = List.flatten (List.map (map_block map) l);;

let to_xml =
  let rec iter = function
    Source s -> Xtmpl.D s
  | Block ({ tag = ("","[") } as b) ->
     let subs = (Xtmpl.D "[") :: (List.map iter b.subs) @ [ Xtmpl.D "]"] in
     Xtmpl.E (("","span"), Xtmpl.atts_empty, subs)
  | Block b ->
     let atts =
        (match b.title with
          [] -> []
        | t when b.tag = ("", "figure") -> []
          (* hack: do not use figure option as title, because optional arg
            is used for position *)
        | t -> [("","title"), (List.map iter t)]
        ) @
        (match b.id with
           None -> []
         | Some id -> [("","id"), [ Xtmpl.D id ] ]
        )
     in
     let atts = Xtmpl.atts_of_list ~atts: b.atts atts in
     Xtmpl.E (b.tag, atts, List.map iter b.subs)
  in
  fun l -> List.map iter l
;;

let rec resolve_includes com tex_file s =
  let dir = Filename.dirname tex_file in
  let re_include = Str.regexp ("\\\\"^com^"{\\([^}]+\\)}") in
  let f s =
    let filename = Str.matched_group 1 s in
    let filename =
      if Filename.is_relative filename then
        Filename.concat dir filename
      else
        filename
    in
    let s = Stog_misc.string_of_file filename in
    resolve_includes com filename s
  in
  Str.global_substitute re_include f s
;;

let cut_with_stog_directives source =
  let re_open = Str.regexp "^%<\\([a-z]+\\)>" in
  let re_close name = Str.regexp ("^%</" ^ name ^ ">") in
  let len = String.length source in
  let rec iter acc pos =
    let p =
      try Some (Str.search_forward re_open source pos)
      with Not_found ->
         prerr_endline "no more %< >";
         None
    in
    match p with
      None ->
        let s = String.sub source pos (len - pos) in
        List.rev ((None, s) :: acc)
    | Some p ->
        let s = String.sub source pos (p - pos) in
        let acc = (None, s) :: acc in
        let matched = Str.matched_string source in
        let name = Str.matched_group 1 source in
        prerr_endline ("found %<"^name^">");
        let p = p + String.length matched in
        let p_end =
          try Str.search_forward (re_close name) source p
          with Not_found ->
            let msg = "Missing %</"^name^">" in
            failwith msg
        in
        let s = String.sub source p (p_end - p) in
        let p = p_end + String.length (Str.matched_string source) in
        iter ((Some name, s) :: acc) p
  in
  iter [] 0

;;

let mk_preambule s =
  let lines = Stog_misc.split_string s [ '\n' ; '\r' ] in
  let preambule =
    match lines with
      [] -> assert false
    | _ :: q -> String.concat "\n" q
  in
  cut_with_stog_directives preambule
;;

let string_of_stog_directives ?(tags=[]) ?(notags=[]) p =
  let pred =
    match tags with
      [] -> (fun _ -> true)
    | l -> (fun (tag, _) -> List.mem tag l)
  in
  let nopred =
    match notags with
      [] -> (fun _ -> true)
    | l -> (fun (tag, _) -> not (List.mem tag l))
  in
  let pred s = pred s && nopred s in
  let sections = List.filter pred p in
  String.concat "\n" (List.map snd sections)
;;

let env_map =
  let l = [
    "equation", ("math", "equation") ;
    "eqnarray", ("math", "eqnarray") ;
    "align", ("math", "eqnarray") ;
    "theo", ("math", "theorem") ;
    "lemma", ("math", "lemma") ;
    "prop", ("math", "prop") ;
    "rem", ("math", "remark") ;
    "proof", ("math", "proof") ;
    "pte", ("math", "property") ;
    "itemize", ("", "ul") ;
    "enumerate", ("", "ol") ;
    "defi", ("math", "definition") ;
    "cor", ("math", "corollary") ;
  ]
  in
  List.fold_left
    (fun acc (s, tag) -> SMap.add s tag (SMap.add (s^"*") tag acc)) SMap.empty l
;;

let block_map =
  let l =
    [ ("","legend"),
      (fun b ->
        [ Block { b with
             tag = ("","div") ;
             atts = Xtmpl.atts_one ~atts: b.atts ("","class") [Xtmpl.D "legend"] ;
            }
        ]
       ) ;
    ]
  in
  List.fold_left
    (fun acc (tag, f) -> SSMap.add tag f acc) SSMap.empty l
;;

let parse sectionning environments tex_file =
  let source = Stog_misc.string_of_file tex_file in
  let source = remove_comments source in
  let source = resolve_includes "include" tex_file source in
  let source = resolve_includes "input" tex_file source in
  let source = remove_comments source in
  let source = replace_strings source in

  let (preambule, body) =
    let (preambule, _, s) = str_cut begin_document re_begin_document source in
    let (body, _, _) = str_cut end_document re_end_document s in
    let body =
      let l = cut_with_stog_directives body in
      string_of_stog_directives ~notags: [Some "ignore"] l
    in
    let preambule = mk_preambule preambule in
    (preambule, body)
  in
  let tex = { preambule ; body = [Source body] } in
  (*let tex = cut_sectionning sectionning tex in*)
  (*let body = (*cut_envs_in_tree_list env_map*) tex.body in*)
  let tokens = tokenize body in
  let body = blocks_of_tokens (funs sectionning) tokens in
  let body = flatten_blocks
    [("begin","refsection") ; ("end","refsection")]
    body
  in
  let body = mk_sections sectionning body in
  let body = mk_pars sectionning body in
  let body = mk_envs env_map body in
  let body = add_ids body in
  let body = map_blocks block_map body in
  let tex = { tex with body } in
  tex
;;

let prefix = ref None;;
let sectionning = ref [ "section" ; "subsection" ; "subsubsection" ];;
let envs = ref [ "proof"; "lemma" ; "proposition" ; "figure" ; "theorem"];;

let options =
  [ "-p", Arg.String (fun s -> prefix := Some s),
    "<s> use <s> as prefix for generated files, instead of original file's basename" ;

    "-s", Arg.String
      (fun s -> sectionning := List.map
         Stog_misc.strip_string (Stog_misc.split_string s [','])),
    "... specify sectionning commands, separated by commas" ;

    "-e", Arg.String
      (fun s -> envs := List.map
         Stog_misc.strip_string (Stog_misc.split_string s [','])),
    "... specify environments, separated by commas" ;
  ];;

let usage = Sys.argv.(0) ^" [options] file.tex\nwhere options are:";;

let main () =
  let args = ref [] in
  try
    let options = Arg.align options in
    Arg.parse options (fun s -> args := s :: !args) usage;
    match !args with
      [] | _ :: _ :: _ ->
        failwith (Arg.usage_string options usage)
    | [tex_file] ->
        let tex = parse !sectionning !envs tex_file in
        (*
        match tex.body with
          [ Source s ] -> print_endline s
        | l -> print_endline (string_of_tree_list l)
        *)
        let prefix =
          match !prefix with
            None -> Filename.basename (Filename.chop_extension tex_file)
          | Some s -> s
        in
        let mathjax_file = prefix^"_mathjax.tex" in
        let latex_file = prefix^"_latex.tex" in
        let xml_file = prefix^"_body.xml" in

        Stog_misc.file_of_string ~file: mathjax_file
          (string_of_stog_directives
            ~tags: [None ; Some "mathjax"] ~notags: [Some "ignore"] tex.preambule) ;
        Stog_misc.file_of_string ~file: latex_file
          (string_of_stog_directives
            ~notags: [Some "mathjax" ; Some "ignore"] tex.preambule) ;

        Stog_misc.file_of_string ~file: xml_file
          (Xtmpl.string_of_xml (Xtmpl.E (("","dummy_"),Xtmpl.atts_empty, to_xml tex.body)));
  with
    Failure msg -> prerr_endline msg ; exit 1
;;

let () = main ();;