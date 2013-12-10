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
  let re = Str.regexp "^%$" in
  let f s = "" in
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
| Tex_blank of char
| Tex_math1 of string
| Tex_math2 of string

type state = Normal | Escaping | Command of string | Math1 of string | Math2 of string

let rec string_of_token = function
| Tex c
| Tex_blank c -> String.make 1 c
| Tex_block (c, l) ->
    let b = Buffer.create 256 in
    Buffer.add_string b ("|BLOCK "^(String.make 1 c)^"");
    List.iter (fun t -> Buffer.add_string b (string_of_token t)) l;
    Buffer.add_string b "}|" ;
    Buffer.contents b
| Tex_command name -> "|COMMAND "^name^"|"
| Tex_math1 s -> "$...$"
| Tex_math2 s -> "\\[...\\]"
;;

let tokenize =
  let close_of_open = function
    '{' -> '}'
  | '[' -> ']'
  | _ -> assert false
  in
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
             Normal -> (List.rev acc, -1)
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
       | Escaping, c ->
           if is_com_char c then
             iter source len (Command (String.make 1 c)) acc bchar (i+1)
           else
             iter source len Normal ((Tex c) :: acc) bchar (i+1)
       | Normal, '$' ->
           iter source len (Math1 "") acc bchar (i+1)
       | Normal, c when c = ']' || c = '}' ->
         let close =
           match c, bchar with
           | '}', Some '}' -> true
           | '}', _ ->
             List.iter (fun t -> prerr_endline (string_of_token t)) acc;
             fail source "Too many }'s" i
           | ']', Some ']' -> true
           | ']', _ -> false
           | _ -> assert false
         in
         if close then
           (List.rev acc, i)
         else
           iter source len Normal ((Tex c) :: acc) bchar (i+1)
       | Command com, c when c = ']' || c = '}' ->
           let acc = (Tex_command com) :: acc in
           iter source len Normal acc bchar i
       | Normal, '{'
       | Normal, '[' ->
          let c = source.[i] in
          let cc = close_of_open c in
          let (l, i) = iter source len Normal [] (Some cc) (i+1) in
          let acc = (Tex_block (c, l)) :: acc in
          iter source len Normal acc bchar (i+1)
       | Normal, '\\' ->
          iter source len Escaping acc bchar (i+1)
       | Normal, c ->
          let acc =
            let x = if is_blank c then Tex_blank c else Tex c in
            x :: acc
          in
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
  | (Tex_blank _) :: q -> get_args com eval_tokens n q
  | h :: q when n > 0 ->
    let h = eval_tokens [h] in
    let (next, q2) = get_args com eval_tokens (n-1) q in
    (h :: next, q2)
  | l -> ([], l)

let rec blocks_of_tokens map tokens =
  let rec add_chars b = function
    (Tex c) :: q
  | (Tex_blank c) :: q ->
    Buffer.add_char b c;
    add_chars b q
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
  | ((Tex _) :: _ | (Tex_blank _) :: _) as l ->
     let b = Buffer.create 256 in
     let rest = add_chars b l in
     iter ((Source (Buffer.contents b)) :: acc) rest
  | (Tex_command name) :: q ->
     command acc name q
  | (Tex_block (c, subs)) :: q ->
     let subs = iter [] subs in
     let xmls =
       match c with
         '{' -> subs
       | c ->
         let b = block ("",String.make 1 c) subs in
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

let mk_one_arg_fun name tag =
  fun eval tokens ->
    let (arg, rest) = get_args name eval 1 tokens in
    ([Block (block tag (List.hd arg))], rest)
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

let fun_ref com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let label = "#"^(string_tree (List.hd arg)) in
  let atts = Xtmpl.atts_one ("", "href") [Xtmpl.D label] in
  ([Block (block ~atts ("", Stog_tags.elt) [])], rest)
;;

let fun_section com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let tag =
    let len = String.length com in
    if len > 0 && com.[len-1] = '*' then String.sub com 0 (len-1) else com
  in
  ([Block (block ~title: (List.hd arg) ("latex", tag) [])], rest)
;;

let fun_begin com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let env = string_tree (List.hd arg) in
  ([Block (block ("begin", env) [])], rest)
;;

let fun_end com eval tokens =
  let (arg, rest) = get_args com eval 1 tokens in
  let env = string_tree (List.hd arg) in
  ([Block (block ("end", env) [])], rest)
;;

let funs sectionning =
  let dummy0 =
    [ "medskip" ; "bigskip" ]
  in
  let dummy1 =
    [ "vspace" ]
  in
  let funs =
    (List.fold_left
      (fun acc com -> (com, fun_section) :: (com^"*", fun_section) :: acc)
       [] sectionning
    ) @
    (List.map (fun com -> (com, mk_const_fun 0 [])) dummy0) @
    (List.map (fun com -> (com, mk_const_fun 1 [])) dummy1) @
    [
      "emph", fun_emph ;
      "bf", fun_bf ;
      "textbf", fun_bf ;
      "texttt", fun_texttt ;
      "ref", fun_ref ;
      "begin", fun_begin ;
      "end", fun_end ;
      "dots", mk_const_fun 0 [Source "..."] ;
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
  let rec iter acc stack l =
    match l, stack with
    | [], [] -> List.rev acc
    |  [], (com,_,_) :: _ -> failwith ("Missing end:"^com)
    | (Block { tag = ("end", command) }) :: q, [] ->
         let msg = "Unexpected end:"^command in
         failwith msg
    | (Block { tag = ("end", command) }) :: q, (c,_,_) :: _ when c <> command ->
         let msg = "Expected end:"^c^" but found end:"^command in
         failwith msg
    | (Block { tag = ("end", command) }) :: q, (_,b,old_acc) :: stack ->
         let b = { b with tag = ("",command) ; subs = List.rev acc } in
         iter ((Block b) :: old_acc) stack q

    | (Block ({ tag = ("begin", command) } as b)) :: q, stack ->
        iter [] ((command,b,acc)::stack) q
(*
        match search_end ("end",command) [] q with
        | None -> failwith ("Could not find end:"^command)
        | Some (l,rest) ->
            let l = iter [] l in
            let b = { b with tag = ("",command) ; subs = l } in
            iter ((Block b) :: acc) rest
      end
*)
    | x :: q, stack -> iter (x :: acc) stack q
  in
  iter [] []
;;

let mk_sections =
  let rec search_end tag acc = function
    [] -> None
  | ((Source _) as x) :: q -> search_end tag (x::acc) q
  | ((Block b) as x) :: q ->
     if b.tag = tag then
       Some (List.rev acc, x :: q)
     else
       search_end tag (x::acc) q
  in
  let rec iter command acc = function
    | [] -> List.rev acc
    | (Block ({ tag = ("latex", c) } as b)) :: q when c = command ->
        let (subs, q) =
          match search_end ("latex",command) [] q with
          | None -> (q, [])
          | Some (l,rest) -> (l, rest)
        in
        let l = iter command [] subs in
        let b = { b with tag = ("",command) ; subs = l } in
        iter command ((Block b) :: acc) q
    | x :: q -> iter command (x :: acc) q
  in
  fun commands body ->
    List.fold_left
      (fun acc com -> iter com [] acc)
      body
      commands
;;

let to_xml =
  let rec iter = function
    Source s -> Xtmpl.D s
  | Block b ->
     let atts =
        (match b.title with [] -> [] | t -> [("","title"), (List.map iter t)]) @
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
    "theo", ("math", "theorem") ;
    "lemma", ("math", "lemma") ;
    "prop", ("math", "prop") ;
    "rem", ("math", "remark") ;
    "proof", ("math", "proof") ;
  ]
  in
  List.fold_left (fun acc (s, tag) -> SMap.add s tag acc) SMap.empty l
;;



let parse sectionning environments tex_file =
  let source = Stog_misc.string_of_file tex_file in
  let source = remove_comments source in
  let source = resolve_includes "include" tex_file source in
  let source = resolve_includes "input" tex_file source in
  let source = remove_comments source in
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
  let body = mk_envs body in
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