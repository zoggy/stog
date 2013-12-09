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
    title : string option;
    id : string option ;
    subs : tree list ;
  }

let string_of_tag = function
  ("",s) -> s
| (p,s) -> p^":"^s
;;

let rec string_of_tree = function
  Source s -> "<source>"^s^"</source>"
| Block b ->
  "<"^(string_of_tag b.tag)^" title="^(Stog_misc.string_of_opt b.title)^" id="^(match b.id with None -> "_" | Some s -> s)^">\n"^
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

let mk_blocks com source =
  let re_com = Str.regexp ("\\\\"^com^"\\*?{\\([^}]+\\)}\n?\\(\\\\label{\\([^}]+\\)}\\)?") in
  let len = String.length source in
  let rec iter acc ?title ?id start =
    let p_opt =
      try Some (Str.search_forward re_com source start)
      with Not_found -> None
    in
    match p_opt with
      None ->
        begin
          match title with
           None -> (Source (String.sub source start (len - start))) :: acc
         | Some t ->
            let body = Source (String.sub source start (len - start)) in
            let b = { tag = ("", com) ; title = t ; id = id ; subs = [ body ] } in
            (Block b) :: acc
        end
    | Some stop ->
        let acc =
          begin
            match title with
             None -> (Source (String.sub source start (stop - start))) :: acc
           | Some t ->
              let body = Source (String.sub source start (stop - start)) in
              let b = { tag = ("", com) ; title = t ; id = id ; subs = [ body ] } in
              (Block b) :: acc
          end
        in
        let title = Some (Str.matched_group 1 source) in
        let id = try Some (Str.matched_group 3 source) with _ -> None in
        iter acc ~title ?id (stop+(String.length (Str.matched_string source)))
  in
  List.rev (iter [] 0)
;;

let cut_sectionning =
  let rec cut body = function
    [] -> body
  | com :: q ->
    let body = List.fold_left (cut_sect com) [] body in
    cut body q

  and cut_sect com acc = function
    Block b ->
      acc @ [Block { b with subs = List.fold_left (cut_sect com) [] b.subs }]
  | Source s ->
      acc @ (mk_blocks com s)
  in
  fun sections tex ->
    { tex with body = cut tex.body sections }
;;


type ('b, 'e) com_limit =
  Begin of int * int * 'b
| End of int * int * 'e
;;
type env_limit = (string * string option * string option, string) com_limit

let next_com_limit re_open f_begin re_close f_end source pos =
  let p_begin =
    try
      let p = Str.search_forward re_open source pos in
      prerr_endline ("begin: matched_string="^(Str.matched_string source));
      Some p
    with Not_found -> None
  in
  match p_begin with
    None ->
      begin
        let p_end =
          try Some (Str.search_forward re_close source pos)
          with Not_found -> None
        in
        match p_end with
          None -> None
        | Some p ->
            let matched = Str.matched_string source in
            Some (End (p, p + String.length matched, f_end source))
      end
  | Some p_begin ->
      begin
        let begin_matched = Str.matched_string source in
        let begin_data = f_begin source in
        let p_end =
          try Some (Str.search_forward re_close source pos)
          with Not_found -> None
        in
        match p_end with
        | Some p when p < p_begin->
            let matched = Str.matched_string source in
            Some (End (p, p + String.length matched, f_end source))
        | _ ->
            Some (Begin (p_begin, p_begin + String.length begin_matched, begin_data))
      end
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
  let cur_s source cur =
    let (start,len) = cur in
    if start >= len then "" else String.sub source start len
  in
  let extend (start,len) = (start, len+1) in
  let is_com_char = function
       'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | ':' | '~'-> true
     | _ -> false
  in
  let is_blank = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
  in
  let fail source msg i =
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

type tex_com_result =
  Arity of int
| Result of Xtmpl.tree list
;;

type tex_eval = Xtmpl.tree list list -> tex_com_result

let blocks_of_tokens map tokens =
  let rec get_n n = function
    [] when n > 0 -> raise Not_found
  | h :: q when n > 0 ->
    let (next, q2) = get_n (n-1) q in
    (h :: next, q2)
  | l -> ([], l)
  in
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
     let acc = (Xtmpl.D s) :: acc in
     iter acc q
  | (Tex_math2 s) :: q ->
     let s = "\\["^s^"\\]" in
     let acc = (Xtmpl.D s) :: acc in
     iter acc q
  | ((Tex _) :: _ | (Tex_blank _) :: _) as l ->
     let b = Buffer.create 256 in
     let rest = add_chars b l in
     iter ((Xtmpl.D (Buffer.contents b)) :: acc) rest
  | (Tex_command name) :: q ->
     command acc name q
  | (Tex_block (c, subs)) :: q ->
     let subs = iter [] subs in
     let xmls =
       match c with
         '{' -> subs
       | c -> [ Xtmpl.E (("",String.make 1 c), Xtmpl.atts_empty, subs) ]
     in
     iter ((List.rev xmls) @ acc) q
  and command acc name tokens =
    match try Some(SMap.find name map) with Not_found -> None with
     None -> iter ((Xtmpl.D ("\\"^name)) :: acc) tokens
   | Some f ->
       match f [] with
         Result l -> iter ((List.rev l) @ acc) tokens
       | Arity n -> apply_fun f acc n tokens

  and apply_fun f acc arity tokens =
    let (l,q) = get_n arity tokens in
    let args = List.map (fun x -> iter [] [x]) l in
    match f args with
      Arity n -> apply_fun f acc n tokens
    | Result l -> iter ((List.rev l) @ acc) q
  in
  iter [] tokens
;;


let cut_envs =
  let re_open = Str.regexp
    "\\\\begin{\\([^}*]+\\)\\*?}\\(\\[\\([^]]+\\)\\]\\)?\n?\\(\\\\label{\\([^}]+\\)}\\)?"
  in
  let re_close = Str.regexp "\\\\end{\\([^}*]+\\)\\*?}" in
  let f_begin source =
    let name = Str.matched_group 1 source in
    let title = try Some (Str.matched_group 3 source) with _ -> None in
    let id = try Some (Str.matched_group 5 source) with _ -> None in
    (name, title, id)
  in
  let f_end source = Str.matched_group 1 source in
  let next_env_limit = next_com_limit re_open f_begin re_close f_end in
  let map_tag map tag =
    try SMap.find tag map
    with Not_found -> ("", tag)
  in
  let rec iter map source acc stack pos =
    let len = String.length source in
    match next_env_limit source pos with
      None ->
        let s = String.sub source pos (len - pos) in
        (List.rev ((Source s) :: acc), len-1)
    | Some (Begin (p_start,p_stop,(tag,title,id))) ->
         let s = String.sub source pos (p_start - pos) in
         let acc = (Source s) :: acc in
         let (subs, p_end) =
           iter map source [] (tag :: stack) p_stop
         in
         let b = { tag = map_tag map tag ; id ; title ; subs } in
         let acc = (Block b) :: acc in
         iter map source acc stack p_end
    | Some (End (p_start,p_stop,tag)) ->
         match stack with
           [] -> failwith ("too many \\end{"^tag^"} in\n"^(String.sub source pos (p_stop - pos)))
         | t :: _ when t <> tag ->
             let msg = "Expected \\end{"^t^"} but found \\end{"^tag^"} in "^
               (String.sub source pos (len -pos))
             in
             failwith msg
         | _ :: q ->
             let s = String.sub source pos (p_start - pos) in
             let subs = List.rev ((Source s) :: acc) in
             (subs, p_stop)
  in
  iter
;;

let rec gen_cut_tree f par = function
  Source s -> fst (f s [] par 0)
| Block b ->
  [ Block { b with subs = gen_cut_tree_list f par b.subs } ]

and gen_cut_tree_list f par l =
  let l = List.map (gen_cut_tree f par) l in
  List.flatten l
;;

let cut_envs_in_tree_list map =
  gen_cut_tree_list (cut_envs map) []
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

let to_xml =
  let rec iter = function
    Source s -> Xtmpl.D s
  | Block b ->
     let atts =
        (match b.title with None -> [] | Some t -> [("","title"), [ Xtmpl.D t]])@
        (match b.id with
           None -> []
         | Some id -> [("","id"), [ Xtmpl.D id ] ]
        )
     in
     let atts = Xtmpl.atts_of_list atts in
     Xtmpl.E (b.tag, atts, List.map iter b.subs)
  in
  fun l -> List.map iter l
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

let fun_emph = function
  [] -> Arity 1
| h :: _ -> Result [Xtmpl.E(("","em"),Xtmpl.atts_empty,h)]
;;

let funs =
  let funs = [
    "emph", fun_emph ;
    ]
  in
  List.fold_left
    (fun acc (name,f) -> SMap.add name f acc)
    SMap.empty
    funs
;;

let parse sectionning environments tex_file =
  let source = Stog_misc.string_of_file tex_file in
  let source = remove_comments source in
  let source = resolve_includes "include" tex_file source in
  let source = resolve_includes "input" tex_file source in
  let source = remove_comments source in
  let tokens = tokenize source in
  let xmls = blocks_of_tokens funs tokens in
  prerr_endline (Xtmpl.string_of_xmls xmls);
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
  let tex = cut_sectionning sectionning tex in
  let body = cut_envs_in_tree_list env_map tex.body in
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