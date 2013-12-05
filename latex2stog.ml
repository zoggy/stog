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
  let re = Str.regexp "[^\\\\]%\\([^<].*$\\)$" in
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

let re_open = Str.regexp "\\\\begin{\\([^}*]+\\)\\*?}\\(\\[\\([^]]+\\)\\]\\)?\n?\\(\\\\label{\\([^}]+\\)}\\)?" ;;
let re_close = Str.regexp "\\\\end{\\([^}*]+\\)\\*?}" ;;

type env_limit =
  Begin of int * int * string * string option * string option
| End of int * int * string
;;

let next_env_limit source pos =
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
            let name = Str.matched_group 1 source in
            Some (End (p, p + String.length matched, name))
      end
  | Some p_begin ->
      begin
        let begin_matched = Str.matched_string source in
        let begin_name = Str.matched_group 1 source in
        let title = try Some (Str.matched_group 3 source) with _ -> None in
        let id = try Some (Str.matched_group 5 source) with _ -> None in
        let p_end =
          try Some (Str.search_forward re_close source pos)
          with Not_found -> None
        in
        match p_end with
        | Some p when p < p_begin->
            let matched = Str.matched_string source in
            let name = Str.matched_group 1 source in
            Some (End (p, p + String.length matched, name))
        | _ ->
            Some (Begin (p_begin, p_begin + String.length begin_matched, begin_name, title, id))
      end
;;

let map_tag map tag =
  try SMap.find tag map
  with Not_found -> ("", tag)
;;

let rec cut_envs map source len acc stack pos =
  match next_env_limit source pos with
    None ->
      let s = String.sub source pos (len - pos) in
      (List.rev ((Source s) :: acc), len-1)
  | Some (Begin (p_start,p_stop,tag,title,id)) ->
       let s = String.sub source pos (p_start - pos) in
       let acc = (Source s) :: acc in
       let (subs, p_end) =
         cut_envs map source len [] (tag :: stack) p_stop
       in
       let b = { tag = map_tag map tag ; id ; title ; subs } in
       let acc = (Block b) :: acc in
       cut_envs map source len acc stack p_end
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
;;

(*
let rec cut_envs map source =
  let re_open = Str.regexp "^\\\\begin{\\([a-z]+\\)}.*\\(\\\\label{\\([^]]+\\)}\\)?" in
  let re_open_ name = Str.regexp ("\\\\begin{" ^ name ^ "}") in
  let re_close name = Str.regexp ("\\\\end{" ^ name ^ "}") in
  let len = String.length source in
  let rec iter map acc pos =
    let p =
      try Some (Str.search_forward re_open source pos)
      with Not_found ->
         prerr_endline "no more \\begin{...}";
         None
    in
    match p with
      None ->
        let s = String.sub source pos (len - pos) in
        List.rev ((Source s) :: acc)
    | Some p ->
        let s = String.sub source pos (p - pos) in
        let acc = (Source s) :: acc in
        let matched = Str.matched_string source in
        let name = Str.matched_group 1 source in
        let id = try Some (Str.matched_group 3 source) with _ -> None in
        prerr_endline ("found \\begin{"^name^"} id="^(Stog_misc.string_of_opt id));
        let p = p + String.length matched in
        let p_end =
          try Str.search_forward (re_close name) source p
          with Not_found ->
            let msg = "Missing \\end{"^name^"}" in
            failwith msg
        in
        let s = String.sub source p (p_end - p) in
        let p = p_end + String.length (Str.matched_string source) in
        let subs = cut_envs map s in
        let tag = try SMap.find name map with Not_found -> name in
        let b = { tag ; id ; title = None ; subs } in
        iter map ((Block b) :: acc) p
  in
  iter map [] 0
;;
*)

let rec cut_envs_in_tree map = function
  Source s -> fst (cut_envs map s (String.length s) [] [] 0)
| Block b ->
  [ Block { b with subs = cut_envs_in_tree_list map b.subs } ]

and cut_envs_in_tree_list map l =
  let l = List.map (cut_envs_in_tree map) l in
  List.flatten l
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
          (string_of_stog_directives ~tags: [None ; Some "mathjax"] tex.preambule) ;
        Stog_misc.file_of_string ~file: latex_file
          (string_of_stog_directives ~notags: [Some "mathjax"] tex.preambule) ;

        Stog_misc.file_of_string ~file: xml_file
          (Xtmpl.string_of_xml (Xtmpl.E (("","dummy_"),Xtmpl.atts_empty, to_xml tex.body)));
  with
    Failure msg -> prerr_endline msg ; exit 1
;;

let () = main ();;