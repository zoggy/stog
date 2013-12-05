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

type tree =
  Source of string
| Block of block

and block = {
    tag : string ;
    title : string ;
    id : string option ;
    subs : tree list ;
  }

let rec string_of_tree = function
  Source s -> "<source>"^s^"</source>"
| Block b ->
  "<"^b.tag^" title="^b.title^" id="^(match b.id with None -> "_" | Some s -> s)^">\n"^
  (string_of_tree_list b.subs)^"\n</"^b.tag^">\n"

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

let remove_comments s =
  let re = Str.regexp "[^\\\\]%.*$" in
  let f s =
    let matched = Str.matched_string s in
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
  let re_com = Str.regexp ("\\\\"^com^"{\\([^}]+\\)}") in
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
            let b = { tag = com ; title = t ; id = id ; subs = [ body ] } in
            (Block b) :: acc
        end
    | Some stop ->
        let acc =
          begin
            match title with
             None -> (Source (String.sub source start (stop - start))) :: acc
           | Some t ->
              let body = Source (String.sub source start (stop - start)) in
              let b = { tag = com ; title = t ; id = id ; subs = [ body ] } in
              (Block b) :: acc
          end
        in
        let title = Str.matched_group 1 source in
        let id = None in
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

let cut_envs = ()

;;

let rec resolve_includes com tex_file s =
  let dir = Filename.dirname tex_file in
  let re_include = Str.regexp ("\\"^com^"{\\([^}]+\\)}") in
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
        (("","title"), [ Xtmpl.D b.title ]) ::
        (match b.id with
           None -> []
         | Some id -> [("","id"), [ Xtmpl.D id ] ]
        )
     in
     let atts = Xtmpl.atts_of_list atts in
     Xtmpl.E (("",b.tag), atts, List.map iter b.subs)
  in
  fun l -> List.map iter l
;;

let mk_preambule s =
  let lines = Stog_misc.split_string s [ '\n' ; '\r' ] in
  let preambule =
    match lines with
      [] -> assert false
    | _ :: q -> String.concat "\n" q
  in
  let re_open = Str.regexp "^%<\\([a-z]+\\)>" in
  let re_close name = Str.regexp ("^%</" ^ name ^ ">") in
  let len = String.length preambule in
  let rec iter acc pos =
    let p =
      try Some (Str.search_forward re_open preambule pos)
      with Not_found ->
         prerr_endline "no more %< >";
         None
    in
    match p with
      None ->
        let s = String.sub preambule pos (len - pos) in
        List.rev ((None, s) :: acc)
    | Some p ->
        let s = String.sub preambule pos (len - pos) in
        let acc = (None, s) :: acc in
        let matched = Str.matched_string preambule in
        let name = Str.matched_group 1 preambule in
        prerr_endline ("found %<"^name^">");
        let p = p + String.length matched in
        let p_end =
          try Str.search_forward (re_close name) preambule p
          with Not_found ->
            let msg = "Missing %</"^name^">" in
            failwith msg
        in
        let s = String.sub preambule p (p_end - p) in
        let p = p_end + String.length (Str.matched_string preambule) in
        iter ((Some name, s) :: acc) p
  in
  iter [] 0
;;

let string_of_preambule ?(tags=[]) ?(notags=[]) p =
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

let parse sectionning environments tex_file =
  let source = Stog_misc.string_of_file tex_file in
  let source = remove_comments source in
  let source = resolve_includes "include" tex_file source in
  let source = resolve_includes "input" tex_file source in
  let source = remove_comments source in
  let (preambule, body) =
    let (preambule, _, s) = str_cut begin_document re_begin_document source in
    let (body, _, _) = str_cut end_document re_end_document s in
    let preambule = mk_preambule preambule in
    (preambule, body)
  in
  let tex = { preambule ; body = [ Source body ] } in
  let tex = cut_sectionning sectionning tex in
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
          (string_of_preambule ~tags: [None ; Some "mathjax"] tex.preambule) ;
        Stog_misc.file_of_string ~file: latex_file
          (string_of_preambule ~notags: [Some "mathjax"] tex.preambule) ;

        Stog_misc.file_of_string ~file: xml_file
          (Xtmpl.string_of_xml (Xtmpl.E (("","dummy_"),Xtmpl.atts_empty, to_xml tex.body)));
  with
    Failure msg -> prerr_endline msg ; exit 1
;;

let () = main ();;