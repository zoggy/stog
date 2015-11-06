(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2015 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
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

module SMap = Stog_of_latex.SMap

let prefix = ref None;;
let sectionning = ref [ "section" ; "subsection" ; "subsubsection" ];;
let envs = ref [ "proof"; "lemma" ; "proposition" ; "figure" ; "theorem"];;
let ext_file_prefix = ref "";;

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

    "--ext-file-prefix", Arg.Set_string ext_file_prefix,
      "s use s as prefix for external files (images, ...)" ;
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
        let params = {
          Stog_of_latex.prefix = !prefix ;
          ext_file_prefix = !ext_file_prefix ;
          sectionning = !sectionning ;
          envs = !envs ;
          image_sizes = SMap.empty ;
          }
        in
        let source = Stog_misc.string_of_file tex_file in
        let src_dir = Filename.dirname tex_file in
        let (tex, params) = Stog_of_latex.parse params source src_dir in
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
          (Stog_of_latex.string_of_stog_directives
            ~tags: [None ; Some "mathjax"] ~notags: [Some "ignore"] tex.Stog_of_latex.preambule) ;
        Stog_misc.file_of_string ~file: latex_file
          (Stog_of_latex.string_of_stog_directives
            ~notags: [Some "mathjax" ; Some "ignore"] tex.Stog_of_latex.preambule) ;

        Stog_misc.file_of_string ~file: xml_file
          (Xtmpl_rewrite.to_string
           [Xtmpl_rewrite.node ("","dummy_")
            (Stog_of_latex.to_xml tex.Stog_of_latex.body)]);
  with
    Failure msg -> prerr_endline msg ; exit 1
;;

let () = main ();;