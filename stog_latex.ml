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

(** *)

let gensym = let cpt = ref 0 in fun () -> incr cpt; !cpt;;

let cache = Hashtbl.create 111;;

let get_in_env env (prefix, s) =
  let node = [ Xtmpl.E((prefix,s),[],[]) ] in
  let node2 = Xtmpl.apply_to_xmls env node in
  if node2 = node then "" else Xtmpl.string_of_xmls node2
;;

let make_svg outdir ?(packages=[]) ?(scale=1.1) ?defs latex_code =
  let defs = match defs with None -> "" | Some s -> s^"\n" in
  try Hashtbl.find cache latex_code
  with Not_found ->
      let tex = Filename.temp_file "stog" ".tex" in
      let code = Printf.sprintf
"\\documentclass[12pt]{article}
\\pagestyle{empty}
\\usepackage[utf8x]{inputenc}
%s%s
\\begin{document}
%s
\\end{document}
"
        (String.concat ""
         (List.map (fun s -> Printf.sprintf "\\usepackage%s\n" s) packages))
         defs
        latex_code
      in
      let base = Filename.chop_extension tex in
      let dvi = base^".dvi" in
      let svg = Filename.concat outdir
        (Filename.basename (Printf.sprintf "_latex%d.svg" (gensym())))
      in
      Stog_misc.file_of_string ~file: tex code;
      let log = Filename.temp_file "stog" ".log" in
      let command = Printf.sprintf
        "(latex -output-directory=/tmp -interaction=batchmode %s > %s 2>&1) && \
         dvisvgm -e --scale=%f -M 1.5 --no-fonts %s -s 2>> %s > %s"
        (Filename.quote tex) (Filename.quote log)
          scale
        (Filename.quote dvi)
        (Filename.quote log) (Filename.quote svg)
      in
      match Sys.command command with
        0 ->
          List.iter (fun f -> try Sys.remove f with _ -> ())
          [ tex ; dvi ; log ];
          Hashtbl.add cache latex_code svg;
          svg
      | n ->
          let log = Stog_misc.string_of_file log in
          (try Sys.remove log with _ -> ());
          failwith
          (Printf.sprintf "Command failed [%d]: %s\n=== log ===\n%s\n=== tex code ===\n%s"
           n command log latex_code)
;;

let code_of_subs =
  let f b = function
    Xtmpl.D code -> Buffer.add_string b code
  | xml -> failwith (Printf.sprintf "Invalid latex code: %s" (Xtmpl.string_of_xml xml))
  in
  function
    [ Xtmpl.D code] -> code
  | subs ->
    let b = Buffer.create 256 in
    List.iter (f b) subs;
    Buffer.contents b
;;


let fun_latex stog env args subs =
  let code = code_of_subs subs in
  let packages = Xtmpl.opt_arg args ("", "packages") in
  let packages = Stog_misc.split_string packages [';'] in
  let showcode = Xtmpl.opt_arg args ("", "showcode") = "true" in
  let defs = match get_in_env env ("", "latex-defs") with "" -> None | s -> Some s in
  let scale =
    match get_in_env env ("", "latex-svg-scale") with
      "" -> None
    | s ->
        try Some (float_of_string s)
        with _ -> failwith (Printf.sprintf "Invalid latex-svg-scale %S" s)
  in
  let svg = Filename.basename (make_svg stog.Stog_types.stog_outdir ~packages ?scale ?defs code) in
  let url = Stog_types.url_concat stog.Stog_types.stog_base_url svg in
  (Xtmpl.E (("","img"),
    [("", "class"), "latex" ;
     ("", "src"), (Stog_types.string_of_url url) ;
     ("", "alt"), code ;
     ("", "title"), code],
     []) ) ::
  (match showcode with
     false -> []
   | true -> [ Xtmpl.E (("","hcode"), [("","lang"), "tex"], [Xtmpl.D code]) ]
  )
;;