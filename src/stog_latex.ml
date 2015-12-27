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

(** *)

module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

let gensym = let cpt = ref 0 in fun () -> incr cpt; !cpt;;

let cache = Hashtbl.create 111;;

let latex_code_use_packages l =
  String.concat ""
    (List.map (fun s -> Printf.sprintf "\\usepackage%s\n" s) l)
;;

let build_preambule ~packages ~defs ~def_files =
  Printf.sprintf "\\pagestyle{empty}\n\\usepackage[utf8]{inputenc}\n%s\n%s\n%s"
   (latex_code_use_packages packages)
    (String.concat "\n"
     (List.map (fun file -> "\\input{"^file^"}") def_files)
    )
    defs
;;

let get_latex_defs stog env =
  let (stog, def_files) =
    let (stog, xmls) = Stog_engine.get_in_env stog env ("", "latex-defs-files") in
    match xmls with
      [] ->
        (stog, [])
    | _ ->
        let s =
          match xmls with
            [XR.D s] -> s.Xml.text
          | _ ->
              let msg = "Invalid filenames: "^(XR.to_string xmls) in
              failwith msg
        in
        let files = List.map Stog_misc.strip_string
          (Stog_misc.split_string s [',' ; ';'])
        in
        let files = List.filter ((<>) "") files in
        match files with
          [] -> (stog, files)
        | _ ->
            let (stog, path) = Stog_engine.get_path stog env in
            let (_, doc) = Stog_types.doc_by_path stog path in
            let dir = Filename.dirname doc.Stog_types.doc_src in
            let f filename =
              if Filename.is_relative filename
              then Filename.concat dir filename
              else filename
            in
            (stog, List.map f files)
  in
  let (stog, defs) =
    let (stog, xmls) = Stog_engine.get_in_env stog env ("", "latex-defs") in
    let defs = match xmls with [] -> "" | _ -> XR.to_string xmls in
    (stog, defs)
  in
  (stog, defs, def_files)
;;

let make_svg outdir ?(packages=[]) ?(scale=1.1) ?(def_files=[]) ?defs latex_code =
  let defs = match defs with None -> "" | Some s -> s^"\n" in
  try Hashtbl.find cache latex_code
  with Not_found ->
      let tex = Filename.temp_file "stog" ".tex" in
      let tex_aux = Filename.chop_extension tex ^ ".aux" in
      let tex_log = Filename.chop_extension tex ^ ".log" in
      let code = Printf.sprintf
        "\\documentclass[12pt]{article}
%s
\\begin{document}
%s
\\end{document}
"
          (build_preambule ~packages ~defs ~def_files)
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
        "(latex -output-directory=%s -interaction=batchmode %s > %s 2>&1) && \
         dvisvgm -e --scale=%f -M 1.5 --no-fonts %s -s 2>> %s > %s"
          (Filename.get_temp_dir_name ())
          (Filename.quote tex) (Filename.quote log)
          scale
          (Filename.quote dvi)
          (Filename.quote log) (Filename.quote svg)
      in
      match Sys.command command with
        0 ->
          List.iter (fun f -> try Sys.remove f with _ -> ())
            [ tex ; tex_aux ; tex_log ; dvi ; log ];
          Hashtbl.add cache latex_code svg;
          svg
      | n ->
          let log = Stog_misc.string_of_file log in
          List.iter (fun f -> try Sys.remove f with _ -> ())
            [ tex_aux ; tex_log ; log ];
          failwith
            (Printf.sprintf "Command failed [%d]: %s\n=== log ===\n%s\n=== tex code ===\n%s"
             n command log latex_code)
;;

let code_of_subs =
  let f b = function
    XR.D code -> Buffer.add_string b code.Xml.text
  | xml -> failwith (Printf.sprintf "Invalid latex code: %s" (XR.to_string [xml]))
  in
  function
  | [ XR.D code] -> code.Xml.text
  | subs ->
    let b = Buffer.create 256 in
    List.iter (f b) subs;
    Buffer.contents b
;;

let get_packages stog env args =
  let (stog, s) =
    match XR.get_att_cdata args ("","packages") with
      Some s -> (stog, s)
    | None ->
        let (stog, xmls) = Stog_engine.get_in_args_or_env stog env args ("","latex-packages") in
        match xmls with
          [XR.D s] -> (stog, s.Xml.text)
        | _ -> (stog, "")
  in
  let l = List.map Stog_misc.strip_string (Stog_misc.split_string s [';']) in
  (stog, l)
;;

let fun_latex stog env ?loc args subs =
  let code = code_of_subs subs in
  let (stog, packages) = get_packages stog env args in
  let showcode = XR.opt_att_cdata args ("", "showcode") = "true" in
  let (stog, defs, def_files) = get_latex_defs stog env in
  let (stog, scale) =
    let (stog, xmls) = Stog_engine.get_in_env stog env ("", "latex-svg-scale") in
    let scale =
      match xmls with
        [] -> None
      | _ ->
          let s = XR.to_string xmls in
          try Some (float_of_string s)
          with _ -> failwith (Printf.sprintf "Invalid latex-svg-scale %S" s)
    in
    (stog, scale)
  in
  let svg = Filename.basename
    (make_svg stog.Stog_types.stog_outdir ~packages ?scale ~def_files ~defs code)
  in
  let url = Stog_url.concat stog.Stog_types.stog_base_url svg in
  let xmls =
    (XR.node ("","img")
      ~atts:
       (XR.atts_of_list
        [ ("", "class"), [XR.cdata "latex"] ;
          ("", "src"), [XR.cdata (Stog_url.to_string url) ] ;
          ("", "alt"), [XR.cdata code] ;
          ("", "title"), [XR.cdata code]
        ])
      []
    ) ::
      (match showcode with
         false -> []
       | true ->
           [ XR.node ("","hcode")
              ~atts: (XR.atts_one ("","lang") [XR.cdata "tex"])
               [XR.cdata code]
           ]
      )
  in
  (stog, xmls)
;;


let fun_latex_body stog env ?loc args subs =
  let (stog, packages) = get_packages stog env args in
  let (stog, defs, def_files) = get_latex_defs stog env in
  let code = code_of_subs subs in
  let code =
    (build_preambule ~packages ~defs ~def_files)^
      "\n\\begin{document}"^code^"\\end{document}"
  in

  let (stog, path) = Stog_engine.get_path stog env in
  let (_, doc) = Stog_types.doc_by_path stog path in
  let doc_dir = Filename.dirname doc.Stog_types.doc_src in

  let (stog, xmls) = Stog_engine.get_in_env stog env ("","sectionning") in
  let sectionning =
    match xmls with
      [XR.D { Xml.text } ] ->
        List.map Stog_misc.strip_string
          (Stog_misc.split_string text [','])
    | _ -> Stog_tags.default_sectionning
  in
  let params = {
      prefix = None ;
      ext_file_prefix = "" ;
      envs = [] ;
      Stog_of_latex.sectionning = sectionning ;
      image_sizes = Stog_of_latex.SMap.empty ;
    }
  in
  let (tex,_) = Stog_of_latex.parse params code doc_dir in
  (stog, Stog_of_latex.to_xml tex.Stog_of_latex.body)
;;
