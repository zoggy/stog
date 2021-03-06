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

(* An graphviz plugin.

Example of use:

    <dot src="foo.dot" outfile="foo.svg">
      dot code
    </dot>

It calls [dot -T svg -o outfile src] to create a SVG file
from the dot code.

If [outfile] attribute is given, then the given name is used to
store the result file and an <img> tag is used to display
the svg file. Else the resulting svg file is included and the
generated file is removed.

Source code is found in the file indicated with the [src] attribute
if present. Else it is the content of the [<dot>] node.

An additional attribute, [args], is handled to pass extra arguments
to the [dot] command.

The [command] attribute can be used to specify another command
than dot, for example neato.

*)

module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

let concat_code =
  let f ?loc b = function
    XR.D code -> Buffer.add_string b code.Xml.text
  | xml ->
      failwith
        (Xtmpl_xml.loc_sprintf loc
         "XML code in dot code: %s"
           (XR.to_string [xml]))
  in
  fun ?loc xmls ->
    let b = Buffer.create 256 in
    List.iter (f ?loc b) xmls;
    Buffer.contents b
;;

let fun_dot stog env ?loc atts subs =
  let code = concat_code subs in
  let (stog, path) = Stog_engine.get_path stog env in
  let (_, doc) = Stog_types.doc_by_path stog path in
  let doc_dir = Filename.dirname doc.Stog_types.doc_src in
  let command = XR.opt_att_cdata ~def: "dot" atts ("","command") in
  let typ = XR.opt_att_cdata ~def: "svg" atts ("", "type") in
  let id_prefix = XR.get_att_cdata atts ("","prefix-svg-ids") in
  let (stog, infile, finalize_src) =
    match XR.get_att_cdata atts ("","src") with
      None ->
        let f = Filename.temp_file "stog" ".dot" in
        Stog_misc.file_of_string ~file: f code ;
        (stog, f, (fun () -> try Unix.unlink f with _ -> ()))
    | Some f ->
        let f =
          if Filename.is_relative f then
            Filename.concat doc_dir f
          else f
        in
        let stog = Stog_plug.add_dep stog doc (Stog_types.File f) in
        (stog, f, fun () -> ())
  in
  try
    let (outfile, abs_outfile, inc, finalize_outfile) =
      match XR.get_att_cdata atts ("","outfile") with
        None ->
          if typ <> "svg" then
            failwith
              (Xtmpl_xml.loc_sprintf loc
               "<dot>: please specify outfile attribute if file type is not 'svg'"
              );
          let f = Filename.temp_file "stog_dot" ".svg" in
          (f, f, true, (fun () -> try Unix.unlink f with _ -> ()))
      | Some f ->
          let absf =
            if Filename.is_relative f then
              Filename.concat stog.Stog_types.stog_outdir (Filename.concat doc_dir f)
            else f
          in
          (f, absf, false, fun () -> ())
    in
    let args = XR.opt_att_cdata ~def: "" atts ("", "args") in
    Stog_misc.safe_mkdir (Filename.dirname abs_outfile);
    let com = Printf.sprintf "%s -T%s %s -o %s %s"
      (Filename.quote command) (Filename.quote typ)
        args (Filename.quote abs_outfile)
        (Filename.quote infile)
    in
    let xml =
      match Sys.command com with
        0 ->
          if inc then
            begin
              let xmldoc = XR.doc_from_file abs_outfile in
              let xml = xmldoc.Xml.elements in
              let xml =
                match id_prefix with
                  None -> xml
                | Some prefix ->
                    let xml = List.map (Stog_svg.prefix_svg_ids prefix) xml in
                    (*prerr_endline (XR.string_of_xml xml);*)
                    xml
              in
              xml
            end
          else
            begin
              let atts = XR.atts_remove ("","args")
                (XR.atts_remove ("","outfile")
                 (XR.atts_remove ("","type")
                  (XR.atts_remove ("","prefix-svg-ids")
                   (XR.atts_remove ("","src") atts)
                  )
                 )
                )
              in
              let atts = XR.atts_one ~atts
                ("","src") [ XR.cdata outfile ]
              in
              [ XR.node ("","img") ~atts [] ]
            end
      | _ ->
          Stog_msg.error ?loc ("Command failed: "^com);
          []
    in
    finalize_outfile () ;
    finalize_src ();
    (stog, xml)
  with
    Failure msg ->
      Stog_msg.error ?loc msg ;
      (stog, [])
;;

let () = Stog_plug.register_html_base_rule ("", "dot") fun_dot;;










