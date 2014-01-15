(**************************************************************************)
(*              Stog                                                      *)
(*                                                                        *)
(*  Copyright (C) 2012 Maxence Guesdon. All rights reserved.              *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as               *)
(*  published by the Free Software Foundation; either version 2 of the    *)
(*  License.                                                              *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Library General Public License for more details.                  *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public             *)
(*  License along with this program; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA              *)
(*  02111-1307  USA                                                       *)
(*                                                                        *)
(*  As a special exception, you have permission to link this program      *)
(*  with the OCaml compiler and distribute executables, as long as you    *)
(*  follow the requirements of the GNU GPL in regard to all of the        *)
(*  software in the executable aside from the OCaml compiler.             *)
(*                                                                        *)
(*  Contact: Maxence.Guesdon@inria.fr                                     *)
(*                                                                        *)
(**************************************************************************)

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

let concat_code =
  let f b = function
    Xtmpl.D code -> Buffer.add_string b code
  | xml ->
      failwith (Printf.sprintf "XML code in dot code: %s"
       (Xtmpl.string_of_xml xml))
  in
  fun xmls ->
    let b = Buffer.create 256 in
    List.iter (f b) xmls;
    Buffer.contents b
;;

let fun_dot stog env atts subs =
  let code = concat_code subs in
  let (stog, hid) = Stog_engine.get_hid stog env in
  let hid = Stog_types.human_id_of_string hid in
  let (_, elt) = Stog_types.elt_by_human_id stog hid in
  let elt_dir = Filename.dirname elt.Stog_types.elt_src in
  let command = Xtmpl.opt_arg_cdata ~def: "dot" atts ("","command") in
  let (infile, finalize_src) =
    match Xtmpl.get_arg_cdata atts ("","src") with
      None ->
        let f = Filename.temp_file "stog" ".dot" in
        Stog_misc.file_of_string ~file: f code ;
        (f, (fun () -> try Unix.unlink f with _ -> ()))
    | Some f ->
        let f =
          if Filename.is_relative f then
            Filename.concat elt_dir f
          else f
        in
        (f, fun () -> ())
  in
  let (outfile, abs_outfile, inc, finalize_outfile) =
    match Xtmpl.get_arg_cdata atts ("","outfile") with
      None ->
        let f = Filename.temp_file "stog" ".svg" in
        (f, f, true, (fun () -> try Unix.unlink f with _ -> ()))
    | Some f ->
        let absf =
          if Filename.is_relative f then
            Filename.concat stog.Stog_types.stog_outdir (Filename.concat elt_dir f)
          else f
        in
        (f, absf, false, fun () -> ())
  in
  let args = Xtmpl.opt_arg_cdata ~def: "" atts ("", "args") in
  let com = Printf.sprintf "%s -Tsvg %s -o %s %s"
    (Filename.quote command)
    args (Filename.quote abs_outfile)
    (Filename.quote infile)
  in
  let xml =
    match Sys.command com with
      0 ->
        let xml = Xtmpl.xml_of_file abs_outfile in
        finalize_outfile () ;
        finalize_src ();
        if inc then
          [ xml ]
        else
          begin
            let atts = Xtmpl.atts_remove ("","args")
              (Xtmpl.atts_remove ("","outfile")
               (Xtmpl.atts_remove ("","src") atts)
              )
            in
            let atts = Xtmpl.atts_one ~atts
              ("","src") [ Xtmpl.D outfile ]
            in
            [ Xtmpl.E (("","img"), atts, []) ]
          end
    | _ ->
        Stog_msg.error ("Command failed: "^com);
        []
  in
  (stog, xml)
;;

let () = Stog_plug.register_html_base_rule ("", "dot") fun_dot;;










