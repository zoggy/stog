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

(* An asymptote plugin.

Example of use:

    <asy file="foo.svg">
      asymptote code
    </asy>

It calls 'asy -f svg' to create a SVG file. If file attribute is
given, then the given name is used. Else the resulting svg file
is included and the generated file is removed.

The dvisvgm tool must be installed.
*)

let concat_code =
  let f b = function
    Xtmpl.D code -> Buffer.add_string b code
  | xml ->
      failwith (Printf.sprintf "XML code in Asymptote code: %s"
       (Xtmpl.string_of_xml xml))
  in
  fun xmls ->
    let b = Buffer.create 256 in
    List.iter (f b) xmls;
    Buffer.contents b
;;

let fun_asy stog env args subs =
  let code = concat_code subs in
  let (infile, finalize_src) =
    match Xtmpl.get_arg_cdata args ("","src") with
      None ->
        let f = Filename.temp_file "stog" ".asy" in
        Stog_misc.file_of_string ~file: f code ;
        (f, (fun () -> try Unix.unlink f with _ -> ()))
    | Some f -> (f, fun () -> ())
  in
  let (outfile, finalize_file) =
    match Xtmpl.get_arg_cdata args ("","file") with
      None ->
        let f = Filename.temp_file "stog" ".svg" in
        (f, (fun () -> try Unix.unlink f with _ -> ()))
    | Some f -> (f, fun () -> ())
  in
  let args = Xtmpl.opt_arg_cdata ~def: "" args ("", "args") in
  let com = Printf.sprintf "asy -f svg %s -o %s %s"
    args (Filename.quote outfile)
    (Filename.quote infile)
  in
  let xml =
    match Sys.command com with
      0 ->
        let _xml = Xtmpl.xml_of_file outfile in
        (*finalize_file () ;*)
        finalize_src ();
        let atts = Xtmpl.atts_of_list
          [ ("","src"), [ Xtmpl.D outfile ] ]
        in
        [ Xtmpl.E (("","img"), atts, []) ]
    | _ ->
        Stog_msg.error ("Command failed: "^com);
        []
  in
  (stog, xml)
;;

let () = Stog_plug.register_html_base_rule ("", "asy") fun_asy;;










