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

(*
A markdown plugin, contributed by Gabriel Scherer.

Example of use:

    <markdown>
    # A typical markdown example

    [Markdown](http://daringfireball.net/projects/markdown/) is
    a markup language meant to be written simply in a text editor,
    with a markup that is both easy to read and write. It is later
    translated into HTML by one of the numerous `markdown`
    implementations.
    </markdown>

It uses the Omd library by default, but the attribute "command" allows to
specify a different preprocessor command (a different implementation
of markdown, or even a completely different markup langauge). The
attribute "args" allows to pass parameters that are appended to the
command name.

Example of use (markdown to LaTeX processing using Pandoc
http://johnmacfarlane.net/pandoc/ ):

  <markdown command="pandoc" args="-f markdown -t latex">
  # A typical markdown example
  ...
  </markdown>
*)

module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

let maybe_arg args key ~default =
  match XR.get_att_cdata args key with
  | None -> default
  | Some v -> v

let run_command command ?loc args input =
  let args = maybe_arg args ("", "args") ~default:"" in
  let input_file = Filename.temp_file "stog" "markdown_input" in
  Stog_misc.file_of_string ~file:input_file input;
  let output_file = Filename.temp_file "stog" "markdown_output" in
  let com =
    Printf.sprintf "%s %s < %s > %s"
      command args (Filename.quote input_file) (Filename.quote output_file)
  in
  match Sys.command com with
      0 ->
      let output = Stog_misc.string_of_file output_file in
      Sys.remove input_file;
      Sys.remove output_file;
      output
  | _ ->
      Stog_msg.error ?loc ("Command failed: "^com);
      Sys.remove input_file;
      Sys.remove output_file;
      ""

let cs ~lang code =
  let lang = match lang with "" -> None | _ -> Some lang in
  let xmls = Stog_highlight.highlight ?lang code in
  XR.to_string xmls

let rec override elt =
  let open Omd_representation in
  let module X = Xtmpl_rewrite in
  match elt with
    Html (name, atts, subs)
  | Html_block (name, atts, subs) ->
      let b = Buffer.create 256 in
      let p fmt = Printf.bprintf b fmt in
      let atts = List.map
        (function
           (a,None) -> (Xtmpl_xml.name_of_string a, [X.cdata ""])
         | (a,Some s) -> (Xtmpl_xml.name_of_string a, X.from_string s))
         atts
      in
      let atts = X.atts_of_list atts in
      p "<%s %s>" name
        (Xtmpl_xml.string_of_atts (X.atts_to_string ~xml_atts:true atts));
      Buffer.add_string b
        (Omd.to_html ~override ~pindent:false ~nl2br:false ~cs subs);
      p "</%s" name;
      Some (Buffer.contents b)
  | _ -> None

let use_omd ?loc args input =
  let md = Omd.of_string input in
  let html = Omd.to_html ~override ~pindent:false ~nl2br:false ~cs md in
  html

let fun_markdown stog env ?loc args subs =
  let input =
    match subs with
      | [ XR.D text ] -> text.Xml.text
      | _ -> XR.to_string subs
  in
  let output =
    match XR.get_att_cdata args ("","command") with
      None -> use_omd ?loc args input
    | Some command -> run_command command ?loc args input
  in
  (* markdown may contain HTML portions meant to be processed by
     XR, so we re-run XR.apply here *)
  let (stog, applied_output) = XR.apply_to_string stog env output in
  (stog, applied_output)
;;

let () = Stog_plug.register_html_base_rule ("", "markdown") fun_markdown;;










