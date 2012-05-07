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

It calls 'markdown' by default, but the attribute "command" allow to
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

let maybe_arg args key ~default =
  match Xtmpl.get_arg args key with
    | None -> default
    | Some v -> v

let fun_markdown env args subs =
  let command = maybe_arg args "command" ~default:"markdown" in
  let args = maybe_arg args "args" ~default:"" in
  let input =
    match subs with
      | [ Xtmpl.D text ] -> text
      | _ ->
        String.concat "" (List.map Xtmpl.string_of_xml subs)
  in
  let input_file = Filename.temp_file "stog" "markdown_input" in
  Stog_misc.file_of_string ~file:input_file input;
  let output_file = Filename.temp_file "stog" "markdown_output" in
  let com =
    Printf.sprintf "%s %s < %s > %s"
      command args (Filename.quote input_file) (Filename.quote output_file) in
  if Sys.command com <> 0 then begin
  end;
  let output = Stog_misc.string_of_file output_file in
  Sys.remove input_file;
  Sys.remove output_file;
  (* markdown may contain HTML portions meant to be processed by
     Xtmpl, so we re-run Xtmpl.apply here *)
  let applied_output = Xtmpl.apply env output in
  [ Xtmpl.xml_of_string applied_output ]
;;

let () = Stog_html.plugin_funs :=
  ("markdown", fun_markdown) :: !Stog_html.plugin_funs
;;










