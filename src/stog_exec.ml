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

type result = Ok of string | Error of string

let exec_command ?directory command =
  let command = Stog_misc.strip_string command in
  let in_dir com = match directory with
      | None -> com
      | Some d -> Printf.sprintf "cd %s && %s" (Filename.quote d) com
  in
  let temp_file = Filename.temp_file "stogexec" ".txt" in
  let com = Printf.sprintf "(%s) > %s 2>&1"
    (in_dir command) (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let output = Stog_misc.string_of_file temp_file in
      (try Sys.remove temp_file with _ -> ());
      Ok output
  | n ->
      let output = Stog_misc.string_of_file temp_file in
      (try Sys.remove temp_file with _ -> ());
      Error output

let concat_code =
  let f b = function
    Xtmpl.D code -> Buffer.add_string b code
  | xml ->
      failwith (Printf.sprintf "XML code in command: %s"
       (Xtmpl.string_of_xml xml))
  in
  fun xmls ->
    let b = Buffer.create 256 in
    List.iter (f b) xmls;
    Buffer.contents b


let commands_of_xml xmls =
  let f xml acc =
    match xml with
      Xtmpl.D _ -> acc
    | Xtmpl.E(_,_,subs) -> (concat_code subs) :: acc
  in
  List.fold_right f xmls []

let fun_exec stog env args code =
  try
    let directory =
      match Xtmpl.get_att_cdata args ("", "directory") with
        None | Some "" -> None
      | x -> x
    in
    let exc = Xtmpl.opt_att_cdata args ~def: "true" ("", "stop-on-error") = "true" in
    let prompt =
      match Xtmpl.get_att_cdata args ("", "prompt") with
        None | Some "" -> None
      | x -> x
    in
    let show_code = Xtmpl.opt_att_cdata args ~def: "true" ("", "show-code") <> "false" in
    let show_stdout = Xtmpl.opt_att_cdata args
      ~def: (if prompt <> None then "true" else "false") ("", "show-stdout") <> "false"
    in
    let in_xml_block = Xtmpl.opt_att_cdata args ~def: "true" ("", "in-xml-block") <> "false" in
    let id_opt = Xtmpl.opt_att_cdata args ("", "id") in
    let atts = Xtmpl.atts_of_list
      (match id_opt with "" -> [] | id -> [("","id"), [Xtmpl.D id]])
    in
    let list = Xtmpl.opt_att_cdata args ~def: "false" ("", "list") = "true" in
    let commands =
      if list
      then commands_of_xml code
      else [concat_code code]
    in
    let rec iter acc = function
      [] -> List.rev acc
    | command :: q ->
        let lang_file =
          let d = stog.Stog_types.stog_dir in
          Filename.concat d "sh.lang"
        in
        let opts = if Sys.file_exists lang_file then
            Some (Printf.sprintf "--config-file=%s" lang_file)
          else
            None
        in
        let code =
          if show_code then
            Stog_highlight.highlight ~lang: "sh" ?opts command
          else
            [Xtmpl.D ""]
        in
        (*prerr_endline (Printf.sprintf "execute %S" command);*)
        let (output, error) =
          match exec_command ?directory command with
            Ok output -> (output, false)
          | Error output -> (output, true)
        in
        if error && exc then
          begin
            let msg = Printf.sprintf "Exec error with command:\n%s\n%s" command output in
            failwith msg
          end;

        let acc =
          let code =
            if in_xml_block then
              begin
                let code =
                  match prompt with
                    None -> code
                  | Some str -> (Xtmpl_xhtml.span ~classes: ["command-prompt"] [Xtmpl.D str]) :: code
                in
                [ Xtmpl.E (("","div"), Xtmpl.atts_empty, code) ]
              end
            else
              code
          in
          let classes = Printf.sprintf "command-output%s"
            (if error then " command-error" else "")
          in
          let xml =
            Xtmpl.E (("","div"),
             Xtmpl.atts_one ("","class") [Xtmpl.D classes],
             [Xtmpl.D output]
            )
          in
          xml :: code @ acc
        in
        iter acc q
    in
    let xml = iter [] commands in
    if show_code || show_stdout then
      let xml =
        if in_xml_block then
          [ Xtmpl.E (("","pre"),
             Xtmpl.atts_of_list ~atts [("","class"), [Xtmpl.D "code-exec"]],
             xml)
          ]
        else
          xml
      in
      (stog, xml)
    else
      (stog, [ Xtmpl.D "" ])
  with
    e ->
      raise e
;;

