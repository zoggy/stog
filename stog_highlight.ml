(**************************************************************************)
(*              Stog                                                      *)
(*                                                                        *)
(*  Copyright (C) 2014 Maxence Guesdon. All rights reserved.              *)
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

(** *)

let external_highlight ~opts code =
  let code_file = Filename.temp_file "stog" "code" in
  Stog_misc.file_of_string ~file: code_file code;
  let temp_file = Filename.temp_file "stog" "highlight" in
  let com = Printf.sprintf
    "highlight -O xhtml %s -f %s > %s"
    opts (Filename.quote code_file)(Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let code = Stog_misc.string_of_file temp_file in
      Sys.remove code_file;
      Sys.remove temp_file;
      let code = Stog_misc.strip_string code in
      [Xtmpl.xml_of_string code]
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let higlo_classes =
  let keyword = function
    0 -> "hl kwa"
  | 1 -> "hl kwb"
  | 2 -> "hl kwc"
  | _ -> "hl kwd"
  in
  let symbol _ = "sym" in
  {
    Higlo.id = "" ;
    keyword ;
    lcomment = "hl slc" ;
    bcomment = "hl com" ;
    string = "hl str" ;
    text = "hl std" ;
    numeric = "hl num" ;
    directive = "hl dir" ;
    escape = "hl esc" ;
    symbol ;
    constant = "hl num" ;
  }
;;

let highlight ?lang ?opts code =
  match lang, opts with
    None, Some opts -> external_highlight ~opts code
  | None, None -> [Xtmpl.D code]
  | Some lang, Some opts ->
      let opts = opts^" --syntax="^lang in
      external_highlight ~opts code
  | Some lang, None ->
      try
        let _lexer = Higlo.get_lexer lang in
        Higlo.to_xtmpl ~classes: higlo_classes ~lang code
      with
        Higlo.Unknown_lang s ->
          Stog_msg.warning
            (Printf.sprintf "Higlo: unknown language "^lang^". Falling back to external highlight");
          let opts = " --syntax="^lang in
          external_highlight ~opts code
;;

    