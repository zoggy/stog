(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
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

let hack_cmxs = ref false;;

let create_cmxs src =
  let dst = Filename.temp_file ("stog-"^(Filename.basename src)) ".cmxs" in
  let options =
    match Filename.basename (Filename.chop_extension src) with
      "ulexing" | "cryptokit" | "pcre" -> " -linkall"
    | _ -> ""
  in
  let includes = "-I "^(Filename.quote (Filename.dirname src)) in
  let com = "ocamlopt -shared -o "^(Filename.quote dst)^" "^includes^options^" "^(Filename.quote src) in
  match Sys.command com with
    0 -> dst
  | _  -> failwith ("Command failed: "^com)
;;

let _ = Dynlink.allow_unsafe_modules true;;

let load_file file =
  Stog_msg.verbose (Printf.sprintf "Loading file %s" file);
  try Dynlink.loadfile file
  with Dynlink.Error e ->
      failwith (Dynlink.error_message e)

let loaded_files = ref [];;

let hack_load_file file =
  (* special case for cryptokit, pcre, by now, who is missing reference to C code in .cmxs *)
  match Filename.chop_extension (Filename.basename file) with
    "cryptokit" | "pcre" ->
      let cmxs = create_cmxs ((Filename.chop_extension file)^".cmxa") in
      load_file cmxs;
      Sys.remove cmxs
  | _ ->
      match Filename.check_suffix file ".cmxa" ||
        Filename.check_suffix file ".cmx"
      with
        false -> load_file file
      | true ->
          (* let's create a .cmxs from this .cmxa if not corresponding .cmxs exists *)
          let cmxs = (Filename.chop_extension file)^".cmxs" in
          if Sys.file_exists cmxs then
            load_file cmxs
          else
            (
             let cmxs = create_cmxs file in
             load_file cmxs ;
             (*Sys.remove cmxs*)
            )
;;

let check_file_has_extension file =
  try ignore(Filename.chop_extension file)
  with _ ->
    failwith ("Filename "^file^" has no extension.");
;;

let load_files =
  let load_file file =
    check_file_has_extension file ;
    if !hack_cmxs then
      hack_load_file file
    else
      load_file (Dynlink.adapt_filename file)
  in
  let f file =
    if List.mem file !loaded_files then
      Stog_msg.verbose (Printf.sprintf "Not loading already loaded file %s" file)
    else
      begin
        load_file file;
        loaded_files := file :: !loaded_files;
      end
  in
  List.iter f
;;

let files_of_packages kind pkg_names =
  let file = Filename.temp_file "stog" ".txt" in
  let com =
    Printf.sprintf "ocamlfind query %s -predicates plugin,%s -r -format %%d/%%a > %s"
      (String.concat " " (List.map Filename.quote pkg_names))
      (match kind with `Byte -> "byte" | `Native -> "native")
      (Filename.quote file)
  in
  match Sys.command com with
    0 ->
      let s = Stog_misc.string_of_file file in
      Sys.remove file;
      Stog_misc.split_string s ['\n']
  | n ->
      let msg = Printf.sprintf "Command failed (%d): %s" n com in
      failwith msg
;;

let load_packages_comma pkg_names =
  let kind = if Dynlink.is_native then `Native else `Byte in
  let pkg_names = Stog_misc.split_string pkg_names [','] in
  let files = files_of_packages kind pkg_names in
  load_files files
;;

let load_packages packages =
  List.iter load_packages_comma packages
;;

