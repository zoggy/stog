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

let version = "0.4";;

type t =
  { ignored : string list ; (** list of regexps of filenames to ignore *)
    elements : string list ; (** list of regexps for element files *)
    not_elements : string list ;
    (** list of regexps for file matching elements rules but not being  elements *)
  }
;;

module CF = Config_file;;

let config_dir dir = Filename.concat dir ".stog";;
let config_file dir = Filename.concat (config_dir dir) "config";;
let tmpl_dir dir = Filename.concat (config_dir dir) "templates";;
let cache_dir dir = Filename.concat (config_dir dir) "cache";;
let modules_dir dir = Filename.concat (config_dir dir) "modules";;

let read_config dir =
  let rc_file = config_file dir in
  let group = new CF.group in
  let o_ignored = new CF.list_cp CF.string_wrappers ~group
    ["ignored"] [ ".*\\.git" ; ".*Makefile" ; ".*tmpl$" ] "Regexps of files to ignore"
  in
  let o_elts = new CF.list_cp CF.string_wrappers ~group
    ["elements"] [ ".*\\.xml$" ; ".*\\.html$" ] "Regexps of files containing elements"
  in
  let o_not_elts = new CF.list_cp CF.string_wrappers ~group
    ["not-elements"] [ ]
    "Regexps of files matching 'elements' regexps but not containing elements"
  in
  let cfg_dir = config_dir dir in
  if not (Sys.file_exists cfg_dir) then
    begin
      Stog_msg.warning
        (Printf.sprintf "Creating inexistent configuration directory %S" cfg_dir);
      Stog_misc.safe_mkdir cfg_dir;
    end;
  group#read rc_file;
  { ignored = o_ignored#get ;
    elements = o_elts#get ;
    not_elements = o_not_elts#get ;
  }
;;




    