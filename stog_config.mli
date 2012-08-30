(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
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

(** Reading stog config. *)

(** Version of stog. *)
val version : string

(** Contents of [.stog/config] file. *)
type t = {
  ignored : string list;
  elements : string list;
  not_elements : string list;
}

(** [config_dir dir] returns the stog configuration directory in the given directory. *)
val config_dir : string -> string

(** [config_file dir] returns the stog [config] file for a given project directory,
     that is [Filename.concat (config_dir dir) "config"].*)
val config_file : string -> string

(** [tmpl_dir dir] returns the directory containing templates, from a
  stog project directory. *)
val tmpl_dir : string -> string

(** [read_config file] returns the configuration stored in the given
  stog configuration file.
  If the file does not exists, it is created.
  The file is also written back, so that new fields automatically appear in the file.
*)
val read_config : string -> t
