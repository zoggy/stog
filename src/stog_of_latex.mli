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

(** LaTeX to Stog translation. *)

module SMap : Map.S with type key = string

type param = {
  prefix : string option;
  ext_file_prefix : string;
  envs : string list;
  sectionning : string list;
  image_sizes : string SMap.t;
}

type tree = Source of string | Block of block
and block = {
  tag : Xtmpl.name;
  title : tree list;
  id : string option;
  subs : tree list;
  atts : Xtmpl.attributes;
}

type preambule_section = string option * string
type preambule = preambule_section list
type tex_file = { preambule : preambule; body : tree list; }

val to_xml : tree list -> Xtmpl.tree list
val string_of_stog_directives :
  ?tags:'a list -> ?notags:'a list -> ('a * string) list -> string

val parse : param -> string -> string -> tex_file * param
