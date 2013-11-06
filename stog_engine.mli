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

(** Rewrite engine *)

open Stog_types;;


type 'a fun_type = 'a Xtmpl.env -> 'a -> (elt_id * elt) list -> 'a
type 'a level_fun =
  | Fun_stog of stog fun_type
  | Fun_data of 'a fun_type
  | Fun_stog_data of (stog * 'a) fun_type

type 'a engine = {
      eng_data : 'a ;
      eng_levels : 'a level_fun Stog_types.Int_map.t ;
      eng_name : string ;
    }

module type Stog_engine = sig
    type data
    val engine : data engine
    type cache_data
    val cache_load : data -> elt -> cache_data -> data
    val cache_store : data -> elt -> cache_data
  end

type stog_state =
  { st_stog : stog ;
    st_engines : (module Stog_engine) list ;
  };;

(** Generate the target files, with the following steps:
  - create the output directory,
  - build the base environment from the site global attributes,
  - compute by-topic, by-keyword and by-month elements,
  - compute elements,
  - for each level, for each element, apply level functions on the element
  - output {!Stog_types.elt.elt_out} field in the destination file.
*)
val generate :
  ?use_cache: bool -> ?only_elt:string -> Stog_types.stog ->
    (module Stog_engine) list -> unit

val elt_dst : (string -> string -> string) ->
  ?encode:bool -> Stog_types.stog -> string -> Stog_types.elt -> string

val env_of_defs : ?env:'a Xtmpl.env -> Stog_types.def list -> 'a Xtmpl.env
val env_of_used_mods : Stog_types.stog ->
  ?env:'a Xtmpl.env -> Stog_types.Str_set.t -> 'a Xtmpl.env
