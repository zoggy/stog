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

(** Caching system. *)

module type Cache =
  sig
    type t
    val name : string
    val load : Stog_types.elt -> t -> unit
    val store : Stog_types.elt -> t
  end

val cache_file : string -> Stog_types.stog -> Stog_types.elt -> string
val register_cache : (module Cache) -> unit

val apply_loaders : Stog_types.stog -> Stog_types.elt -> unit
val apply_storers : Stog_types.stog -> Stog_types.elt -> unit

val set_elt_env : string -> Xtmpl.env -> unit

val get_cached_elements : Stog_types.stog -> (Stog_types.elt_id list * Stog_types.elt_id list)