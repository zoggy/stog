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

(** Caching system. *)

module type Cache =
  sig
    (** The type of the information associated to each element in the cache. *)
    type t

    (** The name of the cache, used in for the directory name where
      information is cached. *)
    val name : string

    (** This function is called when an element is read from the cache. *)
    val load : Stog_types.elt -> t -> unit

    (** This function provides the information to associate to an
         element to store it in the cache. *)
    val store : Stog_types.elt -> t
  end

val cache_file : string -> Stog_types.stog -> Stog_types.elt -> string
val stog_cache_name : string

val register_cache : (module Cache) -> unit

val apply_loaders : Stog_types.stog -> Stog_types.elt -> unit
val apply_storers : Stog_types.stog -> Stog_types.elt -> unit
val output_cache_info : Stog_types.stog -> unit

val set_elt_env : Stog_types.elt -> Stog_types.stog -> Xtmpl.env -> unit
val get_cached_elements : Stog_types.stog -> Xtmpl.env -> (Stog_types.elt_id list * Stog_types.elt_id list)

