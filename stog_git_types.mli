(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 INRIA All rights reserved.                         *)
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

(** Git types *)

(** All paths should relative to repository root. *)
type path = Ojs_path.t [@@deriving yojson]

(** [`B] is for space (blank), [`Q] for '?', [`I] for '!' (ignored files). *)
type status = [ `A | `B | `C | `D | `I | `M | `Q | `R | `U ] [@@deriving yojson]

(** As the output of git status --porcelain -z:
  {[ [status][status] to from }] *)
type path_status = status * status * Ojs_path.t * Ojs_path.t option
 [@@deriving yojson]

module type B =
  sig
    type server_msg = .. [@@deriving yojson]
    type server_msg +=
      | SError of string
      | SOk of string
      | SStatus of path_status list

    type client_msg = .. [@@deriving yojson]
    type client_msg +=
      | Commit of path list * string
      | Status
      | Rebase_from_origin
      | Push
  end
module Base : B

module Make_base : functor () -> B

module type P =
  sig
    type app_server_msg = .. [@@deriving yojson]
    type app_client_msg = .. [@@deriving yojson]

    include B

    val pack_server_msg : string -> server_msg -> app_server_msg
    val unpack_server_msg : app_server_msg -> (string * server_msg) option
    val pack_client_msg : string -> client_msg -> app_client_msg
    val unpack_client_msg : app_client_msg -> (string * client_msg) option
  end

module type S = sig
    include P

    type app_server_msg += SGit of string * server_msg
    type app_client_msg += Git of string * client_msg
 end

module Default_P :
  functor (App : Ojs_types.App_msg) ->
    S with type app_server_msg = App.app_server_msg
       and type app_client_msg = App.app_client_msg
