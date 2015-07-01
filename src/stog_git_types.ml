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

type path = Ojs_path.t [@@deriving yojson]

type status = [`B | `M | `A | `D | `R | `C | `U | `Q | `I]
  [@@deriving yojson]

type path_status = status * status * Ojs_path.t * Ojs_path.t option [@@deriving yojson]

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

module Base : B =
  struct
    type server_msg = .. [@@deriving yojson]
    type server_msg +=
      | SError of string
      | SOk of string
      | SStatus of path_status list
      [@@deriving yojson]

    type client_msg = .. [@@deriving yojson]
    type client_msg +=
      | Commit of path list * string
      | Status
      | Rebase_from_origin
      | Push
      [@@deriving yojson]
  end

module Make_base() = struct include Base end

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


module type Default_P = sig
    include P

    type app_server_msg += SGit of string * server_msg
    type app_client_msg += Git of string * client_msg
 end

module Default(App:Ojs_types.App_msg) =
  struct
    type app_server_msg = App.app_server_msg = .. [@@deriving yojson]
    type app_client_msg = App.app_client_msg = .. [@@deriving yojson]

    include (Make_base())

    type app_server_msg += SGit of string * server_msg [@@deriving yojson]
    type app_client_msg += Git of string * client_msg [@@deriving yojson]

    let pack_server_msg id msg = SGit (id, msg)
    let unpack_server_msg = function SGit (id,msg) -> Some (id,msg) | _ -> None

    let pack_client_msg id msg = Git (id, msg)
    let unpack_client_msg = function Git (id,msg) -> Some (id,msg) | _ -> None
  end