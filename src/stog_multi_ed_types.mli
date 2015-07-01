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

(** Types for editor server and client parts *)

module App_msg : Ojs_types.App_msg

module FT : Ojsft_types.Default_P
  with type app_server_msg = App_msg.app_server_msg
   and type app_client_msg = App_msg.app_client_msg

module ED : Ojsed_types.Default_P
  with type app_server_msg = App_msg.app_server_msg
   and type app_client_msg = App_msg.app_client_msg

module Git : Stog_git_types.Default_P
  with type app_server_msg = App_msg.app_server_msg
   and type app_client_msg = App_msg.app_client_msg

val server_msg_to_yojson : App_msg.app_server_msg -> Yojson.Safe.json
val server_msg_of_yojson :
  Yojson.Safe.json -> [ `Error of string | `Ok of App_msg.app_server_msg ]
val client_msg_to_yojson : App_msg.app_client_msg -> Yojson.Safe.json
val client_msg_of_yojson :
  Yojson.Safe.json -> [ `Error of string | `Ok of App_msg.app_client_msg ]

val ft_id : string
val ed_id : string
val ojs_msg_id : string
val bar_id : string
val gitrepo_id : string
