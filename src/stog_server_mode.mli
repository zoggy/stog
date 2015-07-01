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

(** Registering a server *)

(** Default URL, modified by command line option *)
val http_url : string ref

(** Default websocket URL, modified by command line option *)
val ws_url : string ref

(** Optional public HTTP URL, modified by command line option *)
val pub_http_url : string option ref

(** Optional public websocket URL, modified by command line option *)
val pub_ws_url : string option ref

(** Multi server: handling users, sessions with editor and preview for each session.

  Single: Simple preview server.
*)
type server_mode =
    [ `Multi of string list -> unit
    | `Single of (unit -> Stog_types.stog) -> Stog_types.stog -> unit ]

val server_mode : server_mode option ref

val set_single :
  ((unit -> Stog_types.stog) -> Stog_types.stog ->
   http_url: Stog_url.url_config ->
     ws_url: Stog_url.url_config -> unit) ->
  unit

val set_multi :
  (http_url: Stog_url.url_config ->
   ws_url: Stog_url.url_config -> string list -> unit) -> unit
