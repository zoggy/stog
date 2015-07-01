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

open Stog_url

let http_url = ref "http://localhost:8080"
let ws_url = ref "ws://localhost:8081"

let pub_http_url = ref None
let pub_ws_url = ref None

let mk_urls () =
  let url s = Stog_url.remove_ending_slash (Stog_url.of_string s) in
  let http_url =
    { pub = (match !pub_http_url with None -> url !http_url | Some s -> url s) ;
      priv = url !http_url ;
    }
  in
  let ws_url =
    { pub = (match !pub_ws_url with None -> url !ws_url | Some s -> url s) ;
      priv = url !ws_url ;
    }
  in
  (http_url, ws_url)

type server_mode = [
    `Single of (unit -> Stog_types.stog) -> Stog_types.stog -> unit
  | `Multi of string list -> unit
  ]
let server_mode = ref (None : server_mode option)

let set_single f =
  let g read_stog stog =
    let (http_url, ws_url) = mk_urls () in
    f read_stog stog ~http_url ~ws_url
  in
  server_mode := Some (`Single g)

let set_multi f =
  let g args =
    let (http_url, ws_url) = mk_urls () in
    f ~http_url ~ws_url args
  in
  server_mode := Some (`Multi g)
  