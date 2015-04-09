(**************************************************************************)
(*              Stog                                                      *)
(*                                                                        *)
(*  Copyright (C) 2015 Maxence Guesdon. All rights reserved.              *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as               *)
(*  published by the Free Software Foundation; either version 2 of the    *)
(*  License.                                                              *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Library General Public License for more details.                  *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public             *)
(*  License along with this program; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA              *)
(*  02111-1307  USA                                                       *)
(*                                                                        *)
(*  As a special exception, you have permission to link this program      *)
(*  with the OCaml compiler and distribute executables, as long as you    *)
(*  follow the requirements of the GNU GPL in regard to all of the        *)
(*  software in the executable aside from the OCaml compiler.             *)
(*                                                                        *)
(*  Contact: Maxence.Guesdon@inria.fr                                     *)
(*                                                                        *)
(**************************************************************************)

(** *)

type t = Neturl.url

type url_config = { pub : t ; priv : t }

(* register ws and wss url syntaxes, using http syntax *)
let () =
  let http_url_syntax =
    try Hashtbl.find Neturl.common_url_syntax "http"
    with Not_found -> failwith "No http syntax registered in Neturl !"
  in
  Hashtbl.add Neturl.common_url_syntax "ws" http_url_syntax ;
  Hashtbl.add Neturl.common_url_syntax "wss" http_url_syntax

let to_neturl t = t
let of_neturl t = t

let of_string s =
  try Neturl.parse_url ~enable_fragment: true ~accept_8bits: true s
  with Neturl.Malformed_URL ->
    failwith (Printf.sprintf "Malformed URL %S" s)
;;
let to_string = Neturl.string_of_url;;

let path url =
  match Neturl.url_path url with
    "" :: q -> q
  | x -> x

let with_path url path =
  (* to be compliant with Neturl, path must begin with "" *)
  let path =
    match path with
    | "" :: _ -> path
    | _ -> "" :: path
  in
  Neturl.modify_url ~path url

let concat uri s =
  match s with
    "" -> uri
  | _ ->
      let uri_path = path uri in
      let path = uri_path @ [s] in
      try with_path uri path
      with e ->
          prerr_endline
            (Printf.sprintf "url_concat: uri=%s url_path=%s, s=%s"
             (to_string uri)
               (String.concat "/" (Neturl.url_path uri)) s);
          raise e
;;


let field name f url =
  try f url
  with Not_found ->
    failwith (Printf.sprintf "No %s in url %s" name (to_string url))

let scheme = field "scheme" Neturl.url_scheme
let port t =
  try field "port" Neturl.url_port t
  with e ->
    match scheme t with
    | exception _ -> raise e
    | "http" | "ws" -> 80
    | "https" | "wss" -> 443
    | _ -> raise e

let host = field "host" Neturl.url_host


let with_fragment t fragment = Neturl.modify_url ~fragment t

let append uri p =
  let p0 =
    let p = path uri in
    match List.rev p with
    | "" :: q -> List.rev q
    | _ -> p
  in
  let path = p0 @ p in
  with_path uri path

let remove_ending_slash url =
  try
    match List.rev (Neturl.url_path url) with
    | [""] -> url
    | "" :: q -> Neturl.modify_url ~path: (List.rev q) url
    | _ -> url
  with Neturl.Malformed_URL ->
      failwith (Printf.sprintf "Could not modify path of %S"  (to_string url))

let remove ?scheme ?user ?user_param ?password
    ?host ?port ?path ?param ?query ?fragment ?other t =
    try Neturl.remove_from_url
      ?scheme ?user ?user_param ?password
        ?host ?port ?path ?param ?query ?fragment ?other t
    with Neturl.Malformed_URL ->
        failwith (Printf.sprintf "Malformed_URL: %s" (to_string t))
