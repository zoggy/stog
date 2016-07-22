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

type t = Uri.t

module W = Ocf.Wrapper

type url_config = { pub : t; priv: t }

let of_string s =
  try Uri.of_string s
  with _ ->
    failwith (Printf.sprintf "Malformed URL %S" s)
;;
let to_string = Uri.to_string ;;

let path url =
  let l =
    match Stog_misc.split_string ~keep_empty: true (Uri.path url) ['/'] with
      "" :: q -> q
    | x -> x
  in
  List.map Uri.pct_decode l

let with_path url path =
  let path = List.map Uri.pct_encode path in
  Uri.with_path url ("/"^(String.concat "/" path))

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
             (to_string uri) (Uri.path uri) s);
          raise e
;;


let field name f url =
  match f url with
  | None ->
      failwith (Printf.sprintf "No %s in url %s" name (to_string url))
  | Some v -> v

let scheme = field "scheme" Uri.scheme
let port t =
  try field "port" Uri.port t
  with e ->
    match scheme t with
    | exception _ -> raise e
    | "http" | "ws" -> 80
    | "https" | "wss" -> 443
    | _ -> raise e

let host = field "host" Uri.host

let with_fragment = Uri.with_fragment

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
  match List.rev (path url) with
  | [""] -> url
  | "" :: q -> with_path url (List.rev q)
  | _ -> url

let wrapper = W.string_ to_string of_string

let url_config_wrapper =
  let to_j ?with_doc c =
    `Assoc ["url", wrapper.W.to_json ?with_doc c.priv ;
            "public_url", wrapper.W.to_json c.pub ]
  in
  let from_j ?def = function
    (`Assoc l) as json ->
      begin
        match try Some (List.assoc "url" l) with Not_found -> None with
          None -> Ocf.invalid_value json
        | Some priv ->
            let priv = wrapper.W.from_json priv in
            let pub =
              try wrapper.W.from_json (List.assoc "public_url" l)
              with Not_found -> priv
            in
            { pub ; priv }
      end
  | json ->
      let priv = wrapper.W.from_json json in
      { pub = priv ; priv }
  in
  W.make to_j from_j

let default_url_config url = { pub = url ; priv = url }

let remove_query t = Uri.with_query t []
