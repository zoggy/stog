(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
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

(** *)

let (>>=) = Lwt.bind

let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever

let active_cons = ref [] ;;

let handle_con uri (stream, push) =
  prerr_endline "new connection";
  active_cons := (stream, push) :: !active_cons ;
  wait_forever ()
;;

let push_message msg acc (stream, push) =
  try
    push (Some msg);
    prerr_endline "msg pushed";
    Lwt.return ((stream, push) :: acc)
  with _ -> Lwt.return acc
;;

let push_message (msg : Stog_server_types.server_message) =
  (*prerr_endline ("Number of active connections: "^(string_of_int (List.length !active_cons)));*)
  prerr_endline
    ("pushing message: "^
     (match msg with
        Stog_server_types.Update _ -> "Update"
      | Stog_server_types.Errors _ -> "Errors"
     ));
  let marshalled = Marshal.to_string  msg [] in
  let hex = Stog_server_types.to_hex marshalled in
  let msg = Websocket.Frame.of_string hex in
  Lwt_list.fold_left_s (push_message msg) [] !active_cons >>=
   fun l -> active_cons := l; Lwt.return_unit
;;

let server sockaddr =
  (*
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream >>= fun frame ->
    Lwt.wrap (fun () -> push (Some frame)) >>= fun () ->
    echo_fun uri (stream, push) in
  *)
  Websocket.establish_server sockaddr handle_con
;;

let run_server host port =
  prerr_endline ("setting up websocket server on host="^host^", port="^(string_of_int port));
  Lwt_io_ext.sockaddr_of_dns host (string_of_int port) >>= fun sa ->
    Lwt.return (server sa) >>= fun _ -> wait_forever ()
;;