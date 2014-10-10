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

open Stog_types
open Stog_server_types

let (>>=) = Lwt.bind

let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever

let push_message active_cons msg (stream, push) =
  Lwt.catch
    (fun () ->
       push (Some msg);
       prerr_endline "msg pushed";
       active_cons := (stream, push) :: !active_cons ;
       Lwt.return_unit
    )
    (fun e -> prerr_endline ("push_message: "^(Printexc.to_string e)) ; Lwt.return_unit)
;;

let push_message active_cons ?push (msg : Stog_server_types.server_message) =
  (*prerr_endline ("Number of active connections: "^(string_of_int (List.length !active_cons)));*)
  prerr_endline
    ("pushing message: "^
     (match msg with
        Stog_server_types.Update _ -> "Update"
      | Stog_server_types.Errors (errs,warns) ->
          String.concat "\n" ("Errors:" :: errs @ ["Warnings:"] @ warns)
     ));
  let marshalled = Marshal.to_string  msg [] in
  let hex = Stog_server_types.to_hex marshalled in
  let msg = Websocket.Frame.of_string hex in
  match push with
    None ->
      let l = !active_cons in
      active_cons := [] ;
      Lwt_list.iter_s (push_message active_cons msg) l
  | Some push ->
        Lwt.return (push (Some msg))
;;

let handle_messages current_state active_cons base_path stream push =
  let f frame =
    match Websocket.Frame.opcode frame with
    | `Close ->
        prerr_endline (Printf.sprintf "A Close frame camed when there were %d connections" (List.length !active_cons));
        active_cons := List.filter (fun (_,p) -> p != push) !active_cons ;
        prerr_endline (Printf.sprintf "Now I have only %d." (List.length !active_cons));
        Lwt.return_unit
    | _ ->
        let s = Websocket.Frame.content frame in
        let len = String.length s in
        prerr_endline ("GOT MESSAGE: "^s);
        if len >= 4 && String.sub s 0 3 = "GET" then
          begin
            let path = String.sub s 4 (len - 4) in
            match !current_state with
              None -> Lwt.fail (Failure "No state yet!")
            | Some state ->
                try
                  let (_, doc) = Stog_types.doc_by_path state.Stog_server_run.stog (Stog_path.of_string path) in
                  match doc.Stog_types.doc_out with
                    None -> Lwt.return_unit
                  | Some [] -> Lwt.return_unit
                  | Some (tree :: _) ->
                      let msg = Stog_server_types.Update (path, (Stog_server_types.Update_all tree)) in
                      push_message active_cons ~push msg
                with e ->
                    Lwt.return_unit
          end
        else
          Lwt.return_unit
  in
  Lwt.catch
    (fun _ -> Lwt_stream.iter_s f stream)
    (fun _ -> Lwt.return_unit)

let handle_con current_state active_cons base_path uri (stream, push) =
  prerr_endline "new connection";
  active_cons := (stream, push) :: !active_cons ;
  handle_messages current_state active_cons base_path stream push
;;

let server current_state active_cons base_path sockaddr =
  (*
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream >>= fun frame ->
    Lwt.wrap (fun () -> push (Some frame)) >>= fun () ->
    echo_fun uri (stream, push) in
  *)
  Websocket.establish_server sockaddr (handle_con current_state active_cons base_path)
;;

let run_server current_state active_cons host port base_path =
  prerr_endline ("Setting up websocket server on host="^host^", port="^(string_of_int port));
  Lwt_io_ext.sockaddr_of_dns host (string_of_int port) >>= fun sa ->
    Lwt.return (server current_state active_cons base_path sa)
;;

let send_errors active_cons ~errors ~warnings =
  let msg = Stog_server_types.Errors (errors, warnings) in
  push_message active_cons msg
;;

let send_update_message active_cons path op =
  let msg = Stog_server_types.Update (path, op) in
  push_message active_cons msg
;;

let diff_cut =
  let set = List.fold_left
    (fun set tag -> Stog_types.Str_set.add tag set)
    Stog_types.Str_set.empty
    [ "pre" ; "ul" ; "p" ; "svg"]
  in
  fun (_,tag) _ _ -> Stog_types.Str_set.mem (String.lowercase tag) set

let send_patch active_cons old_stog stog doc_id =
  let old_doc = Stog_types.doc old_stog doc_id in
  let new_doc = Stog_types.doc stog doc_id in
  let path = Stog_path.to_string new_doc.doc_path in

  prerr_endline ("sending patch (if needed) for path="^path) ;

    match old_doc.doc_out, new_doc.doc_out
      (*Some [Xtmpl.xml_of_string ~add_main: false "<html><body>coucou</body></html>"] *)
    with
    | None, None -> Lwt.return_unit
    | None, Some (t :: _) ->
        (*let s = Marshal.to_string t [] in*)
        let op = Stog_server_types.Update_all t in
        send_update_message active_cons path op
    | Some _, None ->
        let op = Stog_server_types.Patch [] in
        send_update_message active_cons path op
    | Some xtmpl1, Some xtmpl2 when xtmpl1 = xtmpl2 ->
        Lwt.return_unit
    | Some xtmpl1, Some xtmpl2 -> (* xml1 <> xml2 *)
        let xml1 = Xmldiff.xml_of_string (Xtmpl.string_of_xmls xtmpl1) in
        let xml2 = Xmldiff.xml_of_string (Xtmpl.string_of_xmls xtmpl2) in
        Lwt_preemptive.detach (Xmldiff.diff ~cut: diff_cut xml1) xml2 >>=
          (fun patch ->
             prerr_endline "patch computed";
             let op = Stog_server_types.Patch patch in
             send_update_message active_cons path op;
             (*let s = Marshal.to_string (List.hd xtmpl2) [] in
                Ocsigen_messages.warning (Printf.sprintf "sending marshalled operation, size=%d" (String.length s));
                send_update_message path
                (Stog_server_common.Update_all s);
                *)
          )
    | _, Some [] ->
        Lwt.return_unit
;;
