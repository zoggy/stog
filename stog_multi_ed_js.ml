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

(** *)


let msg_of_wsdata = Ojs_js.mk_msg_of_wsdata Stog_multi_ed_types.server_msg_of_yojson
let wsdata_of_msg msg =
  Yojson.Safe.to_string (Stog_multi_ed_types.client_msg_to_yojson msg)

let ref_send = ref ((fun _ -> Lwt.return_unit) : Stog_multi_ed_types.App_msg.app_client_msg -> unit Lwt.t)
let send msg = !ref_send msg

module Rpc_base = Ojs_rpc.Base(Stog_multi_ed_types.App_msg)
module Rpc = Ojs_rpc.Make_client(Rpc_base)
let rpc_handler = Rpc.rpc_handler send

let call = Rpc.call rpc_handler

module FT = Ojsft_js.Make(Stog_multi_ed_types.FT)
module ED = Ojsed_js.Make(Stog_multi_ed_types.ED)

let trees = new FT.trees call send (new FT.tree);;
let editors = new ED.editors call send (new ED.editor);;

let on_deselect ti path = ()

let on_select ti path =
  Ojs_js.log "select !";
  ignore
    (call (Stog_multi_ed_types.ED.pack_client_msg "ed"
      (Stog_multi_ed_types.ED.Get_file_contents path)
     )
     (fun msg ->
        match Stog_multi_ed_types.ED.unpack_server_msg msg with
        | Some (id, msg) ->
            Ojs_js.log "got a response!";
            ignore(editors#handle_message (Stog_multi_ed_types.ED.SEditor (id, msg)));
            Lwt.return_unit
        | None ->
            Ojs_js.log "select: ignored response";
            Lwt.return_unit)
    )

let onopen ws =
  ref_send := (fun msg -> Ojs_js.send_msg ws (wsdata_of_msg msg); Lwt.return_unit);
  let tree = trees#setup_filetree
    ~msg_id: Stog_multi_ed_types.ojs_msg_id
    Stog_multi_ed_types.ft_id
  in
  tree#set_on_select on_select;
  tree#set_on_deselect on_deselect;
  ignore(editors#setup_editor
   ~msg_id: Stog_multi_ed_types.ojs_msg_id
     ~bar_id: Stog_multi_ed_types.bar_id
     Stog_multi_ed_types.ed_id)

let onmessage ws msg =
  match msg with
  | Stog_multi_ed_types.FT.SFiletree _ -> trees#handle_message msg
  | Stog_multi_ed_types.ED.SEditor _  -> editors#handle_message msg
  | Rpc_base.SReturn (call_id, msg) -> Rpc.on_return rpc_handler call_id msg; Js._false
  | _ -> failwith "Unhandled message"

let set_up_ws_connection ~ws_url =
  try
    Some (Ojs_js.setup_ws ws_url
     msg_of_wsdata wsdata_of_msg
       ~onopen ~onmessage)
  with e ->
      Ojs_js.log (Printexc.to_string e);
      None

let stog_server =
  (Js.Unsafe.variable "stog_server" :>
   < wsUrl : Js.js_string Js.t Js.prop ;
   > Js.t )

let ws_url = Js.to_string stog_server##wsUrl

let _ = set_up_ws_connection ~ws_url