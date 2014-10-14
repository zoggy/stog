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

open Stog_server_types

module Xdiff = Xmldiff
module Xdiffjs = Xmldiff_js

let msg_box_id = "stog-server-preview-msgbox";;
let preview_style_url = "/styles/preview.css";;

let log s = Firebug.console##log (Js.string s);;

let mathjax_active =
  (* need to remember it mathjax was active, since in case of page
     updatefrom a server message, mathjax typeset is not called.
     It is called after the document is loaded, not when it changes. *)
  let mathjax_was_active = ref false in
  fun () ->
    !mathjax_was_active ||
      (
       let doc = Dom_html.document##getElementById(Js.string "MathJax_Message") in
       Js.Opt.case doc (fun _ -> false) (fun _ -> mathjax_was_active := true; true)
      )

let mathjax_typeset () =
  Js.Unsafe.eval_string("MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);")

let skip_node node =
  match Dom.nodeType node with
    Dom.Element e ->
      (
       match String.lowercase (Js.to_string (e##tagName)) with
         "div" ->
           (
            let skip =
              Js.Opt.case (e##getAttribute (Js.string "id"))
                (fun _ -> false)
                (fun js ->
                  let s = Js.to_string js in
                  s = msg_box_id || s = "MathJax_Message")
            in
            skip ||
              Js.Opt.case (e##getAttribute (Js.string "style"))
              (fun _ -> false)
              (fun js ->
                 let s = Js.to_string js in
                 s <> "")
           )
       | "link" ->
           (
            Js.Opt.case (e##getAttribute (Js.string "href"))
              (fun _ -> false)
              (fun js -> Js.to_string js = preview_style_url)
           )
       | _ -> false
      )
  | _ -> false

let dom_of_xtmpl =
  let rec map (doc : Dom_html.document Js.t) = function(*(t : Dom_html.element Js.t) = function*)
    Xtmpl.D s ->
      let n = doc##createTextNode (Js.string s) in
      (n :> Dom.node Js.t)
  | Xtmpl.E (name, atts, subs) ->
      let n =
        match name with
          ("", tag) -> doc##createElement (Js.string tag)
        | (uri, tag) ->
            (*log ("createElementNS("^uri^", "^tag^")");*)
            doc##createElementNS (Js.string uri, Js.string tag)
      in
      let atts =
        try Xtmpl.string_of_xml_atts atts
        with _ ->
            log ("problem with attributes of "^(Xdiff.string_of_name name));
            []
      in
      List.iter
        (fun (name, v) ->
           let v = Js.string v in
           match name with
             ("", att) -> ignore (n##setAttribute (Js.string att, v))
           | (uri, att) ->
               try ignore (Js.Unsafe.meth_call n "setAttributeNS"
                  (Array.map Js.Unsafe.inject [| Js.string uri ; Js.string att ; v |]))
                 (* FIXME: use setAttributeNS when will be available *)
               with _ ->
                   log ("could not add attribute "^(Xdiff.string_of_name name))
        )
        atts;
      let subs = List.map (map doc) subs in
      List.iter (Dom.appendChild n) subs;
      (n :> Dom.node Js.t)
  in
  fun t ->
    let doc = Dom_html.document in
    map doc t
;;


let add_msg_box () =
  let add () =
    let doc = Dom_html.document in
    let nodes = doc##getElementsByTagName (Js.string "body") in
    Js.Opt.iter (nodes##item(0))
      (fun body_node ->
         let atts = Xdiff.atts_of_list [ ("","id"), msg_box_id ] in
         let node = Xdiffjs.dom_of_xml (`E (("","div"), atts, [])) in
         ignore(body_node##insertBefore(node, body_node##firstChild));
      );
    let nodes = doc##getElementsByTagName (Js.string "head") in
    Js.Opt.iter (nodes##item(0))
      (fun head_node ->
         let atts = Xdiff.atts_of_list [
             ("","href"), preview_style_url ;
             ("","rel"), "stylesheet" ;
             ("","type"), "text/css";
           ]
         in
         let node = Xdiffjs.dom_of_xml (`E (("","link"), atts, [])) in
         ignore(head_node##insertBefore(node, head_node##firstChild));
      );
  in
  let doc = Dom_html.document##getElementById (Js.string msg_box_id) in
  Js.Opt.case doc add (fun _ -> ())
;;

let display_msg xmls =
  let nodes = List.map dom_of_xtmpl xmls in
  Ojsmsg_js.display_message msg_box_id nodes
;;

let display_error xmls =
  let nodes = List.map dom_of_xtmpl xmls in
  Ojsmsg_js.display_error msg_box_id nodes

let set_page_content xml =
  log "set_page_content";
  let doc = Dom_html.document in
  let children = doc##childNodes in
  log ("got children ("^(string_of_int (children##length)^")"));
  for i=0 to children##length-1 do
      Js.Opt.iter (doc##firstChild) (fun node -> Dom.removeChild doc node) ;
    (*Js.Opt.iter (children##item (i)) (fun node -> Dom.removeChild doc node) (* (children##item (i))*)*)
  done;
  log "building tree";
  let tree = dom_of_xtmpl xml in
  log "appending tree";
  Dom.appendChild doc tree;


  add_msg_box ();
  log "done";
;;

let stop_updating ws_connection  =
  ws_connection##close()
;;

let update page_path (path,op) =
  (*log (Printf.sprintf "receiving update for %s\npage_path=%s" path page_path);*)
  try
    match op with
      _ when path <> page_path -> ()
    | Patch patch ->
        log "patch received";
        Xdiffjs.apply_dom_patch ~skip_node patch ;
        if mathjax_active () then mathjax_typeset ();
        display_msg [Xtmpl.D "Page patched !"]
    | Update_all xml ->
        set_page_content xml;
        if mathjax_active () then mathjax_typeset ();
        display_msg [Xtmpl.D "Page updated !"]
  with e ->
    log (Printexc.to_string e)
;;

let display_errors ~errors ~warnings =
  let xmls =
    [
      Xtmpl.E (("","pre"),
       Xtmpl.atts_of_list [("","class"), [Xtmpl.D "error"]],
       [ Xtmpl.D (String.concat "\n" errors) ]) ;
      Xtmpl.E (("","pre"),
       Xtmpl.atts_of_list [("","class"), [Xtmpl.D "warning"]],
       [ Xtmpl.D (String.concat "\n" warnings) ]) ;
    ]
  in
  display_error xmls
;;

let handle_server_message page_path = function
  Update (path, op) -> update page_path (path, op)
| Errors (errors, warnings) -> display_errors ~errors ~warnings
;;

let ws_onmessage page_path _ event =
  try
    log "message received on ws";
    let hex = Js.to_string event##data in
    let marshalled = Stog_server_types.from_hex hex in
    let (mes : Stog_server_types.server_message) = Marshal.from_string marshalled 0 in
    handle_server_message page_path mes ;
    Js._false
  with
   e ->
      log (Printexc.to_string e);
      Js._false
;;

let set_up_ws_connection path url =
  try
    log ("connecting with websocket to "^url);
    (*let url = "ws://echo.websocket.org/" in*)
    let ws = jsnew WebSockets.webSocket(Js.string url) in
    (*log "setting binary";
    ws##binaryType <- Js.string "arraybuffer";*)
    ws##onopen <- Dom.handler (fun _ -> ws##send (Js.string ("GET "^path)); Js._false);
    ws##onclose <- Dom.handler (fun _ -> log "WS now CLOSED"; Js._false);
    (*log "handler set up";
    (
      match ws##readyState with
        | WebSockets.CONNECTING -> log "CONNECTING"
        | WebSockets.OPEN -> log "OPEN"
        | WebSockets.CLOSING -> log "CLOSING"
        | WebSockets.CLOSED -> log "CLOSED"
    );
    *)
    ws##onmessage <- Dom.full_handler (ws_onmessage path) ;
    Some ws
  with e ->
    log (Printexc.to_string e);
    None
;;

let stog_server =
  (Js.Unsafe.variable "stog_server" :>  < url : Js.js_string Js.t Js.prop ; doc : Js.js_string Js.t Js.prop > Js.t )

let ws_url = Js.to_string stog_server##url
let path = Js.to_string stog_server##doc

let _ = set_up_ws_connection path ws_url
(*
let launch_client_page ws_host ws_port path =
  Stog_server_common.call_caml (Get_doc path)
  >>=
    (fun ret ->
       match ret with
       | Error s ->
           let xml = Xtmpl.E
             (("","html"),Xtmpl.Name_map.empty,
              [ Xtmpl.E (("","body"),Xtmpl.Name_map.empty,
                 [ Xtmpl.E (("","pre"), Xtmpl.Name_map.empty, [ Xtmpl.D s ])
                 ])
              ])
           in
           set_page_content xml;
           Lwt.return_none
       | Elt_body s ->
           let (xml : Xtmpl.tree) = Marshal.from_string s 0 in
           set_page_content xml;
           if WebSockets.is_supported () then
              set_up_ws_connection path ws_host ws_port
           else
             (
              log "websockets not supported by this browser";
              Lwt.return_none
             )
    )
;;
*)