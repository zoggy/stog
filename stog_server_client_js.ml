(** *)

open Stog_server_types

module Xdiff = Xmldiff

let status_box_id = "stog-server-preview-status";;
let status_msg_id = status_box_id^"-message";;

let log s = Firebug.console##log (Js.string s);;

let string_of_name = function
  "", s -> s
| s1, s2 -> s1 ^ ":" ^ s2
;;

let dom_of_xml =
  let rec map (doc : Dom_html.document Js.t) = function
    `D s ->
      let n = doc##createTextNode (Js.string s) in
      (n :> Dom.node Js.t)
  | `E (name, atts, subs) ->
      let n =
        match name with
          ("", tag) -> doc##createElement (Js.string tag)
        | (uri, tag) -> doc##createElementNS (Js.string uri, Js.string tag)
      in
      Xdiff.Nmap.iter
        (fun name v ->
          let v = Js.string v in
           match name with
             ("", att) -> ignore (n##setAttribute (Js.string att, v))
           | (uri, att) ->
               try
                 ignore (Js.Unsafe.meth_call n "setAttributeNS"
                  (Array.map Js.Unsafe.inject [| Js.string uri ; Js.string att ; v |]))
                   (* FIXME: use setAttributeNS when will be available *)
               with _ ->
                   log ("could not add attribute "^(string_of_name name))
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

let dom_node_by_path path =
  let rec skip_node node =
    match Dom.nodeType node with
      Dom.Element e ->
        (
         match String.lowercase (Js.to_string (e##tagName)) with
           "div" ->
             (
              let skip =
                Js.Opt.case (e##getAttribute (Js.string "id"))
                  (fun _ -> false)
                  (fun js -> Js.to_string js = status_box_id)
              in
              skip ||
                Js.Opt.case (e##getAttribute (Js.string "style"))
                (fun _ -> false)
                (fun js ->
                   let s = Js.to_string js in
                   s <> "")
             )
         | _ -> false
        )
    | _ -> false

  in
  let doc = Dom_html.document in
  let rec next node path =
    let node = Js.Opt.get (node##nextSibling)
      (fun _ -> log ((Js.to_string node##nodeName)^" has no sibling"); raise Not_found)
    in
    iter node path
  and on_child node path =
    let node = Js.Opt.get (node##firstChild)
      (fun _ -> log ((Js.to_string node##nodeName)^" has no child"); raise Not_found) in
    iter node path
  and iter node path =
    if skip_node node then
      next node path
    else
      match path with
        Xdiff.Path_cdata n when (node##nodeType) = Dom.TEXT ->
          if n = 0 then
            node
          else
            next node (Xdiff.Path_cdata (n-1))
      | Xdiff.Path_node (name, n, more) when node##nodeType = Dom.ELEMENT ->
          let s_name = String.lowercase (string_of_name name) in
          let node_name = Js.to_string node##nodeName in
          (*log ("name="^s_name^", nodeName="^node_name^", n="^(string_of_int n));*)
          let node_name = String.lowercase node_name in
          if s_name = node_name then
            if n = 0 then
              match more with
                None -> node
              | Some p -> on_child node p
            else
              next node (Xdiff.Path_node (name, n-1, more))
          else
            next node (Xdiff.Path_node (name, n, more))
      | p -> next node p
  in
  on_child (doc:>Dom.node Js.t) path

let apply_patch_operation (path, op) =
  log (Xmldiff.string_of_patch_operation (path, op));
  let parent node = Js.Opt.get (node##parentNode) (fun _ -> assert false) in
  let apply node op =
    match op with
    | Xdiff.PReplace tree ->
        let parent = parent node in
        ignore(parent##replaceChild (dom_of_xml tree, node))
    | Xdiff.PInsertTree tree ->
        let parent = parent node in
        ignore(parent##insertBefore (dom_of_xml tree, (node##nextSibling)))
    | Xdiff.PDeleteTree ->
        let parent = parent node in
        ignore(parent##removeChild(node))
    | Xdiff.PUpdateCData s ->
        let parent = parent node in
        let text = Dom_html.document##createTextNode (Js.string s) in
        ignore(parent##replaceChild ((text :> Dom.node Js.t), node))
    | Xdiff.PUpdateNode (name, atts) when node##nodeType = Dom.TEXT ->
        let n = dom_of_xml (`E(name,atts,[])) in
        let parent = parent node in
        ignore(parent##replaceChild (n, node))
    | Xdiff.PUpdateNode (name, atts) when node##nodeType = Dom.ELEMENT ->
        let parent = parent node in
        let n = dom_of_xml (`E(name,atts,[])) in
        let children = node##childNodes in
        for i=0 to children##length-1 do
          Js.Opt.iter (node##firstChild) (fun node -> Dom.appendChild n node) ;
        done;
        ignore(parent##replaceChild (n, node))
    | _ -> () (* TODO print message ? *)
  in
  let node = dom_node_by_path path in
  apply (node:>Dom.node Js.t) op
;;

let apply_dom_patch l = List.iter apply_patch_operation l ;;

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
            log ("problem with attributes of "^(string_of_name name));
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
                   log ("could not add attribute "^(string_of_name name))
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


let atts_of_list l=
  List.fold_left
    (fun acc (name, v) -> Xdiff.Nmap.add name v acc)
      Xdiff.Nmap.empty l
;;

let add_status_box () =
  let add () =
    let doc = Dom_html.document in
    let nodes = doc##getElementsByTagName (Js.string "body") in
    Js.Opt.iter (nodes##item(0))
      (fun body_node ->
         let atts = atts_of_list
           [ ("","id"), status_box_id ;

             ("","style"),
             "position: fixed ; top: 40px ; left: 5px; z-index: 1000 ; \
             width: 30px; height: 30px; ; border-color: red ; \
             border-width: 3px ; \
             border-style : solid ; \
             background-color: white; opacity: 0.8 ;\
             overflow: hidden ; " ;

             ("","onmouseover"),
             "this.style.width = '80%'; this.style.height = '600px'; overflow: scroll ;" ;

             ("","onmouseout"),
             "this.style.width = '30px'; this.style.height = '30px'; overflow: hidden;" ;
           ]
         in
         let node = dom_of_xml
           (`E (("","div"), atts,
             [ `E (("","h2"), atts_of_list [("","style"), "color:#333333"], [ `D "Status" ]) ;
               `E (("","pre"), atts_of_list [("","id"), status_msg_id], [ `D "" ]) ;
             ] )
           )
         in
         ignore(body_node##insertBefore(node, body_node##firstChild));
      )
  in
  let doc = Dom_html.document##getElementById (Js.string status_box_id) in
  Js.Opt.case doc add (fun _ -> ())
;;

let set_status_msg xmls =
  let doc = Dom_html.document##getElementById (Js.string status_msg_id) in
  Js.Opt.case doc (fun _ -> ())
    (fun node ->
       let children = node##childNodes in
       for i = 0 to children##length - 1 do
         Js.Opt.iter (node##firstChild) (fun child -> Dom.removeChild node child) ;
       done;
       let l = List.map dom_of_xtmpl xmls in
       List.iter (Dom.appendChild node) l
    )
;;

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
  add_status_box ();
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
        apply_dom_patch patch ;
        set_status_msg [Xtmpl.D "Patched !"]
    | Update_all xml ->
        set_page_content xml;
        set_status_msg [Xtmpl.D "Updated !"]
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
  set_status_msg xmls
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