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

open Stog_ocaml_types;;

module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

type err = { line : int ; start : int ; stop : int ; message : string ; warning : bool }

let prompt = "# ";;
let length_prompt = String.length prompt;;

let stog_ocaml_session = ref "stog-ocaml-session";;

type session =
  { session_out : out_channel ;
    session_in : in_channel ;
  }

module Smap = Stog_types.Str_map;;

let sessions = ref Smap.empty;;

let close_sessions () =
  let f name t =
    close_out t.session_out;
    close_in t.session_in;
    Stog_msg.verbose ~level:1 (Printf.sprintf "Closing OCaml session %S" name);
    try ignore(Unix.close_process (t.session_in, t.session_out))
    with Unix.Unix_error (e, s1, s2) ->
      Stog_msg.warning
        (Printf.sprintf "Closing OCaml session %S: %s %s %s" name
          (Unix.error_message e) s1 s2)
  in
  Smap.iter f !sessions;
  sessions := Smap.empty
;;

let in_dir dir f =
  let old_cwd = Sys.getcwd () in
  Sys.chdir dir;
  let x = f () in
  Sys.chdir old_cwd ;
  x

let create_session () =
  try
    let (ic, oc) = Unix.open_process !stog_ocaml_session in
    { session_out = oc ; session_in = ic }
  with
    Unix.Unix_error (e, s1, s2) ->
      failwith (Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2)
;;

let get_session ?directory name =
  try Smap.find name !sessions
  with Not_found ->
      Stog_msg.verbose ~level:1
        (Printf.sprintf "Opening OCaml session %S%s" name
          (match directory with None -> "" | Some s -> "in "^s)
        );
      let t =
        match directory with
          None -> create_session ()
        | Some dir -> in_dir dir create_session
      in
      sessions := Smap.add name t !sessions;
      t
;;

let eval_ocaml_phrase ?(session_name="default") ?directory phrase =
  let session = get_session ?directory session_name in
  Stog_ocaml_types.write_input session.session_out
    { Stog_ocaml_types.in_phrase = phrase };
  Stog_ocaml_types.read_result session.session_in
;;

let ocaml_phrases_of_string s =
  let s = Stog_misc.strip_string s in
  let len = String.length s in
  let s =
    if len < 2 || String.sub s (len - 2) 2 <> ";;" then
      s^";;"
    else
      s
  in
  let acc = ref [] in
  let last_start = ref 0 in
  let len = String.length s in
  for i = 0 to len - 2 do
    if s.[i] = ';' && s.[i+1] = ';' then
      begin
        acc := (String.sub s !last_start (i + 2 - !last_start)) :: !acc;
        last_start := i+2
      end
  done;
  List.rev_map Stog_misc.strip_string !acc
;;

(*
let fun_new_env env args subs =
  let old_env = ! Toploop.toplevel_env in
  let restore_env subs =
    Toploop.toplevel_env := old_env ;
    subs
  in
  let xml =
    List.flatten (List.map (Xtmpl.eval_xml env) subs)
  in
  restore_env xml
;;
*)

let concat_code =
  let f b = function
    XR.D code -> Buffer.add_string b code.Xml.text
  | xml ->
      failwith (Printf.sprintf "XML code in OCaml code: %s"
       (XR.to_string [xml]))
  in
  fun xmls ->
    let b = Buffer.create 256 in
    List.iter (f b) xmls;
    Buffer.contents b
;;

let cut_errors =
  let re = Str.regexp "^Line \\([0-9]+\\), characters \\([0-9]+\\)\\(-[0-9]+\\)?:\n" in
  fun s ->
    let len = String.length s in
    let next p =
      try Some (Str.search_forward re s p)
      with Not_found -> None
    in
    let rec f acc p =
      match next p with
        None -> List.rev acc
      | Some p2 ->
          let line_len = String.length (Str.matched_string s) in
          let line = int_of_string (Str.matched_group 1 s) in
          let start = int_of_string (Str.matched_group 2 s) in
          let stop =
            try - (int_of_string (Str.matched_group 3 s))
            with _ -> start
          in
          let message =
            match next (p2+1) with
              None -> String.sub s (p2+line_len) (len - (p2 + line_len))
            | Some p3 -> String.sub s (p2+line_len) (p3 - (p2+line_len))
          in
          let err = { line ; start ; stop ; message ; warning = false } in
          let acc = err :: acc in
          f acc (p2+1)
    in
    match next 0 with
      None -> (s, [])
    | Some p -> (String.sub s 0 p, f [] 0)
;;


let get_errors ?(print_locs=false) s =
  let (before_errors, errors) = cut_errors s in
  let re_noerrline = Str.regexp "^[^ \t][^ \t][^ \t][^ \t][^ \t]" in
  let f err =
    let len = String.length err.message in
    if len < 7 then (* "Error: " or "Warning" has length 7 *)
      err
    else
      match String.sub err.message 0 7 with
      | "Warning" ->
          let err = { err with warning = true } in
          begin
            (* keep message until end of line *)
            match try Some (String.index err.message '\n') with _ -> None with
              None -> err
            | Some p ->
                let message = String.sub err.message 0 p in
                { err with message }
          end
      | "Error: " ->
          begin
            (* find the first line not beginning with 5 spaces *)
            match
              try Some (Str.search_forward re_noerrline err.message 1)
              with _ -> None
            with
              None -> err
            | Some p ->
                let message = String.sub err.message 0 p in
                { err with message }
          end
      | _ -> err
  in
  let details = List.map f errors in
  let f =
    if print_locs then
      (fun e ->
         Printf.sprintf "Line %d, characters %d-%d:\n%s" e.line e.start e.stop e.message)
    else
      (fun e -> e.message)
  in
  let err_output = String.concat "" (List.map f errors) in
  (before_errors^err_output, details)
;;

let add_loc_blocks errors code =
  let block err s =
    let cl = if err.warning then "warning-loc" else "error-loc" in
    XR.node ("","span")
     ~atts: (XR.atts_of_list [
         ("","title"), [XR.cdata err.message] ;
         ("","class"), [XR.cdata cl] ;
       ])
     [XR.cdata s]
  in
  let rec f err (l,c) = function
    "" -> ((l,c), [])
  | s ->
      if l > err.line then
        ((l, c), [ XR.cdata s])
      else
        if l = err.line then
          if c > err.stop then
            ((l, c), [XR.cdata s])
          else
            begin
              let len = String.length s in
              if c >= err.start then
                (
                 let required_size = err.stop - c in
                 if len <= required_size then
                   ((l, c + len), [block err s])
                 else
                   (
                    let s1 = String.sub s 0 required_size in
                    let s2 = String.sub s required_size (len - required_size) in
                    ((l, c+len), [block err s1 ; XR.cdata s2])
                   )
              )
              else
                if c + len < err.start then
                  ((l, c + len), [XR.cdata s])
                else
                  if c + len < err.stop then
                    (
                     let s1 = String.sub s 0 (err.start - c) in
                     let s2 = String.sub s (err.start - c) (len - (err.start - c)) in
                     ((l, c+len), [XR.cdata s1 ; block err s2])
                    )
                  else
                    (
                     let s1 = String.sub s 0 (err.start - c) in
                     let s2 = String.sub s (err.start - c) (err.stop - err.start) in
                     let s3 = String.sub s (err.stop - c) (len - (err.stop - c)) in
                     let c = c + len in
                     ((l, c+len), [XR.cdata s1 ; block err s2 ; XR.cdata s3])
                  )
            end
        else
          ( (* l < err.line *)
           let lines = Stog_misc.split_string ~keep_empty: true s ['\n'] in
           let nb_lines = List.length lines in
           (*prerr_endline (Printf.sprintf "line=%d; err.line=%d, nb_lines=%d, s=%s" l err.line nb_lines s);*)
           if l + nb_lines - 1 < err.line then
             ((l+nb_lines - 1, String.length (List.hd (List.rev lines))), [XR.cdata s])
           else
             let rec iter (l,c) = function
               [] -> assert false
             | line :: q ->
                 if l = err.line then
                   f err (l,c) (String.concat "\n" (line :: q))
                 else
                   (
                    let (acc, l) = iter (l+1,0) q in
                    (acc, (XR.cdata (line^"\n")) :: l)
                   )
             in
             iter (l,c) lines
          )
  in
  let rec fold_cdata f acc = function
    [] -> (acc, [])
  | (XR.D s) :: q ->
      let (acc, l) = f acc s.Xml.text in
      let (acc, l2) = fold_cdata f acc q in
      (acc, l @ l2)
  | (XR.E node) :: q ->
      let (acc,subs) = fold_cdata f acc node.XR.subs in
      let (acc, l) = fold_cdata f acc q in
      (acc, (XR.E { node with XR.subs }) :: l)
  | (XR.C _)  :: q
  | (XR.PI _) :: q -> fold_cdata f acc q
  in
  let f_err xmls err = snd (fold_cdata (f err) (1, 0) xmls) in
  let errors = List.sort Pervasives.compare errors in
  List.fold_left f_err code errors
;;

let highlight_warnings_and_errors ?print_locs output code =
  let (stderr, errors) = get_errors ?print_locs output.stderr in
  ({output with stderr}, add_loc_blocks errors code)
;;

let concat_toplevel_outputs output =
  let mk (cl, s) =
    match s with
      "" -> []
    | _ ->
        let atts = XR.atts_one ("","class") [XR.cdata cl] in
        [ XR.node ("","div") ~atts [XR.cdata s] ]
  in
  List.flatten (List.map mk
    [ "stderr", output.stderr ;
      "stdout", output.stdout ;
      "toplevel-out", output.topout ;
    ])
;;

let fun_eval stog env ?loc args code =
  try
    let directory =
      match XR.get_att_cdata args ("", "directory") with
        None | Some "" -> None
      | x -> x
    in
    let exc = XR.opt_att_cdata args ~def: "true" ("", "error-exc") = "true" in
    let toplevel = XR.opt_att_cdata args ~def: "false" ("", "toplevel") = "true" in
    let show_code = XR.opt_att_cdata args ~def: "true" ("", "show-code") <> "false" in
    let show_stdout = XR.opt_att_cdata args
      ~def: (if toplevel then "true" else "false") ("", "show-stdout") <> "false"
    in
    let highlight_locs = XR.opt_att_cdata args
      ~def: "true" ("","highlight-locs") <> "false"
    in
    let print_locs = XR.opt_att_cdata args
      ~def: (if highlight_locs then "false" else "true") ("","print-locs") <> "false"
    in
    let in_xml_block = XR.opt_att_cdata args ~def: "true" ("", "in-xml-block") <> "false" in
    let session_name = XR.get_att_cdata args ("", "session") in
    let id_opt = XR.opt_att_cdata args ("", "id") in
    let atts = XR.atts_of_list
      (match id_opt with "" -> [] | id -> [("","id"), [XR.cdata id]])
    in
    let code = concat_code code in
    let phrases = ocaml_phrases_of_string code in
    let rec iter acc = function
      [] -> List.rev acc
    | phrase :: q ->
        let lang_file =
          let d = stog.Stog_types.stog_dir in
          Filename.concat d "ocaml.lang"
        in
        let opts = if Sys.file_exists lang_file then
            Some (Printf.sprintf "--config-file=%s" lang_file)
          else
            None
        in
        let code =
          if show_code then
            Stog_highlight.highlight ~lang: "ocaml" ?opts phrase
           else
            [XR.cdata ""]
        in
        (*prerr_endline (Printf.sprintf "evaluate %S" phrase);*)
        let (output, raised_exc) =
          match eval_ocaml_phrase ?session_name ?directory phrase with
            Stog_ocaml_types.Ok output -> (output, false)
          | Stog_ocaml_types.Handled_error output -> (output, true)
          | Stog_ocaml_types.Exc s ->
              ({ stderr = s ; stdout = "" ; topout = "" }, true)
        in
        if raised_exc && exc then
          begin
            let msg = Xtmpl_xml.loc_sprintf loc
              "OCaml error with code:\n%s\n%s"
                phrase output.stderr
            in
            failwith msg
          end;

        let acc =
          match toplevel with
            false ->
              let code =
                if in_xml_block then [XR.node ("","div") code] else code in
              if show_stdout then
                let xml =
                  if in_xml_block then
                    XR.node ("","div")
                     ~atts: (XR.atts_one ("","class") [XR.cdata "ocaml-toplevel"])
                     [XR.cdata output.stdout]
                  else
                     XR.cdata output.stdout
                in
                xml :: code @ acc
              else
                code @ acc
          | true ->
              let (output, code) =
                if highlight_locs then
                  highlight_warnings_and_errors ~print_locs output code
                else
                  (output, code)
              in
              let code =
                if in_xml_block then
                  [ XR.node ("","div") ((XR.cdata prompt) :: code) ]
                else
                  code
              in
              let classes = Printf.sprintf "ocaml-toplevel%s"
                (if raised_exc then " ocaml-exc" else "")
              in
              let xml =
                XR.node ("","div")
                 ~atts: (XR.atts_one ("","class") [XR.cdata classes])
                 (concat_toplevel_outputs output)
              in
              xml :: code @ acc
        in
        iter acc q
    in
    let xml = iter [] phrases in
    if show_code || toplevel || show_stdout then
      let xml =
        if in_xml_block then
          [ XR.node ("","pre")
             ~atts: (XR.atts_of_list ~atts [("","class"), [XR.cdata "code-ocaml"]])
             xml
          ]
        else
          xml
      in
      (stog, xml)
    else
      (stog, [ XR.cdata "" ])
  with
    e ->
      raise e
;;


let fun_printf stog env ?loc args subs =
  let code = concat_code subs in
  let format = XR.opt_att_cdata args ~def: "%s" ("", "format") in
  let code = "Printf.printf \""^format^"\" "^code^"; flush Pervasives.stdout;;" in
  let args = XR.atts_of_list ~atts: args
    [ ("","show-stdout"), [XR.cdata "true"] ;
      ("","show-code"), [XR.cdata "false"] ;
      ("","in-xml-block"), [XR.cdata "false"] ;
    ]
  in
  fun_eval stog env args [XR.cdata code]
;;


