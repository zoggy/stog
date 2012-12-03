(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
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

let stog_ocaml_session = "stog-ocaml-session";;

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
    Stog_msg.verbose ~level:1 (Printf.sprintf "closing ocaml session %S" name);
    ignore(Unix.close_process (t.session_in, t.session_out))
  in
  Smap.iter f !sessions
;;

let create_session () =
  try
    let (ic, oc) = Unix.open_process stog_ocaml_session in
    { session_out = oc ; session_in = ic }
  with
    Unix.Unix_error (e, s1, s2) ->
      failwith (Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2)
;;

let get_session name =
  try Smap.find name !sessions
  with Not_found ->
      Stog_msg.verbose ~level:1 (Printf.sprintf "opening ocaml session %S"  name);
      let t = create_session () in
      sessions := Smap.add name t !sessions;
      t
;;

let eval_ocaml_phrase ?(session_name="default") phrase =
  let session = get_session session_name in
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

let fun_eval stog env args code =
  try
    let exc = Xtmpl.opt_arg args ~def: "true" "error-exc" = "true" in
    let toplevel = Xtmpl.opt_arg args ~def: "false" "toplevel" = "true" in
    let show_code = Xtmpl.opt_arg args ~def: "true" "show-code" <> "false" in
    let show_stdout = Xtmpl.opt_arg args
      ~def: (if toplevel then "true" else "false") "show-stdout" <> "false"
    in
    let session_name = Xtmpl.get_arg args "session" in
    let id_opt = Xtmpl.opt_arg args "id" in
    let atts = match id_opt with "" -> [] | id -> ["id", id] in
    let code =
      match code with
        [ Xtmpl.D code ] -> code
      | _ ->
          failwith (Printf.sprintf "Invalid code: %s"
           (String.concat "" (List.map Xtmpl.string_of_xml code)))
    in
    let phrases = ocaml_phrases_of_string code in
    let rec iter acc = function
      [] -> List.rev acc
    | phrase :: q ->
        let lang_file =
          let d = stog.Stog_types.stog_dir in
          Filename.concat d "ocaml.lang"
        in
        let opts = if Sys.file_exists lang_file then
            Printf.sprintf "--config-file=%s" lang_file
          else
            "--syntax=ocaml"
        in
        let code =
          if show_code then
            begin
              let code = Stog_misc.highlight ~opts phrase in
              let code = if toplevel then Printf.sprintf "# %s" code else code in
              Xtmpl.T ("div", [], [Xtmpl.xml_of_string code])
            end
           else
            Xtmpl.D ""
        in
        (*prerr_endline (Printf.sprintf "evaluate %S" phrase);*)
        let (output, stdout, raised_exc) =
          match eval_ocaml_phrase ?session_name phrase with
            Stog_ocaml_types.Ok (s, stdout) -> (s, stdout, false)
          | Stog_ocaml_types.Exc s -> (s, "", true)
        in
        if raised_exc && exc then
          begin
            let msg = Printf.sprintf "ocaml error with code:\n%s\n%s" phrase output in
            failwith msg
          end;

        let acc =
          match toplevel with
            false ->
              if show_stdout then
                let xml =
                  Xtmpl.T ("div", ["class", "ocaml-toplevel"], [Xtmpl.D stdout])
                in
                xml :: code :: acc
              else
                code::acc
          | true ->
              let classes = Printf.sprintf "ocaml-toplevel%s"
                (if raised_exc then " ocaml-exc" else "")
              in
              let xml =
                Xtmpl.T ("div", ["class", classes], [Xtmpl.D output])
              in
              xml :: code :: acc
        in
        iter acc q
    in
    let xml = iter [] phrases in
    if show_code || toplevel || show_stdout then
      [ Xtmpl.T ("pre", ["class", "code-ocaml"] @ atts, xml) ]
    else
      [ Xtmpl.D "" ]
  with
    e ->
      raise e
;;

