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

(** Functions to eval and display ocaml in generated pages. *)

let _ = Toploop.set_paths ();;
let _ = Toploop.initialize_toplevel_env();;
let _ =
  match Hashtbl.find Toploop.directive_table "rectypes" with
    Toploop.Directive_none f -> f ()
  | _ -> assert false;;
let _ = Toploop.max_printer_steps := 20;;

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

let log_oc = open_out "ocaml.log";;

(*let _ = Location.input_name := "";;*)
let stderr_file = Filename.temp_file "stogocaml" "err";;
let stdout_file = Filename.temp_file "stogocaml" "out";;

let remove_empty_filename =
  let empty = "File \"\", l" in
  let re = Str.regexp_string empty in
  Str.global_replace re "L"
;;


let eval_ocaml_phrase ~exc phrase =
  try
    let lexbuf = Lexing.from_string phrase in
    let fd_err = Unix.openfile stderr_file
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
      0o640
    in
    Unix.dup2 fd_err Unix.stderr;
    let fd_out = Unix.openfile stdout_file
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
      0o640
    in
    Unix.dup2 fd_out Unix.stdout;
    Unix.close fd_out;
    Printf.fprintf log_oc "executing phrase: %s\n" phrase;
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    Printf.fprintf log_oc "phrase parsed\n";
    ignore(Toploop.execute_phrase true Format.str_formatter phrase);
    let exec_output = Format.flush_str_formatter () in
    let err = Stog_misc.string_of_file stderr_file in
    let err = remove_empty_filename err in
    let out = Stog_misc.string_of_file stdout_file in
    Printf.fprintf log_oc "exec_output: %s\n" exec_output;
    Printf.fprintf log_oc "err: %s\n" err;
    Printf.fprintf log_oc "out: %s\n" out;
    (
     match err with
       "" -> ()
     | s -> Format.pp_print_string Format.str_formatter s
    );
    (
     match out with
       "" -> ()
     | s -> Format.pp_print_string Format.str_formatter s
    );
    Format.pp_print_string Format.str_formatter exec_output;
    `Ok (Stog_misc.strip_string (Format.flush_str_formatter ()), out)
  with
  | e ->
      (* Errors.report_error relies on exported compiler lib; on some
         bugged setups, those libs are not in synch with the compiler
         implementation, and the call below fails
         because of an implementation mismatch with the toplevel.

         We are therefore extra careful when calling
         Errors.report_error, and in particular collect backtraces to
         help spot this vicious issue. *)

      let backtrace_enabled = Printexc.backtrace_status () in
      if not backtrace_enabled then Printexc.record_backtrace true;
      begin
        try Errors.report_error Format.str_formatter e
        with exn ->
          Printf.fprintf log_oc
            "an error happened during phrase error reporting:\n%s\n%!"
            (Printexc.to_string exn);
          Printf.fprintf log_oc "error backtrace:\n%s\n%!"
            (Printexc.get_backtrace ());
      end;
      if not backtrace_enabled then Printexc.record_backtrace false;

      let err = Format.flush_str_formatter () in
      let msg = Printf.sprintf "ocaml error with code:\n%s\n%s" phrase err in
      if exc then
        failwith msg
      else
        `Exc (Stog_misc.strip_string (remove_empty_filename err))
;;


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

let fun_eval env args code =
  let original_stderr = Unix.dup Unix.stderr in
  let original_stdout = Unix.dup Unix.stdout in
  try
    let exc = Xtmpl.opt_arg args ~def: "true" "error-exc" = "true" in
    let toplevel = Xtmpl.opt_arg args ~def: "false" "toplevel" = "true" in
    let show_code = Xtmpl.opt_arg args ~def: "true" "show-code" <> "false" in
    let show_stdout = Xtmpl.opt_arg args
      ~def: (if toplevel then "true" else "false") "show-stdout" <> "false"
    in
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
          let d =
            match !Stog_html.current_stog with
              None -> Filename.dirname Sys.argv.(0)
            | Some stog -> stog.Stog_types.stog_dir
          in
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
              let code = Stog_html.highlight ~opts phrase in
              let code = if toplevel then Printf.sprintf "# %s" code else code in
              Xtmpl.T ("div", [], [Xtmpl.xml_of_string code])
            end
          else
            Xtmpl.D ""
        in
        let (output, stdout, raised_exc) =
          match eval_ocaml_phrase ~exc phrase with
            `Ok (s, stdout) -> (s, stdout, false)
          | `Exc s -> (s, "", true)
        in
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
    Unix.dup2 original_stdout Unix.stdout;
    Unix.dup2 original_stderr Unix.stderr;
    if show_code || toplevel || show_stdout then
      [ Xtmpl.T ("pre", ["class", "code-ocaml"], xml) ]
    else
      [ Xtmpl.D "" ]
  with
    e ->

      Unix.dup2 original_stdout Unix.stdout;
      Unix.dup2 original_stderr Unix.stderr;
      raise e
;;

let ocaml_funs =
  [ "ocaml-new-env", fun_new_env ;
    "ocaml-eval", fun_eval ;
  ]
;;

Stog_html.plugin_funs := ocaml_funs @ !Stog_html.plugin_funs;;
