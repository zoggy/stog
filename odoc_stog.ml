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

(** OCamldoc custom generator, to create stog documents. *)

open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module

let p = Printf.bprintf
let bp = Printf.bprintf
let bs = Buffer.add_string

(*c==m=[File.Find]=0.1====*)
module Find = struct
    open Unix

    type filter =
      Maxdepth of int
      | Type of Unix.file_kind
      | Follow
      | Regexp of Str.regexp
      | Atime of interval
      | Predicate of (string -> bool)
    and interval =
      Le of int | Eq of int | Ge of int

    type mode =
      | Ignore
      | Stderr
      | Failure
      | Custom of (Unix.error * string * string -> unit)

    (* To memorize visited inodes *)
    type inode = int * int
    let inode st = st.st_dev, st.st_ino

    (* parameters driving the find *)
    type status =
	{ maxdepth : int;
	  follow : bool;
	  filters : (string -> stats -> bool) list;
	  stat_function : string -> stats;
	    action : string -> unit;
	      handler : (error * string * string -> unit)
	}

    exception Hide of exn
    (* Used to pathe user-level errors, so that they are no trap by the library *)
    let pathe_exn f x = try f x with exn -> raise (Hide exn)
    let reveal_exn f x = try f x with Hide exn -> raise exn

    let stderr_handler (e, b, c) =
      prerr_endline ("find: " ^ c ^": " ^ (error_message e))
    let ignore_handler _ = ()
    let failure_handler (e,b,c) = raise (Hide (Unix_error (e, b, c)))
    let handler = function
      Stderr -> stderr_handler
      | Ignore -> ignore_handler
      | Failure -> failure_handler
      | Custom h -> pathe_exn h

    (* handlers of errors during the call. *)
    let treat_unix_error h f x =
      try f x with  Unix_error (e, b, c) ->  h (e, b, c)

    let default_status =
      { follow = false;
	maxdepth = max_int;
	filters = [];
	stat_function = lstat;
	action = prerr_endline;
	handler = handler Stderr;
      }

    let add_filter status f = { status with filters = f :: status.filters }

    let seconds_in_a_day = 86400.
    exception Find of string
    let rec parse_option status = function
      | Maxdepth n ->
	  { status with maxdepth = n }
      | Type k ->
	  add_filter status
            (fun name stat -> stat.st_kind = k)
      | Follow ->
	  { status with follow = true }
      | Regexp exp ->
	  add_filter status
            (fun name stat ->
              Str.string_match exp name 0 &&
              Str.match_beginning () = 0 &&
              Str.match_end () = String.length name
            )

      | Atime n ->
	  let min, max =
            match n with
            | Eq d when d > 0 ->
		float d *. seconds_in_a_day, float (d-1) *. seconds_in_a_day
            | Le d when d > 0 ->
		min_float, float (d-1) *. seconds_in_a_day
            | Le d when d > 0 ->
		min_float, float (d-1) *. seconds_in_a_day
            | Ge d when d > 0 ->
		float (d) *. seconds_in_a_day, max_float
            | _ -> raise (Find "Ill_formed argument")
	  in
	  let now = time() in
	  add_filter status
            (fun name stat ->
              let time = now -. stat.st_atime in min <= time && time <= max)
      | Predicate f ->
	  add_filter status (fun name stat -> f name)

    let parse_options options =
      List.fold_left parse_option default_status options

    (* fonctions auxilaires *)

    let filter_all filename filestat filters =
      List.for_all (fun f -> f filename filestat) filters

    let iter_dir f d =
      let dir_handle = opendir d in
      try while true do f (readdir dir_handle) done with
	End_of_file -> closedir dir_handle
      | x -> closedir dir_handle; raise x

    (* fonction principale seconde version *)
    let rec find_rec status visited depth filename =
      let find() =
	let filestat =
	  if status.follow then stat filename else lstat filename in
	let id = filestat.st_dev, filestat.st_ino in
	if filter_all filename filestat status.filters then status.action filename;
	if filestat.st_kind = S_DIR && depth < status.maxdepth &&
	  (not status.follow || not (List.mem id visited))
	then
	  let process_child child =
            if (child <> Filename.current_dir_name &&
		child <> Filename.parent_dir_name) then
              let child_name = Filename.concat filename child in
              let visited = if status.follow then id :: visited else visited in
              find_rec status visited (depth+1) child_name
	  in
          (* process_child is recursively protected from errors *)
	  iter_dir process_child filename
      in
      treat_unix_error status.handler find ()

    let find_entry status filename = find_rec status [] 0 filename

    let find mode filenames options action =
      let status =
	{ (parse_options options) with
	  handler = handler mode;
	  action = pathe_exn action }
      in
      reveal_exn (List.iter (find_entry status)) filenames

    let find_list mode filenames options =
      let l = ref [] in
      find mode filenames options (fun s -> l := s :: !l);
      List.rev !l


   end
(*/c==m=[File.Find]=0.1====*)

(*c==v=[String.replace_in_string]=1.0====*)
let replace_in_string ~pat ~subs ~s =
  let len_pat = String.length pat in
  let len = String.length s in
  let b = Buffer.create len in
  let rec iter pos =
    if pos >= len then
      ()
    else
      if pos + len_pat > len then
        Buffer.add_string b (String.sub s pos (len - pos))
      else
        if String.sub s pos len_pat = pat then
          (
           Buffer.add_string b subs;
           iter (pos+len_pat)
          )
        else
          (
           Buffer.add_char b s.[pos];
           iter (pos+1);
          )
  in
  iter 0;
  Buffer.contents b
(*/c==v=[String.replace_in_string]=1.0====*)

(*c==v=[File.string_of_file]=1.1====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_subbytes buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.1====*)

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)


module Generator (G : Odoc_html.Html_generator) =
  struct
    class html =
      object(self)
        inherit G.html as html

        method html_of_type_expr_param_list b m_name t =
          match Odoc_info.string_of_type_param_list t with
            "" -> ()
          | s ->
              let s2 = Odoc_html.newline_to_indented_br s in
              bs b "<code class=\"type\">";
              bs b (self#create_fully_qualified_idents_links m_name s2);
              bs b "</code>"

          (** Print html code for a list of module parameters. *)
        method html_of_module_parameter_list b m_name l =
          match l with
            [] ->
              ()
          | _ ->
              bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
              bs b "<tr>\n";
              bs b "<td align=\"left\" valign=\"top\" width=\"1%%\"><b>";
              bs b Odoc_messages.parameters ;
              bs b ": </b></td>\n<td>\n";
              bs b "<table class=\"paramstable\">\n";
              List.iter
                (fun (p, desc_opt) ->
                   bs b "<tr>\n";
                   bs b "<td align=\"center\" valign=\"top\" width=\"15%\">\n<code>" ;
                   bs b p.mp_name;
                   bs b "</code></td>\n" ;
                   bs b "<td align=\"center\" valign=\"top\">:</td>\n";
                   bs b "<td>" ;
                   self#html_of_module_parameter_type b m_name p;
                   bs b "\n";
                   (
                    match desc_opt with
                      None -> ()
                    | Some t ->
                        bs b "<br>";
                        self#html_of_text b t;
                   );
                   bs b "</td>\n</tr>\n" ;
                )
                l;
              bs b "</table>\n</td>\n</tr>\n</table>\n"


        method prepare_header module_list =
          let f b ?(nav=None) ?(comments=[]) t =
            bs b ("<title>"^t^"</title>")
          in
          header <- f

        method private post_process file =
          let s = string_of_file file in
          let replacements =
            [ "<html>", "<ocamldoc with-contents=\"true\">" ;
              "</html>", "</ocamldoc>" ;
              "<body>", "<contents><div class=\"ocamldoc-page\">" ;
              "</body>", "</div></contents>" ;
              "&nbsp;", "&#160;" ;
              "<br>", "<br/>";
              "<hr width=\"100%\">", "<hr width=\"100%\"/>";
              "-->", "FOO___" ;
              "->", "-&gt;" ;
              "FOO___", "-->" ;
              "< ", "&lt; " ;
              "<p>", "<div class=\"vertical-space\"> </div>";
              "<h1></h1>", "" ;
              "<h2 ", "<div class=\"section-title\" ";
              "</h2>", "</div>";
              "<h3 ", "<div class=\"subsection-title\" ";
              "</h3>", "</div>";
            ]
          in
          let s = List.fold_left
            (fun s (pat, subs) -> replace_in_string ~pat ~subs ~s)
            s replacements
          in
          file_of_string ~file s

        method generate module_list =
          html#generate module_list ;
          let html_files = Find.find_list
            Find.Ignore
            [ !Global.target_dir ]
            [ Find.Regexp (Str.regexp ".*\\.html") ]
          in
          List.iter self#post_process html_files

  end
end

let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor);;
