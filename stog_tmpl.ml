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

open Stog_types;;

type contents = Stog_types.stog -> Xtmpl.tree

let parse = Xtmpl.xml_of_string ~add_main: false ;;

let get_template stog contents name =
  let file = Filename.concat stog.stog_tmpl_dir name in
  let contents = contents stog in
  if not (Sys.file_exists file) then
    (
     Stog_misc.safe_mkdir stog.stog_tmpl_dir ;
     Stog_msg.warning
       (Printf.sprintf "Creating default template file %S" file);
     Stog_misc.file_of_string ~file (Xtmpl.string_of_xml contents);
     contents
    )
  else
    Xtmpl.xml_of_file file
;;

let page _ = parse
  "<html>
    <head>
      <title><site-title/> : <elt-title/></title>
      <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>
    </head>
    <body>
      <h1><elt-title/></h1>
      <elt-body/>
    </body>
  </html>"

let by_keyword stog =
  let t = parse
    "<include file=\"page.tmpl\" elt-title=\"Posts for keyword '&lt;elt-title/&gt;'\"/>"
  in
  ignore(get_template stog page "page.tmpl");
  t
;;

let by_topic stog =
  let t = parse
    "<include file=\"page.tmpl\" elt-title=\"Posts for topic '&lt;elt-title/&gt;'\"/>"
  in
  ignore(get_template stog page "page.tmpl");
  t
;;

let by_month stog =
  let t = parse
    "<include file=\"page.tmpl\" elt-title=\"Posts of &lt;elt-title/&gt;\"/>"
  in
  ignore(get_template stog page "page.tmpl");
  t
;;

let elt_in_list _ = parse
  "<div itemprop=\"blogPosts\" itemscope=\"\" itemtype=\"http://schema.org/BlogPosting\" class=\"elt-item\">
     <div class=\"elt-item-title\">
       <link itemprop=\"url\" href=\"&lt;elt-url/&gt;\"/>
       <a href=\"&lt;elt-url/&gt;\"><span itemprop=\"name\"><elt-title/></span></a>
     </div>
     <div class=\"date\"><elt-date/></div>
     <div itemprop=\"headline\" class=\"elt-intro\"><elt-intro/></div>
  </div>"
;;

let keyword _ = parse "<span itemprop=\"keywords\"><keyword/></span>";;
let topic = keyword;;
