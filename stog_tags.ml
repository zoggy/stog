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

let site_title = "site-title"
let site_desc = "site-description"
let site_url = "site-url"
let site_email = "site-email"
let stog_dir = "stog-dir"
let rss_length = "rss-length"
let languages = "languages"
let functions = "functions_"

let elt = "elt"
let elt_body = "elt-body"
let elt_date = "elt-date"
let elt_datetime = "elt-datetime"
let elt_intro = "elt-intro"
let elt_keywords = "elt-keywords"
let elt_navpath = "elt-navpath"
let elt_path = "elt-path"
let elt_src = "elt-src"
let elt_title = "elt-title"
let elt_topics = "elt-topics"
let elt_type = "elt-type"
let elt_url = "elt-url"

let sep = "sep_"

let archive_tree = "archive-tree"
let as_xml = "as-xml"
let block = "block"
let command_line = "command-line"
let counter = "counter"
let dummy_ = "dummy_"
let elements = "elements"
let ext_a = "ext-a"
let error_ = "error_"
let graph = "graph"
let hcode = "hcode"
let path_sep = "path-sep"
let icode = "icode"
let if_ = "if"
let image = "image"
let inc = "inc"
let include_ = "include"
let keyword = "keyword"
let langswitch = "langswitch"
let late_inc = "late-inc"
let latex = "latex"
let latex_body = "latex-body"
let list = "list"
let n_columns = "n-columns"
let next = "next"
let next_path = "next-path"
let ocaml = "ocaml"
let ocaml_eval = "ocaml-eval"
let ocaml_printf = "ocaml-printf"
let page = "page"
let paragraph = "paragraph"
let post = "post"
let prefix_svg_ids = "prefix-svg-ids"
let prepare_toc = "prepare-toc"
let previous = "previous"
let previous_path = "previous-path"
let search_form = "search-form"
let section = "section"
let subsection = "subsection"
let subsubsection = "subsubsection"
let toc = "toc"
let topic = "topic"
let two_columns = "two-columns"

let default_sectionning =
  [ section ;
    subsection ;
    subsubsection ;
    paragraph ;
  ]
