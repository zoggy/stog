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

(** Computing diffs on XML trees.

  Algorithm from
  "Tree to tree correction for document trees"
  by Barnart, Clarke and Duncan. Technical report.

  We implement the first extension of Zhang and Shasha.
*)

module Nmap = Xtmpl.Name_map

type name = Xmlm.name
type xmltree =
    E of name * string Nmap.t * xmltree list
  | D of string

let rec xml_of_source s_source source =
 try
    let ns s = Some s in
    let input = Xmlm.make_input ~ns ~enc: (Some `UTF_8) source in
    let el (tag, atts) childs =
      let atts = List.fold_left
        (fun map (name, v) -> Nmap.add name v map) Nmap.empty atts
      in
      E (tag, atts, childs)
    in
    let data d = D d in
    let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
    tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "%sLine %d, column %d: %s"
        s_source line col (Xmlm.error_message error)
      in
      failwith msg
  | Invalid_argument e ->
      let msg = Printf.sprintf "%sInvalid_argumen(%s)" s_source e in
      failwith msg

and xml_of_string s =
  xml_of_source (s^"\n") (`String (0, s))
;;

let xml_of_file file =
  let ic = open_in file in
  try
    let xml = xml_of_source
      (Printf.sprintf "File %S, " file) (`Channel ic)
    in
    close_in ic;
    xml
  with
    e ->
      close_in ic;
      raise e
;;

type node = {
  number : int ;
  leftmost : int ;
  keyroot : bool ;
  child : int array ;
  parent : int option ;
  xml : xmltree ;
  }

let short_label = function
  E ((s1,s2), _, _) -> s1^":"^s2
| D _ -> "<pcdata>"
;;

let dot_of_t t =
  let b = Buffer.create 256 in
  let p b = Printf.bprintf b in
  p b "digraph g {\nrankdir=TB;\nordering=out;\n";
  Array.iter
    (fun node ->
       p b "N%d [ label=\"%d: %s [%d]\" ];\n" node.number node.number (short_label node.xml) node.leftmost;
       Array.iter (fun i -> p b "N%d -> N%d;\n" node.number i) node.child ;
    )
    t;
  p b "}\n";
  Buffer.contents b
;;

let t_of_xml =
  let rec iter (n, acc, acc_children) xml =
    let subs = match xml with
        E (_, _, l) -> l
      | D _ -> []
    in
    let (n, acc, children) = List.fold_left iter (n, acc, []) subs in
    let leftmost =
      match children with
        [] -> n
      | node :: _ -> node.leftmost
    in
    let node =
      { number = n ; leftmost ; keyroot = acc_children <> [] ;
        child = Array.of_list (List.map (fun node -> node.number) children) ;
        parent = None ;
        xml ;
      }
    in
    let children = List.map (fun node -> { node with parent = Some n }) children in
    let acc = match children with [] -> acc | _ -> acc @ children in
    (n+1, acc, acc_children @ [node])
  in
  fun xml ->
    let (_, l, root) = iter (0, [], []) xml in
    let t = Array.of_list (l @ root) in
    Array.sort (fun n1 n2 -> n1.number - n2.number) t;
    (*
    Stog_misc.file_of_string ~file: "/tmp/t.dot" (dot_of_t t);
    *)
    Array.iteri (fun i node ->
       (*prerr_endline (Printf.sprintf "i=%d, node.number=%d, parent=%s, xml=%s" i node.number
         (match node.parent with None -> "" | Some n -> string_of_int n)
         (short_label node.xml)
       );*)
       assert (i = node.number)
    ) t;
    t
;;

let diff xml1 xml2 =
  let t1 = t_of_xml xml1 in
  let t2 = t_of_xml xml2 in
  ignore(t1,t2)
;;

