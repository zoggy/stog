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


type label = Node of string | Text of string

type node = {
  number : int ;
  leftmost : int ;
  keyroot : bool ;
  child : int array ;
  parent : int option ;
  xml : xmltree ;
  label : label ;
  }

type cost = int

type operation =
    | InsertTree of node * int (* insert tree from t2 as a right sibling of [int] in t1 *)
    | DeleteTree of node (* delete tree from t1 *)
    | InsertNode of node * int (* insert node from t2 as a right sibling of [int] in t1 *)
    | DeleteNode of node
    | Edit of node * node (* change label of node from t1 to label of node from t2 *)

type cost_fun = operation -> cost

type actions = cost * operation list

let min_action (c1, l1) (c2, l2) =
  if c1 <= c2 then (c1, l1) else (c2, l2)

let add_action (c,l) cost action = (c+cost, action :: l)
let add_actions (c1,l1) (c2,l2) = (c1+c2, l1@l2)

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

let string_of_name = function
  ("",s) -> s
| (ns,s) -> ns^":"^s
;;

let string_of_atts map =
  let l =
    Nmap.fold
      (fun name s acc ->
         (Printf.sprintf "%s=%S" (string_of_name name) s) :: acc)
      map []
  in
  String.concat " " l
;;

let label_of_xml = function
| D s -> Text s
| E (tag, atts, _) ->
    Node
      (Printf.sprintf "<%s %s>"
       (string_of_name tag) (string_of_atts atts))
;;

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
       p b "N%d [ label=\"%d: %s [%d]\", fontcolor=%s ];\n"
         node.number node.number (short_label node.xml) node.leftmost
         (if node.keyroot then "red" else "black");
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
        xml ; label = label_of_xml xml ;
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
    (* set root node as keyroot *)
    (*let len = Array.length t in
    t.(len-1) <- { t.(len-1) with keyroot = true };*)

    Stog_misc.file_of_string ~file: "/tmp/t.dot" (dot_of_t t);

    Array.iteri (fun i node ->
       prerr_endline (Printf.sprintf "i=%d, keyroot=%b, node.number=%d, parent=%s, xml=%s" i node.keyroot node.number
         (match node.parent with None -> "" | Some n -> string_of_int n)
         (short_label node.xml)
       );
       assert (i = node.number)
    ) t;
    t
;;

let compute fc t1 t2 =
  (* forest distance *)
  let fd = Array.init (Array.length t1) (fun i -> Array.create (Array.length t2) (0,[])) in
  (* tree distance *)
  let d = Array.init (Array.length t1) (fun i -> Array.create (Array.length t2) (0,[])) in

  for x = 0 to Array.length t1 - 1 do
    match t1.(x).keyroot with
      false -> ()
    | true ->
        let lx = t1.(x).leftmost in
        for y = 0 to Array.length t2 - 1 do
          match t2.(y).keyroot with
            false -> ()
          | true ->
              let ly = t2.(y).leftmost in
              prerr_endline  (Printf.sprintf "lx=%d, ly=%d" lx ly);
              fd.(lx - 1).(ly - 1) <- (0, []);
              for i = lx to x do
                let op = DeleteTree t1.(i) in
                fd.(i).(ly - 1) <- add_action
                  fd.(t1.(i).leftmost - 1).(ly - 1)
                  (fc op) op
              done;
              for j = ly to y do
                let op = InsertTree (t2.(y), lx - 1) in
                fd.(lx - 1).(j) <- add_action
                  fd.(lx - 1).(t2.(j).leftmost - 1)
                  (fc op) op
              done;
              for i = lx to x do
                for j = ly to y do
                  let li = t1.(i).leftmost in
                  let lj = t2.(j).leftmost in
                  let op_insertnode = InsertNode(t2.(j), i) in
                  let cost_insertnode = add_action
                    fd.(i).(j-1) (fc op_insertnode) op_insertnode
                  in
                  let op_deletenode = DeleteNode(t1.(i)) in
                  let cost_deletenode = add_action
                    fd.(i-1).(j) (fc op_deletenode) op_deletenode
                  in
                  let op_inserttree = InsertTree(t2.(j), i) in
                  let cost_inserttree = add_action
                    fd.(i).(lj-1)
                    (fc op_inserttree) op_inserttree
                  in
                  let op_deletetree = DeleteTree(t1.(i)) in
                  let cost_deletetree = add_action
                    fd.(li-1).(j)
                    (fc op_deletetree) op_deletetree
                  in
                  let part = min_action
                    (min_action cost_insertnode cost_deletenode)
                    (min_action cost_inserttree cost_deletetree)
                  in
                  if li = lx && lj = ly then
                    (
                     let op_edit = Edit (t1.(i), t2.(i)) in
                     let cost_edit = add_action
                       fd.(i-1).(j-1) (fc op_edit) op_edit
                     in
                     d.(i).(j) <- min_action part cost_edit;
                     fd.(i).(j) <- d.(i).(j)
                    )
                  else
                    (
                     fd.(i).(j) <- min_action
                       part
                       (add_actions fd.(li-1).(lj-1) d.(i).(j))
                    )
                done
              done
        done;
  done;
  fd.(Array.length t1 - 1).(Array.length t2 - 1)
;;

let diff xml1 xml2 =
  let t1 = t_of_xml xml1 in
  let t2 = t_of_xml xml2 in
  let fc = function
    InsertTree _ -> 2
  | _ -> 1
  in
  compute fc t1 t2
;;

