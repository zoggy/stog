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

  Algorithm adapted from
  "Tree to tree correction for document trees"
  by Barnart, Clarke and Duncan. Technical report.

  We implement the first extension of Zhang and Shasha and remove the
  InsertNode and DeleteNode: they can be replaced by InsertTree and
  DeleteTree (even if not exactly the same) and the semantic of InsertNode
  is not clear: where do we insert ? Besides, it is easier to write
  the cost function if there is only one operation to insert and
  one to delete.
*)

module Nmap = Xtmpl.Name_map

type name = Xmlm.name
type 'a xmlt =
    E of name * string Nmap.t * 'a list
  | D of string
type xmltree = xmltree xmlt
type xmlnode = int option * xmlnode xmlt

type label = Node of string | Text of string

type node = {
  number : int ;
  leftmost : int ;
  keyroot : bool ;
  child : int array ;
  parent : int option ;
  xml : xmltree ;
  size : int ;
  label : label ;
  }

let xmlnode_of_t t =
  let len = Array.length t in
  let rec build n =
    let xml = t.(n).xml in
    match xml with
      D s -> (Some n, D s)
    | E (tag,atts,_) ->
        let children = List.map build (Array.to_list t.(n).child) in
        (Some n, E (tag, atts, children))
  in
  build (len-1)
;;

type cost = int

type operation =
  | Replace of node * int
  | InsertTree of node * int (* insert tree from t2 as a right sibling of [int] in t1 *)
  | DeleteTree of node (* delete tree from t1 *)
  | Edit of node * node (* change label of node from t1 to label of node from t2 *)

type cost_fun = operation -> cost

type actions = cost * operation list

type patch_path =
  Path_cdata of int
| Path_node of Xmlm.name * int * patch_path option

type patch_operation =
  PInsertTree of xmltree
| PDeleteTree
| PUpdateCData of string
| PUpdateNode of Xmlm.name * string Nmap.t * xmltree list
| PReplace of xmltree

type patch = (patch_path * patch_operation) list

let min_action (c1, l1) (c2, l2) =
  if c1 <= c2 then (c1, l1) else (c2, l2)

let add_action (c,l) fc action =
  let ca = fc action in
  if ca = 0 then (c, l) else (c+ca, action :: l)
;;

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
  xml_of_source s (`String (0, s))
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
(*
let size =
  let rec iter acc = function
    E (_,_,subs) -> List.fold_left iter (acc+1) subs
  | _ -> acc+1
  in
  iter 0
;;
*)

let atts_of_xml_atts map =
  List.rev
    (Nmap.fold
     (fun name s acc -> (name, s) :: acc)
       map [])
;;

let string_of_xml ?(cut=false) tree =
  let tree =
    if cut then
      match tree with
        D _ -> tree
      | E(name,atts,_) -> E(name,atts,[])
    else
      tree
  in
  let b = Buffer.create 256 in
  let ns_prefix s = Some s in
  let output = Xmlm.make_output ~ns_prefix ~decl: false (`Buffer b) in
  let frag = function
  | E (tag, atts, childs) ->
      let atts = atts_of_xml_atts atts in
      `El ((tag, atts), childs)
  | D d -> `Data d
  in
  Xmlm.output_doc_tree frag output (None, tree);
  Buffer.contents b
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
  let rec iter (n0, acc, acc_children) xml =
    let subs = match xml with
        E (_, _, l) -> l
      | D _ -> []
    in
    let (n, acc, children) = List.fold_left iter (n0, acc, []) subs in
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
        size = n - n0 + 1 ;
      }
    in
    let children = List.map (fun node -> { node with parent = Some n }) children in
    let acc = match children with [] -> acc | _ -> acc @ children in
    (n+1, acc, acc_children @ [node])
  in
  fun xml ->
    let (_, l, root) = iter (1, [], []) xml in
    let t = Array.of_list (l @ root) in
    Array.sort (fun n1 n2 -> n1.number - n2.number) t;
    let t = Array.append
      [| {
         number = -1 ; leftmost = -1 ; keyroot = false ;
         child = [| |] ; parent = None ;
         xml = D ""; size = 0 ; label = Text "dummy" } ; |]
        t
    in
    (* set root node as keyroot *)
    let len = Array.length t in
    t.(len-1) <- { t.(len-1) with keyroot = true };

    Array.iteri (fun i node ->
       (*
       prerr_endline (Printf.sprintf "i=%d, keyroot=%b, node.number=%d, parent=%s, xml=%s" i node.keyroot node.number
         (match node.parent with None -> "" | Some n -> string_of_int n)
         (short_label node.xml)
       );*)
       if i > 0 then assert (i = node.number)
    ) t;
    t
;;

let compute fc t1 t2 =
  (* forest distance *)
  let fd = Array.init (Array.length t1) (fun i -> Array.create (Array.length t2) (0,[])) in
  (* tree distance *)
  let d = Array.init (Array.length t1) (fun i -> Array.create (Array.length t2) (0,[])) in

  for x = 1 to Array.length t1 - 1 do
    match t1.(x).keyroot with
      false -> ()
    | true ->
        let lx = t1.(x).leftmost in
        for y = 1 to Array.length t2 - 1 do
          match t2.(y).keyroot with
            false -> ()
          | true ->
              let ly = t2.(y).leftmost in
              (*prerr_endline  (Printf.sprintf "lx=%d, ly=%d" lx ly);*)
              fd.(lx - 1).(ly - 1) <- (0, []);
              for i = lx to x do
                let op = DeleteTree t1.(i) in
                fd.(i).(ly - 1) <- add_action
                  fd.(t1.(i).leftmost - 1).(ly - 1)
                  fc op
              done;
              for j = ly to y do
                let op = InsertTree (t2.(y), lx - 1) in
                fd.(lx - 1).(j) <- add_action
                  fd.(lx - 1).(t2.(j).leftmost - 1)
                  fc op
              done;
              for i = lx to x do
                for j = ly to y do
                  let li = t1.(i).leftmost in
                  let lj = t2.(j).leftmost in
                  let op_deletetree = DeleteTree(t1.(i)) in
                  let cost_deletetree = add_action
                    fd.(li-1).(j)
                    fc op_deletetree
                  in
                  let op_inserttree = InsertTree(t2.(j), i) in
                  let cost_inserttree = add_action
                    fd.(i).(lj-1)
                    fc op_inserttree
                  in
                  let part = min_action cost_inserttree cost_deletetree in
                  if li = lx && lj = ly then
                    (
                     let op_edit = Edit (t1.(i), t2.(j)) in
                     let cost_edit = add_action
                       fd.(i-1).(j-1) fc op_edit
                     in
                     d.(i).(j) <- min_action part cost_edit;
                     fd.(i).(j) <- d.(i).(j)
                    )
                  else
                    (
                     fd.(i).(j) <- min_action
                       part
                       (add_actions d.(i).(j) fd.(li-1).(lj-1) )
                    )
                done
              done
        done;
  done;
  fd.(Array.length t1 - 1).(Array.length t2 - 1)
;;

let string_of_action = function
| Replace (n2, i) -> Printf.sprintf "Replace (%d, %d): %s" n2.number i (string_of_xml ~cut:true n2.xml)
| InsertTree (n2, i) -> Printf.sprintf "InsertTree (%d, %d): %s" n2.number i (string_of_xml ~cut:true n2.xml)
| DeleteTree n1 -> Printf.sprintf "DeleteTree(%d): %s" n1.number (string_of_xml ~cut: true n1.xml)
| Edit (n1, n2) -> Printf.sprintf "Edit(%d,%d): %s -> %s" n1.number n2.number
  (string_of_xml ~cut: true n1.xml) (string_of_xml ~cut: true n2.xml)
;;

type cur_path = N of Xmlm.name | CData
module Cur_path = Map.Make (struct type t = cur_path let compare = Pervasives.compare end)
let cur_path_get cp map =
  try Cur_path.find cp map
  with Not_found -> 0
;;
let cur_path_inc cp map =
  let n = cur_path_get cp map in
  Cur_path.add cp (n+1) map
;;

let patch_path_of_cur_path_list =
  let rec iter (cp, n) acc =
    match acc, cp with
      (None, CData) -> Some (Path_cdata n)
    | (Some _, CData) -> assert false
    | (_, N name) -> Some (Path_node (name, n, acc))
  in
  fun l ->
    match List.fold_right iter l None with
      None -> assert false
    | Some p -> p
;;

let path_of_id =
  let cp_of_xml = function
    D s -> CData
  | E (name,_,_) -> N name
  in
  let rec iter i path cur_path = function
  | (Some j, xml) when i = j ->
      begin
        let cp = cp_of_xml xml in
        let path = (cp, cur_path_get cp cur_path) :: path in
        patch_path_of_cur_path_list (List.rev path)
      end
  | (Some j, _) when j < i -> raise Not_found
  | (_, D _) -> raise Not_found
  | (_, E (name, atts, subs)) ->
      (* None or Some j with j > i, let's go down after
         adding cur_path to path
         *)
      let cpt = cur_path_get (N name) cur_path in
      let path = (N name, cpt) :: path in
      iter_list i path Cur_path.empty subs

  and iter_list i path cur_path = function
    [] -> raise Not_found
  | h :: q ->
    try iter i path cur_path h
    with Not_found ->
      let cur_path =
        let cp = cp_of_xml (snd h) in
        cur_path_inc cp cur_path
      in
      iter_list i path cur_path q
  in
  fun xmlnode i ->
    try iter i [] Cur_path.empty xmlnode
    with Not_found ->
      let msg = "Id "^(string_of_int i)^" not found" in
      failwith msg
;;

let rec xmlnode_of_xmltree = function
  D s -> (None, D s)
| E (name,atts,subs) ->
  (None, E (name,atts, List.map xmlnode_of_xmltree subs))
;;

let patch_xmlnode t path op =
  let apply xml op =
    match xml, op with
    | _, PReplace tree -> [xmlnode_of_xmltree tree]
    | _, PInsertTree tree -> [xml] @ [xmlnode_of_xmltree tree]
    | _, PDeleteTree -> []
    | (x, _), PUpdateCData s -> [(x, D s)]
    | (x, D _), PUpdateNode (name, atts, _) -> [x, E (name,atts,[])]
    | (x, E(_,_,subs)), PUpdateNode (name, atts, _) -> [x, E(name,atts,subs)]
  in
  let rec iter xmls path =
    match xmls, path with
      ((x, D _) as xml):: q, Path_cdata 0 -> (apply xml op) @ q
    | (x, D s) :: q, Path_cdata n -> (x, D s) :: iter q (Path_cdata (n-1))
    | ((x, E (name,atts,subs) as xml) :: q, Path_node (name2, n, next)) when name = name2 ->
        if n = 0 then
          (match next with
             None -> (apply xml op) @ q
           | Some p ->  [x, E (name, atts, iter subs p)] @ q
          )
        else
          xml :: iter q (Path_node (name2, n-1, next))
    | xml :: q, p ->
        xml :: iter q p
    | [], _ -> assert false
  in
  match iter [t] path with
    [t] -> t
  | _ -> assert false
;;


let patch_of_action (t1, patch) = function
| Replace (n2, i) ->
    let xmltree2 = n2.xml in
    let path = path_of_id t1 i in
    let op = PReplace xmltree2 in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| InsertTree (n2, i) ->
    let xmltree2 = n2.xml in
    let path = path_of_id t1 i in
    let op = PInsertTree xmltree2 in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| DeleteTree i ->
    let path = path_of_id t1 i.number in
    let op = PDeleteTree in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| Edit (n1, n2) ->
    let path = path_of_id t1 n1.number in
    let op =
      match n1.xml, n2.xml with
        _ , D s2 -> PUpdateCData s2
      | E(_,_,_), E(name,atts,_) -> PUpdateNode (name, atts, [])
      | D _, E(name,atts,subs) -> PUpdateNode (name, atts, subs)
    in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
;;

let rec xmltree_of_xmlnode = function
  (_, D s) -> D s
| (_, E (tag,atts,subs)) -> E (tag, atts, List.map xmltree_of_xmlnode subs)
;;

let mk_replace =
  let rec iter acc = function
  | [] -> List.rev acc
  | InsertTree(n2,i) :: DeleteTree j :: q when j.number = i ->
      iter (Replace (n2, i) :: acc) q
  | h :: q ->
      iter (h :: acc) q
  in
  iter []
;;

let patch_of_actions t1 t2 l =
  let t1 = xmlnode_of_t t1 in
  let actions = mk_replace l in
  let (t1, l) = List.fold_left patch_of_action (t1, []) actions in
  let t1 = xmltree_of_xmlnode t1 in
  let t2 = xmltree_of_xmlnode (xmlnode_of_t t2) in
  Stog_misc.file_of_string ~file: "/tmp/xml1.xml" (string_of_xml t1);
  Stog_misc.file_of_string ~file: "/tmp/xml2.xml" (string_of_xml t2);
  List.rev l
;;

let rec string_of_path = function
  Path_cdata n -> "CData("^(string_of_int n)^")"
| Path_node (name, n, next) ->
    let s = (string_of_name name)^"("^(string_of_int n)^")" in
    match next with
      None -> s
    | Some p -> s^"/"^(string_of_path p)
;;

let string_of_patch_operation (path, op) =
  match op with
  | PReplace xmltree ->
      "REPLACE("^(string_of_path path)^", "^(string_of_xml xmltree)^")"
  | PInsertTree xmltree ->
      "INSERT_TREE("^(string_of_path path)^", "^(string_of_xml xmltree)^")"
  | PDeleteTree ->
      "DELETE_TREE("^(string_of_path path)^")"
  | PUpdateCData s ->
      Printf.sprintf "UPDATE_CDATA(%s, %S)" (string_of_path path) s
  | PUpdateNode (name, atts, subs) ->
      Printf.sprintf "UPDATE_NODE(%s, %S, _, _)" (string_of_path path) (string_of_name name)
;;

let string_of_patch l =
  String.concat "\n" (List.map string_of_patch_operation l)
;;

let diff xml1 xml2 =
  let t1 = t_of_xml xml1 in
  let t2 = t_of_xml xml2 in
  Stog_misc.file_of_string ~file: "/tmp/t1.dot" (dot_of_t t1);
  Stog_misc.file_of_string ~file: "/tmp/t2.dot" (dot_of_t t2);

  let fc = function
    InsertTree (j,_) -> j.size
  | DeleteTree i -> max (i.size / 4) 1
  | Edit (n1, n2) ->
      if n1.label = n2.label then 0 else 1
  | Replace _ -> 1
  in
  let cost, actions = compute fc t1 t2 in
  prerr_endline ("actions="^(String.concat "\n" (List.map string_of_action actions)));
  let patch = patch_of_actions t1 t2 actions in
  (cost, patch)
;;

