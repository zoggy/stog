(** Stog plugin example. *)

let fun_list env args subs =
  (* get the optional sep attribute ... *)
  let sep = Xtmpl.opt_arg args "sep" in
  (* and parse it as xml *)
  let xml = Xtmpl.xml_of_string sep in
  (* We can access the stog structure with !Stog_html.current_stog .
     We don't use it here.
  *)

  (* then insert the separator between all children of the node *)
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
      let acc =
        match acc with
          [] -> [h]
        | _ -> h :: xml :: acc
      in
      iter acc q
  in
  (* and finally return the list of xml trees *)
  iter [] subs
;;

(* register the new function, associated to tag "list" *)
Stog_html.plugin_funs := ("list", fun_list) :: !Stog_html.plugin_funs;;
