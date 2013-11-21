(** Stog plugin example. *)

let fun_list stog env args subs =
  (* get the optional sep attribute ... *)
  let sep = Xtmpl.opt_arg args ("", "sep") in
  (* reverse the separator, for final list will be reversed *)
  let sep = List.rev sep in
  (* then insert the separator between all children of the node *)
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
      let acc =
        match acc with
          [] -> [h]
        | _ -> h :: sep @ acc
      in
      iter acc q
  in
  (* and finally return the list of xml trees *)
  (stog, iter [] subs)
;;

(* register the new function, associated to tag "list".
  Before stog 0.3, this function was called [Stog_plug.register_fun]. *)
let () = Stog_plug.register_html_base_rule ("", "list") fun_list;;

