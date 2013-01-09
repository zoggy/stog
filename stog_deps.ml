(** *)

open Stog_types;;

module Smap = Stog_types.Str_map;;
type dependency = File of string | Elt of string;;

module Depset =
  Set.Make (struct type t = dependency let compare = Pervasives.compare end)

let deps = ref Smap.empty;;

let add_dep elt dep =
  match elt.elt_type with
    "by-keyword" | "by-month" | "by-topic" -> ()
  | _ ->
      let hid = Stog_types.string_of_human_id elt.elt_human_id in
      let set =
        try Smap.find hid !deps
        with Not_found -> Depset.empty
      in
      let set = Depset.add dep set in
      deps := Smap.add hid set !deps
;;

let string_of_file_time f =
  match Stog_misc.file_mtime f with
    None -> "<notime>"
  | Some t -> Stog_misc.string_of_time t
;;

let string_of_elt_time stog hid =
  try
    let hid = Stog_types.human_id_of_string hid in
    let (_,elt) = Stog_types.elt_by_human_id stog hid in
    let src_file = Filename.concat stog.stog_dir elt.elt_src in
    match Stog_misc.file_mtime src_file with
      None -> "<notime>"
    | Some t -> Stog_misc.string_of_time t
  with
    e -> Printexc.to_string e
;;

let max_deps_date stog hid =
  let rec f dep acc =
    if Depset.mem dep acc then
      acc
    else
      let acc = Depset.add dep acc in
      match dep with
        File file -> acc
      | Elt hid ->
          try
            let elt_deps = Smap.find hid !deps in
            Depset.fold f elt_deps acc
          with Not_found ->
              acc
  in
  let deps = f (Elt hid) Depset.empty in
  prerr_endline (Printf.sprintf "%S depends on" hid);
  let print = function
    File file ->
      prerr_endline
      (Printf.sprintf "  File %S modified at %s" file (string_of_file_time file))
  | Elt hid ->
      prerr_endline
      (Printf.sprintf "  Elt %S modified at %s" hid (string_of_elt_time stog hid))
  in
  Depset.iter print deps;
  let max_date dep acc =
    let date_opt =
      match dep with
        File file -> Stog_misc.file_mtime file
      | Elt hid ->
          try
            let (_,elt) = Stog_types.elt_by_human_id stog
              (Stog_types.human_id_of_string hid)
            in
            let src = Filename.concat stog.stog_dir elt.elt_src in
            Stog_misc.file_mtime src
          with Failure _ ->
              None
    in
    match date_opt with
      None ->
        (* the element which we previously depended on does not exist;
           use current date to force recomputing *)
        Unix.time ()
    | Some date ->
        max acc date
  in
  Depset.fold max_date deps 0.
;;