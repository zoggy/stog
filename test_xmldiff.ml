


let diff file1 file2 =
  let xml1 = Stog_xmldiff.xml_of_file file1 in
  let xml2 = Stog_xmldiff.xml_of_file file2 in
  Stog_xmldiff.diff xml1 xml2

let usage = "Usage: "^Sys.argv.(0)^" file1 file2";;

let main () =
  let files = ref [] in
  Arg.parse [] (fun f -> files := f :: !files) usage;
  match List.rev !files with
  | [ file1 ; file2 ] ->
      let cost, patch = diff file1 file2 in
      print_endline (Printf.sprintf "cost=%d,patch=" cost);
      prerr_endline (Stog_xmldiff.string_of_patch patch)
  | _ -> failwith usage

;;

let () = Stog_misc.safe_main main;;