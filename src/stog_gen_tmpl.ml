
let about = [%xtmpl "tmpl/about.html"]
let blog = [%xtmpl "tmpl/blog.html"]
let doc = [%xtmpl "tmpl/doc.html"]
let download = [%xtmpl "tmpl/download.html"]
let extlink = [%blob "tmpl/extlink.png"]
let first_post = [%xtmpl "tmpl/posts/first-post.html"]
let makefile = [%blob "tmpl/Makefile"]
let index = [%xtmpl "tmpl/index.html"]
let next = [%blob "tmpl/next.png"]
let release_0_1_0 = [%xtmpl "tmpl/posts/release-0.1.0.html"]
let rss_png = [%blob "tmpl/rss.png"]
let style_css = [%blob "tmpl/style.css"]

let file_of_string ~file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let mkdir s =
  let qs = Filename.quote s in
  match Sys.command (Printf.sprintf "mkdir -p %s" qs) with
    0 -> ()
  | n -> failwith (Printf.sprintf "Could not create directory %s" qs)

let gen_file ~outdir content path =
  let file = Filename.concat outdir path in
  mkdir (Filename.dirname file) ;
  let str =
    match content with
      `Xml xmls -> Xtmpl.string_of_xmls xmls
    | `Text s -> s
  in
  file_of_string ~file str

let generate ~outdir ~sw_name =
  let files =
    [
      `Xml (about ()), "about.html" ;
      `Xml (blog ~sw_name ()), "blog.html" ;
      `Xml (doc ()), "doc.html" ;
      `Xml (download ~sw_name ()), "download.html" ;
      `Text extlink, "extlink.png" ;
      `Xml (first_post ()), "posts/first-post.html" ;
      `Xml (index ~sw_name ()), "index.html" ;
      `Text makefile, "Makefile" ;
      `Text next, "next.png" ;
      `Xml (release_0_1_0 ~sw_name ()), "posts/release-0.1.0.html" ;
      `Text rss_png, "rss.png" ;
      `Text style_css, "style.css" ;
    ]
  in
  List.iter
    (fun (xmls, path) -> gen_file ~outdir xmls path)
    files

let print_templates () = print_endline Stog_install.templates_dir; exit 0
let print_modules () = print_endline Stog_install.modules_dir; exit 0
let outdir = ref Filename.current_dir_name
let name = ref "Mysoftware"

let options = [
  "--templates", Arg.Unit print_templates, " print templates directory and exit";
  "--modules", Arg.Unit print_modules, " print modules directory and exit";
  "-d", Arg.Set_string outdir, "<dir> output to <dir> instead of current directory" ;
  "-n", Arg.Set_string name, "<name> set name of the software in the generated files" ;
  ]

let usage = Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0)

let main () =
  Arg.parse options (fun _ -> ()) usage;
  generate ~outdir: !outdir ~sw_name: !name

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main