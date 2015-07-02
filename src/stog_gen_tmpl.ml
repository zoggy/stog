
type options = {
  site_title : string ;
  sw_name : string ;
  }

let about = [%xtmpl "tmpl/about.html"]
let blog = [%xtmpl "tmpl/blog.html"]
let doc = [%xtmpl "tmpl/doc.html"]
let download = [%xtmpl "tmpl/download.html"]
let extlink = [%blob "tmpl/extlink.png"]
let first_post = [%xtmpl "tmpl/posts/first-post.html"]
let makefile = [%blob "tmpl/Makefile"]
let next = [%blob "tmpl/next.png"]
let release_0_1_0 = [%xtmpl "tmpl/posts/release-0.1.0.html"]
let rss_png = [%blob "tmpl/rss.png"]
let style_css = [%blob "tmpl/style.css"]
let sw_index = [%xtmpl "tmpl/sw-index.html"]

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

let software_files options =
  let site_title = options.site_title in
  let sw_name = options.sw_name in
  [
    `Xml (about ()), "about.html" ;
    `Xml (blog ~site_title ()), "blog.html" ;
    `Xml (doc ()), "doc.html" ;
    `Xml (download ~sw_name ()), "download.html" ;
    `Text extlink, "extlink.png" ;
    `Xml (first_post ()), "posts/first-post.html" ;
    `Xml (sw_index ~site_title ~sw_name ()), "index.html" ;
    `Text makefile, "Makefile" ;
    `Text next, "next.png" ;
    `Xml (release_0_1_0 ~sw_name ()), "posts/release-0.1.0.html" ;
    `Text rss_png, "rss.png" ;
    `Text style_css, "style.css" ;
  ]

let templates = [ "software", software_files ]

let generate ~outdir ~tmpl options =
  let files =
    try List.assoc tmpl templates
    with Not_found -> failwith (Printf.sprintf "Unknown template %S" tmpl)
  in
  List.iter
    (fun (xmls, path) -> gen_file ~outdir xmls path)
    (files options)

let print_templates () = print_endline Stog_install.templates_dir; exit 0
let print_modules () = print_endline Stog_install.modules_dir; exit 0
let list_templates () =
  List.iter (fun (name, _) -> print_endline name) templates; exit 0

let outdir = ref Filename.current_dir_name
let template = ref "software"
let sw_name = ref None
let site_title = ref "Site title"

let cl_options = [
  "--templates", Arg.Unit print_templates, " print stog templates directory and exit";
  "--modules", Arg.Unit print_modules, " print stog modules directory and exit";
  "-d", Arg.Set_string outdir, "<dir> output to <dir> instead of current directory" ;
  "-t", Arg.Set_string template,
    Printf.sprintf "<s> set template to use; default is %S" !template ;

  "-l", Arg.Unit list_templates, " list available templates and exit" ;

  "--site-title", Arg.Set_string site_title,
    "<s> set site title" ;

  "--sw-name", Arg.String (fun s -> sw_name := Some s),
    "<name> set name of the software in the generated files" ;
  ]

let mk_options () =
  let sw_name = match !sw_name with None -> !site_title | Some s -> s in
  { site_title = !site_title ; sw_name }

let usage = Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0)

let main () =
  Arg.parse cl_options (fun _ -> ()) usage;
  generate ~outdir: !outdir ~tmpl: !template (mk_options())

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