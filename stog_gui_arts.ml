(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
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
open Stog_types;;

let sort_by_name column (model : GTree.model) it_a it_b =
  let a = model#get ~row:it_a ~column in
  let b = model#get ~row:it_b ~column in
  Pervasives.compare a b
;;

let sort_by_date column (model : GTree.model) it_a it_b =
  let a = model#get ~row:it_a ~column in
  let b = model#get ~row:it_b ~column in
  Pervasives.compare a b
;;

let inv_sort f model it_a it_b =
  f model it_b it_a
;;

type art_model = {
  store : GTree.list_store ;
  model : GTree.model_sort ;
  col_id : Stog_types.article_id GTree.column ;
  col_title : string GTree.column ;
  col_date : Stog_types.date GTree.column ;
  }

let make_model () =
  let cols = new GTree.column_list in
  let id_col = cols#add Gobject.Data.caml in
  let title_col = cols#add Gobject.Data.string in
  let date_col = cols#add Gobject.Data.caml in
  let l = GTree.list_store cols in
  (*print_flags "ListStore" l ;*)
  (*List.iter
    (fun (stock_id, str, vis, date) ->
      let row = l#append () in
      l#set ~row ~column:stock_id_col stock_id ;
      l#set ~row ~column:str_col str ;
      l#set ~row ~column:vis_col vis ;
      l#set ~row ~column:date_col date)
    data ;
  *)
  let sortable = GTree.model_sort l in
  (*print_flags "TreeModelSort" sortable ;*)
  let sorts =
    [ 1, sort_by_name title_col;
      2, sort_by_date date_col;
    ]
  in
  ignore
  (sortable#connect#sort_column_changed
   (fun () ->
      match sortable#get_sort_column_id with
      | None -> ()
      | Some (id, k)  ->
          try
            let f = List.assoc id sorts in
            sortable#set_sort_func id f
          with
            Not_found ->
              ()
   )
  );
  { store = l ;
    model = sortable ;
    col_id = id_col ;
    col_title = title_col ;
    col_date = date_col ;
  }


let make_view ?packing art_model =
  let col_title =
    let col = GTree.view_column ~title:"Title" () in

    let str_renderer = GTree.cell_renderer_text [ (*`FAMILY "monospace" ; *)`XALIGN 0. ] in
    col#pack str_renderer ;
    col#add_attribute str_renderer "text" art_model.col_title ;

    col#set_sort_column_id 1 ;
    col
  in

  let col_date =
    let col = GTree.view_column ~title:"Date" () in
    let str_renderer = GTree.cell_renderer_text [ `XALIGN 0. ] in
    col#pack str_renderer ;
    col#set_cell_data_func str_renderer
      (fun model row ->
       let date = model#get ~row ~column:art_model.col_date in
       str_renderer#set_properties
       [ `TEXT (Stog_types.string_of_date date) ]) ;
    col#set_sort_column_id 2 ;
    col
  in

  let v = GTree.view ~model: art_model.model ?packing () in
  ignore(v#append_column col_title);
  ignore(v#append_column col_date) ;
  v
;;

class articles_box ?packing () =
  let model = make_model () in
  let view = make_view ?packing model in
  object(self)
    val mutable selection = None

    method view = view
    method private insert_article ?(select=false) (id, art) =
      let row = model.store#append () in
      let path = model.store#get_path row in
      model.store#set ~row ~column:model.col_id id;
      model.store#set ~row ~column:model.col_title art.art_title ;
      model.store#set ~row ~column:model.col_date art.art_date;
      if select then view#selection#select_path path

    method set_articles ?path l =
      model.store#clear () ;
      match path with
        None -> List.iter self#insert_article l;
      | Some path ->
          List.iter
          (fun (id, a) ->
             let select = a.art_path = path in
             self#insert_article ~select (id, a)
          )
          l

    val mutable on_select = (fun _ -> ())
    method set_on_select
      (f : Stog_types.article_id -> unit) = on_select <- f

    val mutable on_unselect = (fun _ -> ())
    method set_on_unselect
      (f : Stog_types.article_id -> unit) = on_unselect <- f

    val mutable selection_changing = false
    method on_selection_changed () =
      if selection_changing then ()
      else
        begin
          selection_changing <- true;
          let continue =
            match selection with
              None -> true
            | Some (path, id) ->
                if view#selection#path_is_selected path then
                  true
                else
                  (
                   try
                     on_unselect id;
                     selection <- None;
                     true
                   with e ->
                       let msg =
                         match e with
                           Failure msg -> msg
                         | e -> Printexc.to_string e
                       in
                       GToolbox.message_box "Error" msg;
                       view#selection#select_path path;
                       false
                  )
          in
          if continue then
            begin
              match view#selection#get_selected_rows with
                [] -> selection <- None
              | path :: _ ->
                  let it = view#model#get_iter path in
                  let id = model.model#get ~row: it ~column: model.col_id in
                  selection <- Some (path, id);
                  on_select id
            end;
          selection_changing <- false
        end

    initializer
      ignore (view#selection#connect#changed self#on_selection_changed);
  end
;;

let make_field_table ?packing fields =
  let table = GPack.table
    ~columns:2 ~rows:(List.length fields)
    ?packing ()
  in
  let f top (text, w) =
    let label = GMisc.label ~text: (text^":") ~xpad: 2 ~xalign: 1. () in
    table#attach ~top ~left: 0 ~expand: `NONE label#coerce;
    table#attach ~top ~left: 1 ~expand: `X w#coerce;
    top+1
  in
  ignore (List.fold_left f 0 fields);
  table
;;

let targets = [
  { Gtk.target = "STRING"; flags = []; info = 0};
  { Gtk.target = "text/plain"; flags = []; info = 0};
  ];;

class file_box ?packing () =
  object(self)
    inherit [string] Gmylist.plist `SINGLE
      [None, Gmylist.String Filename.basename] false
    val mutable dir = None
    method set_dir d =
      dir <- d;
      self#update

    method update =
      match dir with
        None -> self#update_data []
      | Some dir ->
          let l = Stog_io.get_article_files dir in
          let pred s =
            not (Stog_misc.is_prefix "index." (Filename.basename s))
          in
          let l = List.filter pred l in
          self#update_data l

    method copy_to_dir ?resize f =
      match dir with
        None -> ()
      | Some dir ->
          let com = Printf.sprintf "%s %s %s/%s"
            (
             match resize with
               None -> "cp"
             | Some (w, h) ->
                 Printf.sprintf "convert -scale %dx%d" w h
            )
            (Filename.quote f) (Filename.quote dir)
            (Filename.quote (Filename.basename f))
          in
          match Sys.command com with
            0 -> self#update
          | n ->
              GToolbox.message_box "Error"
              (Printf.sprintf "command failed: %s" com)

    method remove_file file =
      match GToolbox.question_box ~title: "Question"
        ~buttons: ["Yes" ; "No"]
         (Printf.sprintf "Remove file %s ?" file)
      with
        1 -> Sys.remove file; self#update
      | _ -> ()

    method on_delete () =
      match self#selection with
        [] -> ()
      | fname :: _ -> self#remove_file fname

    initializer
      let data_get _ sel ~info ~time =
        match self#selection with
          fname :: _ -> sel#return ?typ: None ?format: None fname
        | [] -> ()
      in
      let drop context ~x ~y ~time =
        match context#targets with
        | [] -> false
        | d :: _ -> view#drag#get_data ~target:d ~time context ; false
      in
      let drop_menu data =
        let entries =
          [
            `I ("Copy and resize 500x400",
             (fun () -> self#copy_to_dir ~resize: (500, 400) data)) ;

            `I ("Copy", (fun () -> self#copy_to_dir data)) ;
          ]
        in
        GToolbox.popup_menu ~entries ~button: 1 ~time: Int32.zero
      in
      let data_received context ~x ~y data ~info ~time =
        if data#format = 8 then
          begin
            drop_menu data#data;
            context#finish ~success: true ~del:false ~time
          end
        else
          context#finish ~success:false ~del:false ~time
      in
      view#drag#source_set targets
        ~modi:[`BUTTON1 ] ~actions:[`COPY ];
      ignore(self#view#drag#connect#data_get ~callback: data_get);

      view#drag#dest_set targets ~actions:[`COPY;`MOVE];
      ignore(view#drag#connect#drop ~callback:drop);
      ignore(view#drag#connect#data_received ~callback:data_received);

      ignore
        (view#event#connect#key_press
         (fun t ->
            (List.mem (GdkEvent.Key.keyval t)
             [
               GdkKeysyms._Delete ;
             ]
            ) && (self#on_delete (); true)
         )
        );

      match packing with
        None -> ()
      | Some f -> f self#box
  end
;;

class edition_box ?packing () =
  let vbox = GPack.vbox ?packing () in
  let we_title = GEdit.entry () in
  let we_date = GEdit.entry () in
  let we_topics = GEdit.entry () in
  let we_keywords = GEdit.entry () in
  let wchk_published = GButton.check_button () in
  let fields = [
      "Title", we_title#coerce ; "Date", we_date#coerce ;
      "Topics", we_topics#coerce; "Keywords", we_keywords#coerce ;
      "Published", wchk_published#coerce ;
    ]
  in
  let paned = GPack.paned `HORIZONTAL ~packing: vbox#pack () in
  let _table = make_field_table ~packing: paned#add1 fields in
  let file_box = new file_box ~packing: paned#add2 () in
  let wscroll = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing: (vbox#pack ~expand: true ~fill: true) ()
  in
  let body_view = GSourceView2.source_view ~packing: wscroll#add () in
  object(self)
    method box = vbox

    method clear () =
      we_title#set_text "";
      we_date#set_text "";
      we_topics#set_text "";
      we_keywords#set_text "";
      wchk_published#set_active false ;
      let b = body_view#source_buffer in
      b#delete ~start: b#start_iter ~stop: b#end_iter;
      file_box#set_dir None

    method set_article a =
      we_title#set_text a.art_title;
      let (y,m,d) = a.art_date in
      we_date#set_text (Printf.sprintf "%04d/%02d/%02d" y m d);
      we_topics#set_text (String.concat ", " a.art_topics);
      we_keywords#set_text (String.concat ", " a.art_keywords);
      wchk_published#set_active a.art_published;
      let b = body_view#source_buffer in
      b#begin_not_undoable_action ();
      b#delete ~start: b#start_iter ~stop: b#end_iter;
      b#insert (Printf.sprintf "<->\n%s" a.art_body);
      b#end_not_undoable_action ();
      file_box#set_dir (Some (Filename.dirname a.art_location))

    method get_article a =
      let contents =
        Printf.sprintf
          "title: %s\ndate: %s\ntopics: %s\nkeywords: %s\npublished: %s\n%s"
        we_title#text we_date#text we_topics#text we_keywords#text
        (if wchk_published#active then "true" else "false")
        (body_view#source_buffer#get_text ())
      in
      Stog_io.read_article_main a contents

    method insert_into_body s =
      body_view#source_buffer#insert s

    initializer
      (*List.iter
        (fun l -> prerr_endline l#name)
        (Gtksv_utils.available_source_languages ());*)
      let lang_xml = Gtksv_utils.source_language_by_name "HTML" in
      Gtksv_utils.register_source_view body_view;
      Gtksv_utils.apply_sourceview_props
        body_view (Gtksv_utils.read_sourceview_props());
      Gtksv_utils.register_source_buffer body_view#source_buffer;
      Gtksv_utils.apply_source_style_scheme_to_registered_buffers
      (Gtksv_utils.read_style_scheme_selection ());
      body_view#source_buffer#set_highlight_syntax true;
      body_view#source_buffer#set_language lang_xml;
      body_view#set_wrap_mode `WORD;

  end
;;





