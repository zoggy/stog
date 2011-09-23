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

    let str_renderer = GTree.cell_renderer_text [ `FAMILY "monospace" ; `XALIGN 0. ] in
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
       [ `TEXT (Stog_gui_misc.to_utf8 (Stog_types.string_of_date date)) ]) ;
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
    method view = view
    method private insert_article (id, art) =
      let row = model.store#append () in
      model.store#set ~row ~column:model.col_id id;
      model.store#set ~row ~column:model.col_title (Stog_gui_misc.to_utf8 art.art_title) ;
      model.store#set ~row ~column:model.col_date art.art_date

    method set_articles l =
      model.store#clear () ;
      List.iter self#insert_article l
  end
;;
