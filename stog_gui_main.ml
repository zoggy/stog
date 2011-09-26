(** *)

open Stog_types;;

class menubar () =
  let menubar = GMenu.menu_bar () in
  let (mi_journal, m_journal) =
    let m = GMenu.menu_item ~label: "Journal" () in
    ignore(menubar#insert m ~pos: 0);
    (m, GMenu.menu ~packing: m#set_submenu ())
  in
  let mi_new_art = GMenu.image_menu_item ~label: "New article" ~stock: `NEW
    ~packing: m_journal#add ()
  in
  let mi_save = GMenu.image_menu_item ~label: "Save" ~stock: `SAVE
    ~packing: m_journal#add ()
  in
  let () = m_journal#add (GMenu.separator_item()) in
  let mi_quit = GMenu.image_menu_item ~label: "Quit" ~stock: `QUIT
    ~packing: m_journal#add ()
  in

  let (mi_article, m_article) =
    let m = GMenu.menu_item ~label: "Article" () in
    ignore(menubar#insert m ~pos: 1);
    (m, GMenu.menu ~packing: m#set_submenu ())
  in
  let mi_insert_p = GMenu.image_menu_item ~label: "Insert paragraph"
    ~packing: m_article#add ()
  in
  object(self)
    method menubar = menubar
    method mi_new_art = mi_new_art
    method mi_save = mi_save
    method mi_quit = mi_quit
    method mi_article = mi_article
    method mi_insert_p = mi_insert_p
  end
;;

class stog_box menubar stog =
  let box = GPack.vbox  () in
  let paned = GPack.paned `HORIZONTAL ~packing: (box#pack ~expand: true) () in
  let art_box = new Stog_gui_arts.articles_box ~packing: paned#add1 () in
  let edit_box = new Stog_gui_arts.edition_box ~packing: paned#add2 () in
  object(self)
    method box = box#coerce

    val mutable stog = stog
    method stog = stog
    method set_stog s = stog <- s
    method dir = stog.stog_dir

    val mutable selected_art = None
    method selected_art = selected_art

    method update_selected_article_from_edit_box =
      match selected_art with
        None -> ()
      | Some art_id ->
          let a0 = Stog_types.article stog art_id in
          let a = edit_box#get_article a0 in
          if a <> a0 then
            (
             let stog = Stog_types.set_article stog art_id a in
             self#set_stog stog
            )

    method on_unselect art_id =
      (match selected_art with
         Some id when id = art_id -> ()
       | None -> prerr_endline "unselecting article but no article in selected_art"
       | Some id -> prerr_endline "selected_art <> art_id in on_unselect");
      self#update_selected_article_from_edit_box ;
      edit_box#clear ();
      selected_art <- None;
      menubar#mi_article#misc#set_sensitive false

    method set_stog st =
      stog <- st ;
      begin
        match selected_art with
          None -> ()
        | Some id ->
            self#on_unselect id
      end;
      art_box#set_articles (Stog_types.article_list stog)

    method insert_into_body = edit_box#insert_into_body

    initializer
      art_box#set_articles (Stog_types.article_list stog);

      art_box#set_on_select
      (fun id ->
         selected_art <- Some id;
         menubar#mi_article#misc#set_sensitive true;
         let a = Stog_types.article stog id in
         edit_box#set_article a
      );

      art_box#set_on_unselect self#on_unselect;
  end
;;

class main_window stogs =
  let window = GWindow.window ~width: 1100 ~height: 700 () in
  let vbox = GPack.vbox ~packing: window#add () in
  let menubar = new menubar () in
  let stog_boxes = Array.of_list (List.map (new stog_box menubar) stogs) in
  let () = vbox#pack ~expand: false ~fill: true menubar#menubar#coerce in
  let notebook = GPack.notebook ~packing: (vbox#pack ~expand: true) () in
  object (self)

    method window = window

    method on_new_article stog_box =
      match GToolbox.input_string "New article"
        "Enter the title:"
      with
        None -> ()
      | Some title ->
          let title = Stog_misc.strip_string (Stog_gui_misc.of_utf8 title) in
          let stog = stog_box#stog in
          let human_id = Stog_types.make_human_id stog title in
          let art = Stog_types.dummy_article () in
          let art = { art with art_title = title ; art_human_id = human_id } in
          let stog = Stog_types.add_article stog art in
          stog_box#set_stog stog

    method on_save stog_box =
      stog_box#update_selected_article_from_edit_box ;
      Stog_io.write_stog stog_box#stog

    method on_current_stog f () =
      let page = notebook#current_page in
      let stog_box =
        try stog_boxes.(page)
        with Failure _ | Invalid_argument _ ->
          failwith (Printf.sprintf "Bad notebook page number (%d)" page)
      in
      f stog_box

    method on_insert s =
      self#on_current_stog (fun sb -> sb#insert_into_body s)

    initializer
      let f_append st_box =
        let label = GMisc.label
          ~text: (Stog_gui_misc.to_utf8 st_box#dir)
          ()
        in
        ignore(notebook#append_page ~tab_label: label#coerce st_box#box)
      in
      Array.iter f_append stog_boxes;
      ignore(window#connect#destroy GMain.Main.quit);
      ignore(menubar#mi_quit#connect#activate window#destroy);
      ignore(menubar#mi_save#connect#activate (self#on_current_stog self#on_save));
      ignore(menubar#mi_new_art#connect#activate (self#on_current_stog self#on_new_article));

      ignore(menubar#mi_insert_p#connect#activate
       (self#on_insert "<p>\n\n</p>"));

      ignore(notebook#connect#switch_page
        (fun n -> let sb = stog_boxes.(n) in
          menubar#mi_article#misc#set_sensitive
          (sb#selected_art <> None)))
  end
;;
