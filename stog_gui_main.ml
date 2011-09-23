(** *)

open Stog_types;;

class menubar () =
  let menubar = GMenu.menu_bar () in
  let m_journal =
    let m = GMenu.menu_item ~label: "Journal" () in
    ignore(menubar#insert m ~pos: 0);
    GMenu.menu ~packing: m#set_submenu ()
  in
  let mi_save = GMenu.image_menu_item ~label: "Save" ~stock: `SAVE
    ~packing: m_journal#add ()
  in
  let () = m_journal#add (GMenu.separator_item()) in
  let mi_quit = GMenu.image_menu_item ~label: "Quit" ~stock: `QUIT
    ~packing: m_journal#add ()
  in
  object(self)
    method menubar = menubar
    method mi_save = mi_save
    method mi_quit = mi_quit
  end
;;

class stog_box stog =
  let box = GPack.vbox () in
  let paned = GPack.paned `HORIZONTAL ~packing: (box#pack ~expand: true) () in
  let art_box = new Stog_gui_arts.articles_box ~packing: paned#add1 () in
  object(self)
    method box = box#coerce

    val mutable stog = stog
    method stog = stog
    method dir = stog.stog_dir

    initializer
      art_box#set_articles (Stog_types.article_list stog)
  end
;;

class main_window stogs =
  let window = GWindow.window ~width: 800 ~height: 700 () in
  let stog_boxes = Array.of_list (List.map (new stog_box) stogs) in
  let vbox = GPack.vbox ~packing: window#add () in
  let menubar = new menubar () in
  let () = vbox#pack ~expand: false ~fill: true menubar#menubar#coerce in
  let notebook = GPack.notebook ~packing: (vbox#pack ~expand: true) () in
  object (self)

    method window = window
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

  end
;;
