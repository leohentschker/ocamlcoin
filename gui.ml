(* Follow tutorial of https://ocaml.org/learn/tutorials/introduction_to_gtk.html *)
open GMain
open GdkKeysyms

let locale = GtkMain.Main.init () ;;
let c_WINDOW_WIDTH = 320 ;;
let c_WINDOW_HEIGHT = 500 ;;
let c_YPAD = 20 ;;
let c_TITLE_FONT = "Verdana 20" ;;
let c_HEADER_FONT = "Verdana 15" ;;
let c_MINING_TEXT = "Mine OCamlcoins" ;;
let c_STOP_MINING_TEXT = "Stop Mining OCamlcoins" ;;
let c_BACKGROUND_COLOR = [(`NORMAL, (`RGB (65535, 65535, 65535)))] ;;
let c_BUTTON_COLOR = [(`NORMAL, (`RGB (66000, 113000, 255000)))] ;;
open Mining ;;

class gui =
  object (this)
    val window = GWindow.window ~width:c_WINDOW_WIDTH ~height:c_WINDOW_HEIGHT
                                ~title:"OCamlcoin Wallet" ()
    val mutable balance_label = GMisc.label ()
    (* store UI elements for payment functionality *)
    val mutable payment_target_edit = GEdit.entry ()
    val mutable payment_total_edit = GEdit.entry ()
    (* variabeles for mining *)
    val mutable mining_thread : Thread.t option = None
    val mutable mining_button = GButton.button ()
    method toggle_mining () =
      if Mining.currently_mining () then
        let _ = Mining.stop_mining () in
        mining_button#set_label c_STOP_MINING_TEXT
      else
        Mining.mine_async "asd";
        mining_button#set_label c_MINING_TEXT
    method make_payment () =
      print_endline (payment_target_edit#text ^ " " ^ payment_total_edit#text)
    method initialize () =
      (* Kill the program when we close the window *)
      window#connect#destroy ~callback:Main.quit;

      (* make the background of the window white *)
      window#misc#modify_bg c_BACKGROUND_COLOR;
      let vbox = GPack.vbox ~packing:window#add () in
      let title = GMisc.label ~text:"Welcome to OCamlcoin!" ~ypad:c_YPAD
                              ~packing:vbox#pack () in
      title#misc#modify_font_by_name c_TITLE_FONT;

      (* set the balance *)
      balance_label <- GMisc.label ~text:"Current balance: 13.23 OCamlcoins"
                                   ~ypad:c_YPAD ~packing:vbox#pack ();
      balance_label#misc#modify_font_by_name c_HEADER_FONT;
      (* payment UI elements *)
      let payment_vbox = GPack.vbox ~packing:vbox#pack ~border_width:20 () in
      let payment_label = GMisc.label ~text:"Make Payment" ~ypad:c_YPAD
                                      ~packing:payment_vbox#pack () in
      payment_label#misc#modify_font_by_name c_HEADER_FONT;
      let _ = GEdit.entry ~text:"Number of OCamlcoins"
                          ~packing:payment_vbox#pack () in
      payment_target_edit <- GEdit.entry ~text:"IP of person to pay"
                                         ~packing:payment_vbox#pack ();
      let payment_button = GButton.button ~label:"Make Payment"
                                          ~packing:payment_vbox#pack () in
      payment_button#misc#modify_bg c_BUTTON_COLOR;
      payment_button#connect#clicked ~callback: this#make_payment;
      (* mining UI elements *)
      let mining_vbox = GPack.vbox ~packing:vbox#pack ~border_width:20 () in
      mining_button <- GButton.button ~label:"Mine OCamlcoins"
                                         ~packing:mining_vbox#pack ();
      mining_button#misc#modify_bg c_BUTTON_COLOR;
      mining_button#connect#clicked ~callback: this#toggle_mining;

      (* Display the windows and enter Gtk+ main loop *)
      window#show ();
      Main.main ()
  end
let _ = let g = new gui in g#initialize () ;;