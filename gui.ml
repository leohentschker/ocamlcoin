(* Follow tutorial of https://ocaml.org/learn/tutorials/introduction_to_gtk.html *)
open GdkKeysyms
open Mining
open GMain
open Payments.Transaction
open Profile
open Ocamlcoin
open Events
open Ledger

let locale = GtkMain.Main.init () ;;
let c_WINDOW_WIDTH = 320 ;;
let c_WINDOW_HEIGHT = 500 ;;
let c_YPAD = 20 ;;
let c_TITLE_FONT = "Verdana 20" ;;
let c_HEADER_FONT = "Verdana 15" ;;
let c_MINING_TEXT = "Mine OCamlcoins" ;;
let c_INVALID_AMOUNT_TEXT = "Invalid payment amount" ;;
let c_INVALID_IP_TEXT = "Cannot find IP. Try again?" ;;
let c_NO_BLOCKS_MINING_TEXT = "No blocks to mine. Try again?" ;;
let c_STOP_MINING_TEXT = "Stop Mining OCamlcoins" ;;
let c_SUCCESSFUL_TRANSACTION_TEXT = "Successful transaction. Pay again?" ;;
let c_BACKGROUND_COLOR = [(`NORMAL, (`RGB (65535, 65535, 65535)))] ;;
let c_BUTTON_COLOR = [(`NORMAL, (`RGB (66000, 113000, 255000)))] ;;

class gui =
  object (this)
    val window = GWindow.window ~width:c_WINDOW_WIDTH ~height:c_WINDOW_HEIGHT
                                ~title:"OCamlcoin Wallet" ()
    val mutable balance_label = GMisc.label ()
    (* store UI elements for payment functionality *)
    val mutable payment_target_edit = GEdit.entry ()
    val mutable payment_total_edit = GEdit.entry ()
    (* variabeles for mining *)
    val mutable mining_button = GButton.button ()
    val mutable payment_button = GButton.button ()
    (* User toggled the mining button *)
    method toggle_mining () =
      if Miner.currently_mining () then
        let _ = Miner.stop_mining () in
        mining_button#set_label c_MINING_TEXT
      else
        try
          print_endline "START MINING";
          mining_button#set_label c_STOP_MINING_TEXT;
          Miner.mine_async ()
        with Payments.NoUnverified ->
          mining_button#set_label c_NO_BLOCKS_MINING_TEXT
    method make_payment () =
      let target_ip = payment_target_edit#text in
      try
        let amount = float_of_string payment_total_edit#text in
        try
          let target = OcamlcoinRunner.find_node_by_ip target_ip in
          OcamlcoinRunner.broadcast_event_over_network
            (NewTransaction(create_transaction User.public_key
                              target#pub amount (Unix.time ())
                              User.private_key));
          payment_button#set_label c_SUCCESSFUL_TRANSACTION_TEXT
          
        with NodeNotFound ->
          payment_target_edit#set_text c_INVALID_IP_TEXT
      with Failure float_of_string ->
        payment_total_edit#set_text c_INVALID_AMOUNT_TEXT
    method mining_solution_listener (t : transaction) (n : Miner.nonce) =
      OcamlcoinRunner.broadcast_event_over_network (SolvedTransaction(t, n));
      mining_button#set_label "You solved a block! Mine again?"
    method initialize () =
      (* Kill the program when we close the window *)
      let _ = window#connect#destroy ~callback:Main.quit in

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
      payment_total_edit <- GEdit.entry ~text:"Number of OCamlcoins"
                                           ~packing:payment_vbox#pack ();
      payment_target_edit <- GEdit.entry ~text:"IP of person to pay"
                                         ~packing:payment_vbox#pack ();
      payment_button <- GButton.button ~label:"Make Payment"
                                          ~packing:payment_vbox#pack ();
      payment_button#misc#modify_bg c_BUTTON_COLOR;
      let _ = payment_button#connect#clicked ~callback: this#make_payment in
      (* mining UI elements *)
      let mining_vbox = GPack.vbox ~packing:vbox#pack ~border_width:20 () in
      mining_button <- GButton.button ~label:"Mine OCamlcoins"
                                         ~packing:mining_vbox#pack ();
      mining_button#misc#modify_bg c_BUTTON_COLOR;
      let _ = mining_button#connect#clicked ~callback: this#toggle_mining in

      (* Display the windows and enter Gtk+ main loop *)
      window#show ();
      (* Run the ocamlcoin code *)
      let _ = Thread.create OcamlcoinRunner.run () in
      Miner.add_solution_listener this#mining_solution_listener;
      Main.main ()
  end

let _ =
  let g = new gui in
  g#initialize ();
;;
