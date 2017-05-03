module IO = IOHelpers
open Networking
open Networking.OcamlcoinNetwork
open Events
open Profile
open Payments.Transaction
open Ledger

let c_DATA_JSON_KEY = "message_data"
let c_NODE_JSON_KEY = "node"
let c_MAX_TRANSACTION_BROADCAST_SIZE = 5

let c_AVERAGE_PING_WAITTIME = 5
let c_MAX_NODE_TIMEOUT = 30.
let random_chance a = a = Random.int (a + 1)

exception InvalidBroadcast of string
exception EmptyNetwork
exception NodeNotFound

module OcamlcoinRunner =
  struct
    (* store the other people in our network *)
    let peer_tuples : (ocamlcoin_node * float) list ref =
      ref (List.map (fun p -> p, Unix.time ()) User.stored_nodes)
    let get_peers = fun () -> List.map fst !peer_tuples
    (* load the peers we are aware of *)
    let broadcast_event event node =
      let msg_json = `Assoc[(c_DATA_JSON_KEY, (event_to_json event));
                            (c_NODE_JSON_KEY, node#to_json)] in
      try
        OcamlcoinNetwork.broadcast_to_node msg_json node;
      with Failure(a) ->
        Printf.printf "Error broadcasting to node: %s\n" a
    let add_peer new_node =
      if not(List.fold_left (fun a n -> a || new_node#equal n)
                            false (get_peers ())) then
        peer_tuples := (new_node, Unix.time ()) :: !peer_tuples
    (* ping a list of nodes *)
    let ping_peers () =
      List.iter (broadcast_event PingDiscovery) (get_peers ())
    (* update our list of stored nodes and store it in a file *)
    let update_stored_nodes () =
      (* filter out old peers *)
      peer_tuples := List.filter
        (fun (n, t) -> t -. Unix.time () < c_MAX_NODE_TIMEOUT) !peer_tuples;
      if !peer_tuples = [] then raise EmptyNetwork
    let broadcast_event_over_network (e : network_event) =
      List.iter (broadcast_event e) (get_peers ())
    let store_state () =
      User.export_nodes (get_peers ());
      Bank.export_ledger (Bank.book);
      Payments.export_unverified ()
    let find_node_by_ip (ip : string) =
      let rec find_node (lst : ocamlcoin_node list) : ocamlcoin_node =
        match lst with
        | h :: t ->
            if h#ip = ip then h
            else find_node t
        | [] -> raise NodeNotFound in
      find_node (get_peers ())
    let attach_broadcast_listener f =
      OcamlcoinNetwork.attach_network_listener
        (fun s ->
          let open Yojson.Basic.Util in
          try
            let json = Yojson.Basic.from_string s in
            f (json |> member c_DATA_JSON_KEY)
              (json |> member c_NODE_JSON_KEY |> json_to_ocamlcoin_node)
          with Yojson.Json_error _ ->
            Printf.printf "Received invalid broadcast: %s\n" s)
    (* run everything! *)
    let run () =
      OcamlcoinNetwork.run ();
      attach_broadcast_listener
        (fun json node ->
          match json_to_event json with
          | NewTransaction t ->
              print_endline "NEW TRANS";
              if authenticate_transaction t Bank.book then
                 Payments.add_unmined_transaction t
          | SolvedTransaction(t, nonce, pub_key, s) ->
              print_endline "SOLVED BLOCK";
              Bank.add_transaction
              (new transaction t#originator t#target t#amount t#timestamp
                               t#signature nonce pub_key)
              Bank.book;
              Payments.remove_mined_transaction t;
          | PingDiscovery ->
              add_peer node;
              (match Bank.get_transactions(Bank.book) with
              | _h :: _t as tlist ->
                  List.iter (fun sublist ->
                    broadcast_event (BroadcastTransactions(sublist)) node)
                    (IO.chunk_list c_MAX_TRANSACTION_BROADCAST_SIZE tlist)
              | [] -> ());
              (match get_peers () with
              | _h :: _t as nlist ->
                  broadcast_event (BroadcastNodes(nlist)) node
              | [] -> ());
          | BroadcastNodes(nlist) ->
              List.iter add_peer nlist
          | BroadcastTransactions(tlist) ->
              List.iter (fun t -> Bank.add_transaction t Bank.book) tlist);
      ping_peers ();
      let rec network_loop () =
        Unix.sleep 5;
        if random_chance c_AVERAGE_PING_WAITTIME then ping_peers ();
        update_stored_nodes ();
        store_state ();
        network_loop () in
      network_loop ()
  end

let _ = OcamlcoinRunner.run ()
