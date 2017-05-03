module IO = IOHelpers
open Networking
open Networking.OcamlcoinNetwork
open Events
open Profile

let c_AVERAGE_PING_WAITTIME = 5
let c_MAX_NODE_TIMEOUT = 1000.
let random_chance a = a = Random.int (a + 1)

exception EmptyNetwork

module OcamlcoinRunner =
  struct
    (* store the other people in our network *)
    let peer_tuples : (ocamlcoin_node * float) list ref = ref []
    let get_peers = fun () -> List.map fst !peer_tuples
    (* load the peers we are aware of *)
    let broadcast_event event node =
      try OcamlcoinNetwork.broadcast_to_node (event_to_json event) node with
      Failure(a) ->
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
      Bank.export_ledger (Bank.ledger)
      
    (* run everything! *)
    let run () =
      OcamlcoinNetwork.run ();
      OcamlcoinNetwork.attach_broadcast_listener
        (fun json node ->
          match json_to_event json with
          | NewTransaction t ->
              print_endline "NEW TRANS";
              if Bank.verify_transaction t Bank.ledger then
                Payments.add_unmined_transaction t
          | SolvedTransaction(t, nonce) ->
              print_endline "SOLVED BLOCK";
              Bank.add_transaction t Bank.ledger
          | PingDiscovery ->
              add_peer node;
              (match get_peers () with
              | _h :: _t as nlist ->
                  broadcast_event (BroadcastNodes(nlist)) node
              | [] -> ());
              (match Bank.get_transactions(Bank.ledger) with
              | _h :: _t as tlist ->
                  broadcast_event (BroadcastTransactions(tlist)) node
              | [] -> ());
          | BroadcastNodes(nlist) ->
              List.iter add_peer nlist
          | BroadcastTransactions(tlist) ->
              List.iter (fun t -> Bank.add_transaction t Bank.ledger) tlist);
      peer_tuples := List.map (fun p -> (p, Unix.time ())) (User.stored_nodes);
      ping_peers ();
      let rec network_loop () =
        Unix.sleep 10;
        if random_chance c_AVERAGE_PING_WAITTIME then ping_peers ();
        update_stored_nodes ();
        store_state ();
        network_loop () in
      network_loop ()
  end

let _ = OcamlcoinRunner.run () ;;
