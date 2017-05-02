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
    let get_peers () = List.map fst !peer_tuples
    (* load the peers we are aware of *)
    let broadcast_event event node =
      try OcamlcoinNetwork.broadcast_to_node (event_to_json event) node with
      Failure(a) -> Printf.printf "Error broadcasting to node: %s" a
    let add_peer n =
      if not(List.memq n (get_peers ())) then
        peer_tuples := (n, Unix.time ()) :: !peer_tuples
    (* ping a list of nodes *)
    let ping_peers  =
      List.iter (broadcast_event PingDiscovery) (get_peers ())
    (* update our list of stored nodes and store it in a file *)
    let update_stored_nodes () =
      (* filter out old peers *)
      peer_tuples := List.filter
        (fun (n, t) -> t -. Unix.time () < c_MAX_NODE_TIMEOUT) !peer_tuples;
      if !peer_tuples = [] then raise EmptyNetwork
    (* run everything! *)
    let run () =
      OcamlcoinNetwork.run ();
      OcamlcoinNetwork.attach_broadcast_listener
        (fun json node ->
          match json_to_event json with
          | NewTransaction t ->
              print_endline "NEW TRANS";
              if Bank.verify_transaction t then
                Payments.add_unmined_transaction t
          | SolvedBlock(block, nonce) ->
              print_endline "SOLVED BLOCK";
              List.iter Bank.add_transaction block#transactions
          | PingDiscovery ->
              print_endline ("PING DISCOVERY from ip: " ^ node#ip ^ "asd");
              broadcast_event (BroadcastNodes(get_peers ())) node;
              add_peer node;
          | BroadcastNodes(nlist) ->
              print_endline "BROADCAST NODES";
              List.iter add_peer nlist);
      peer_tuples := List.map (fun p -> (p, Unix.time ())) (User.stored_nodes);
      ping_peers;
      let rec network_loop () =
        if random_chance c_AVERAGE_PING_WAITTIME then ping_peers;
        update_stored_nodes ();
        User.export_nodes (get_peers ());
        Unix.sleep 5;
        network_loop () in
      network_loop ()
  end

let _ = OcamlcoinRunner.run () ;;
