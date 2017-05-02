module IO = IOHelpers
open Networking
open Networking.OcamlcoinNetwork
open Events

let c_AVERAGE_PING_WAITTIME = 5
let c_MAX_NODE_TIMEOUT = 1000.

let random_chance a = a = Random.int (a + 1)

module OcamlcoinRunner =
  struct
    (* store the other people in our network *)
    let peers : (ocamlcoin_node * float) list ref = ref []
    let peer_nodes () = List.map fst !peers
    (* load the peers we are aware of *)
    let get_previous_nodes ?(peer_file : string = "peers.txt") () =
      List.map
        (fun peer_description ->
          match Str.split (Str.regexp ",") peer_description with
          | [ip; port] ->
              new ocamlcoin_node ip (int_of_string port)
          | _ ->
              raise (Invalid_argument "Unable to parse peer description"))
        (IO.page_lines peer_file)
    let broadcast_event event node =
      try OcamlcoinNetwork.broadcast_to_node (event_to_json event) node with
      Failure(a) -> Printf.printf "Error broadcasting to node: %s" a
    let add_peer n =
      if not(List.memq n (peer_nodes ())) then peers := (n, Unix.time ()) :: !peers
    (* ping a list of nodes *)
    let ping_nodes nlist  =
      List.iter (broadcast_event PingDiscovery) nlist
    (* update our list of stored nodes and store it in a file *)
    let update_stored_nodes () =
      (* filter out old peers *)
      peers := List.filter (fun (n, t) -> t -. Unix.time () < c_MAX_NODE_TIMEOUT) !peers;
      let file_contents = List.fold_left (fun a n -> n#serialize ^ "\n") "" (peer_nodes ()) in
      print_endline ("STORED NODES: " ^ file_contents)
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
              broadcast_event (BroadcastNodes(peer_nodes ())) node;
              add_peer node;
          | BroadcastNodes(nlist) ->
              print_endline "BROADCAST NODES";
              List.iter add_peer nlist);
      ping_nodes (get_previous_nodes ());
      let rec network_loop () =
        if random_chance c_AVERAGE_PING_WAITTIME then ping_nodes (peer_nodes ());
        update_stored_nodes ();
        Unix.sleep 5;
        network_loop () in
      network_loop ()
  end


let _ = OcamlcoinRunner.run () ;;
