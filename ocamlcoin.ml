module IO = IOHelpers
open Networking
open Networking.OcamlcoinNetwork
open Events

module OcamlcoinRunner =
  struct
    (* store the other people in our network *)
    let peers : ocamlcoin_node list ref = ref []
    (* load the peers we are aware of *)
    let load_peers ?(peer_file : string = "peers.txt") () =
      peers := List.map
        (fun peer_description ->
          match Str.split (Str.regexp ",") peer_description with
          | [ip; port] ->
              new ocamlcoin_node ip (int_of_string port)
          | _ ->
              raise (Invalid_argument "Unable to parse peer description"))
        (IO.page_lines peer_file)
    let broadcast_over_network msg =
      OcamlcoinNetwork.broadcast_to_nodes msg !peers
    let run () =
      OcamlcoinNetwork.run ();
      OcamlcoinNetwork.attach_broadcast_listener
        (fun json node ->
          match json_to_event json with
          | NewTransaction t ->
              print_endline "NEW TRANS"
          | SolvedBlock(block, nonce) ->
              print_endline "SOLVED BLOCK"
          | PingDiscovery ->
              print_endline ("PING DISCOVERY from ip: " ^ node#ip)
          | BroadcastNodes(nlist) ->
              print_endline "BROADCAST NODES");
      load_peers ();
      broadcast_over_network (event_to_json PingDiscovery);
      Unix.sleep 1000
  end


let _ = OcamlcoinRunner.run () ;;
