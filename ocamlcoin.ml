module IO = IOHelpers
open Networking
open Networking.OcamlcoinNetwork
open Events

module OcamlcoinRunner =
  struct
    (* store the other people in our network *)
    let peers : ocamlcoin_node list ref = ref []
    (* only accept certain peers as being valid *)
    let valid_peer ocamlcoin_node =
      ocamlcoin_node#ip <> "127.0.0.1"
    (* load the peers we are aware of *)
    let load_peers ?(peer_file : string = "peers.txt") () =
      let unfiltered_peers = List.map
        (fun peer_description ->
          match Str.split (Str.regexp ",") peer_description with
          | [ip; port] ->
              new ocamlcoin_node ip (int_of_string port)
          | _ ->
              raise (Invalid_argument "Unable to parse peer description"))
        (IO.page_lines peer_file) in
        peers := List.filter valid_peer unfiltered_peers
    let broadcast_event event node =
      OcamlcoinNetwork.broadcast_to_node (event_to_json event) node
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
              print_endline ("PING DISCOVERY from ip: " ^ node#ip);
              broadcast_event (BroadcastNodes(!peers)) node;
              if not(List.memq node !peers) then
                peers := node :: !peers
          | BroadcastNodes(nlist) ->
              print_endline "BROADCAST NODES");
      load_peers ();
      List.iter (broadcast_event PingDiscovery) !peers;
      Unix.sleep 1000
  end


let _ = OcamlcoinRunner.run () ;;
