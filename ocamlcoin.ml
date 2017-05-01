open Networking
open Events

let run_network () =
  OcamlcoinNetwork.run ();
  OcamlcoinNetwork.attach_broadcast_listener
    (fun json node ->
      match json_to_event json with
      | NewTransaction t ->
          print_endline "NEW TRANS"
      | SolvedBlock(block, nonce) ->
          print_endline "SOLVED BLOCK"
      | PingDiscovery ->
          print_endline ("PING DISCOVERY from ip" ^ node#ip)
      | BroadcastNodes(nlist) ->
          print_endline "BROADCAST NODES");
  OcamlcoinNetwork.broadcast_over_network (event_to_json PingDiscovery);
  Unix.sleep 1000

let _ = run_network () ;;
