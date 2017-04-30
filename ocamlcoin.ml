open Networking
open Events

let run_network () =
  OcamlcoinNetwork.run ();
  OcamlcoinNetwork.attach_broadcast_listener
    (fun s ->
      print_endline s;
      match string_to_event s with
      | NewTransaction t ->
          print_endline "NEW TRANS"
      | SolvedBlock(block, nonce) ->
          print_endline "SOLVED BLOCK"
      | PingDiscovery ->
          print_endline "PING DISCOVERY"
      | BroadcastNodes(nlist) ->
          print_endline "BROADCAST NODES")

let _ = run_network () ;;
