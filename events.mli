open Networking
open Mining
open Payments

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedBlock of (block * nonce)
  | BroadcastNodes of (OcamlcoinNetwork.ocamlcoin_node list)

val event_to_json : network_event -> Yojson.Basic.json
val json_to_event : Yojson.Basic.json -> network_event
