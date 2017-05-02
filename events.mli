open Networking
open Mining.Miner
open Payments.Transaction
open Payments.Block

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedBlock of (block * nonce)
  | BroadcastNodes of (OcamlcoinNetwork.ocamlcoin_node list)

val event_to_json : network_event -> Yojson.Basic.json
val json_to_event : Yojson.Basic.json -> network_event
