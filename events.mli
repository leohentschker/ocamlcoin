open Networking
open Mining.Miner
open Payments.Transaction

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedTransaction of (transaction * nonce)
  | BroadcastNodes of (OcamlcoinNetwork.ocamlcoin_node list)
  | BroadcastTransactions of (transaction list)

val event_to_json : network_event -> Yojson.Basic.json
val json_to_event : Yojson.Basic.json -> network_event
