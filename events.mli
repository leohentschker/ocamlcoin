open Networking
open Mining.Miner
open Payments.Transaction
open Crypto.Keychain
open Crypto.Signature

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedTransaction of (transaction * nonce * pub_key * auth_sig)
  | BroadcastNodes of (OcamlcoinNetwork.ocamlcoin_node list)
  | BroadcastTransactions of (transaction list)

val event_to_json : network_event -> Yojson.Basic.json
val json_to_event : Yojson.Basic.json -> network_event
