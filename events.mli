open Networking.OcamlcoinNetwork
open Mining
open Payments

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedBlock of (block * nonce)
  | BroadcastNodes of (ocamlcoin_node list)
val event_to_string : network_event -> string
val string_to_event : string -> network_event
