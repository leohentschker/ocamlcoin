open Payments
open Mining
exception EmptyNetwork

val c_DEFAULT_COIN_PORT : int
val is_valid_ip : string -> bool
val get_private_ip : unit -> string

module OcamlcoinNetwork :
  sig
    val run : unit -> unit
    val broadcast_over_network : Yojson.Basic.json -> unit
    type peer
    class ocamlcoin_node : string -> int -> object
      method ip : string
      method port : int
      method send_message : string -> bool
      method to_json : Yojson.Basic.json
    end
    val attach_broadcast_listener : (Yojson.Basic.json -> ocamlcoin_node -> unit) -> unit
    val json_to_ocamlcoin_node : Yojson.Basic.json -> ocamlcoin_node
    val peers : ocamlcoin_node list ref
  end

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedBlock of (block * nonce)
  | BroadcastNodes of (OcamlcoinNetwork.ocamlcoin_node list)

