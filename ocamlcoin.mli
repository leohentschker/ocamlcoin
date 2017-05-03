open Networking.OcamlcoinNetwork
open Events

exception NodeNotFound

module OcamlcoinRunner :
  sig
    val run : unit -> unit
    val broadcast_event_over_network : network_event -> unit
    val find_node_by_ip : string -> ocamlcoin_node
    val attach_broadcast_listener : (Yojson.Basic.json -> ocamlcoin_node -> unit) -> unit
  end
