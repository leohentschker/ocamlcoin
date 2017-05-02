open Mining.Miner
open Payments
exception EmptyNetwork

val c_DEFAULT_COIN_PORT : int
val is_valid_ip : string -> bool
val get_private_ip : unit -> string

module OcamlcoinNetwork :
  sig
    val run : unit -> unit
    class ocamlcoin_node : string -> int -> object
      method ip : string
      method port : int
      method send_message : string -> bool
      method to_json : Yojson.Basic.json
      method serialize : string
    end
    val broadcast_to_node : Yojson.Basic.json -> ocamlcoin_node -> unit
    val attach_broadcast_listener : (Yojson.Basic.json -> ocamlcoin_node -> unit) -> unit
    val json_to_ocamlcoin_node : Yojson.Basic.json -> ocamlcoin_node
  end
