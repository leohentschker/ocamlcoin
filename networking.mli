open Crypto.Keychain
open Mining.Miner
open Payments

val c_DEFAULT_COIN_PORT : int
val is_valid_ip : string -> bool
val get_private_ip : unit -> string

module OcamlcoinNetwork :
  sig
    exception EmptyNetwork
    exception InvalidNodeJson of string
    val run : unit -> unit
    class ocamlcoin_node : string -> int -> pub_key -> object
      method ip : string
      method port : int
      method pub : pub_key
      method send_message : string -> bool
      method to_json : Yojson.Basic.json
      method serialize : string
      method equal : ocamlcoin_node -> bool
    end
    val default_node : ocamlcoin_node
    val broadcast_to_node : Yojson.Basic.json -> ocamlcoin_node -> unit
    val attach_network_listener : (string -> unit) -> unit
    val json_to_ocamlcoin_node : Yojson.Basic.json -> ocamlcoin_node
  end
