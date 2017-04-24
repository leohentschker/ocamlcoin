exception EmptyNetwork

module OcamlcoinNetwork :
  sig
    val run : unit -> unit
    val terminate : unit -> unit
    val broadcast_over_network : Yojson.Basic.json -> unit
    val attach_broadcast_listener : (string -> unit) -> unit
    class ocamlcoin_node : string -> object
      method send_message : string -> bool
      method active : bool
    end
  end