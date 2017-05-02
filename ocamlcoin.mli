open Events

module OcamlcoinRunner :
  sig
    val run : unit -> unit
    val broadcast_event_over_network : network_event -> unit
  end
