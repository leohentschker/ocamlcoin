open Payments.Transaction

module Miner :
  sig
    type nonce
    val add_solution_listener : (transaction -> nonce -> unit) -> unit
    val currently_mining : unit -> bool
    val stop_mining : unit -> unit
    val nonce_to_string : nonce -> string
    val string_to_nonce : string -> nonce
    val hash_text : string -> string
    val verify : string -> int -> bool
    val mine : transaction -> int -> nonce
    val mine_async : unit -> unit
    val stop_mining : unit -> unit
    val generate_fake_nonce : unit -> nonce
    val test_mining : unit -> unit
  end
