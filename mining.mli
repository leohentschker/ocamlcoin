open Payments

module Miner :
  sig
    type nonce
    val nonce_to_string : nonce -> string
    val string_to_nonce : string -> nonce
    val hash_text : string -> string
    val verify : string -> int -> bool
    val mine : int -> nonce
    val mine_async : unit -> unit
    val stop_mining : unit -> unit
  end
