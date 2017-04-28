module Mining :
  sig
    type nonce
  	val hash_text : string -> string
    val verify : string -> int -> bool
    val mine : string -> int -> nonce
    val mine_async : string -> unit
    val currently_mining : unit -> bool
    val stop_mining : unit -> unit
  end