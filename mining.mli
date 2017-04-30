type nonce
val nonce_to_string : nonce -> string
val string_to_nonce : string -> nonce
val hash_text : string -> string
val verify : string -> int -> bool
val mine : string -> int -> nonce
val mine_async : string -> Thread.t
