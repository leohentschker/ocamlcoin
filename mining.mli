type nonce
val hash_text : string -> string
val verify : string -> int -> bool
val mine : string -> int -> nonce
val mine_async : string -> Thread.t
