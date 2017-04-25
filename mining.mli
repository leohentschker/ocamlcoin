module Mining :
  sig
  	val hash_text : string -> string
    val verify : int -> string -> int -> bool
    val mine : string -> int -> string
  end
