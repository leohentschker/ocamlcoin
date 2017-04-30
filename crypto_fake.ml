module type HASH = sig val hash_text : string -> string end

module SHA256 =
  struct
    let hash_text s =
      s ^ "x"
  end
