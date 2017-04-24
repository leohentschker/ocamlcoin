module Signature :
  sig
    type priv_key
    type pub_key
    type signature
    val priv_to_string : priv_key -> string
    val pub_to_string : pub_key -> string
    val generate_keypair : unit -> (priv_key * pub_key)
  end

module type HASH = sig val hash_text : string -> string end
module MD5 : HASH
module SHA1 : HASH
module SHA224 : HASH
module SHA256 : HASH
module SHA384 : HASH
module SHA512 : HASH
