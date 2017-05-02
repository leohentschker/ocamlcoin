module Keychain :
  sig
    type priv_key
    type pub_key
    val priv_to_string : priv_key -> string
    val string_to_priv : string -> priv_key
    val pub_to_string : pub_key -> string
    val string_to_pub : string -> pub_key
    val generate_keypair : unit -> (priv_key * pub_key)
  end

module Signature :
  sig
    type signature = Cstruct.t * Cstruct.t
    val to_string : signature -> string
    val sign : Keychain.priv_key -> string -> signature
    val verify : string -> Keychain.pub_key -> signature -> bool
  end

module type HASH = sig val hash_text : string -> string end
module MD5 : HASH
module SHA1 : HASH
module SHA224 : HASH
module SHA256 : HASH
module SHA384 : HASH
module SHA512 : HASH
