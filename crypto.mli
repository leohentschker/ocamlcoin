val c_PUB_JSON_KEY : string
val c_PRIV_JSON_KEY : string

module Keychain :
  sig
    type priv_key
    type pub_key
    val priv_to_string : priv_key -> string
    val string_to_priv : string -> priv_key
    val pub_to_string : pub_key -> string
    val string_to_pub : string -> pub_key
    val generate_keypair : unit -> (priv_key * pub_key)
    val run_tests : unit -> unit
  end

module Signature :
  sig
    type signature = Cstruct.t * Cstruct.t
    val signature_to_json : signature -> Yojson.Basic.json
    val json_to_signature : Yojson.Basic.json -> signature
    val sign : Keychain.priv_key -> string -> signature
    val verify : string -> Keychain.pub_key -> signature -> bool
    val run_tests : unit -> unit
  end

module type HASH =
  sig
    val hash_text : string -> string
    val run_tests : unit -> unit
  end
module MD5 : HASH
module SHA1 : HASH
module SHA224 : HASH
module SHA256 : HASH
module SHA384 : HASH
module SHA512 : HASH
