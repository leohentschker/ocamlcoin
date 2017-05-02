open Crypto.Signature
open Crypto.Keychain

class transaction : pub_key -> pub_key -> float -> float -> signature -> object
    method to_json : Yojson.Basic.json
    method to_string : string
    method originator : pub_key
    method target : pub_key
    method amount : float
    method timestamp : float
    method authenticated : bool
    method signature : signature
  end

val create_transaction : pub_key -> pub_key -> float -> float -> priv_key -> transaction

val json_to_transaction : Yojson.Basic.json -> transaction

class block : transaction list -> object
    method to_string : string
    method to_json : Yojson.Basic.json
    method transactions : transaction list
    method contains_transaction : transaction -> bool
  end

val json_to_block : Yojson.Basic.json -> block

exception NoUnverified
val add_unmined_transaction : transaction -> unit
val get_unmined_block : unit -> block
