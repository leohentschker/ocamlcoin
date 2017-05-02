open Crypto.Keychain

class transaction : pub_key -> pub_key -> float -> float -> object
    method to_json : Yojson.Basic.json
    method to_string : string
    method originator : pub_key
    method target : pub_key
    method amount : float
    method timestamp : float
  end

val json_to_transaction : Yojson.Basic.json -> transaction

class block : transaction list -> object
    method to_string : string
    method to_json : Yojson.Basic.json
    method transactions : transaction list
    method contains_transaction : transaction -> bool
  end

val json_to_block : Yojson.Basic.json -> block

exception NoUnverified
val add_unverified_transaction : transaction -> unit
val get_unverified_block : unit -> block
