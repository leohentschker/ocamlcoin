open Crypto.Signature

class transaction : pub_key -> pub_key -> float -> object
    method to_json : Yojson.Basic.json
    method to_string : string
    method originator : pub_key
    method target : pub_key
    method amount : float
  end

val json_to_transaction : Yojson.Basic.json -> transaction

class block : transaction list -> object
    method to_string : string
    method to_json : Yojson.Basic.json
  end

val json_to_block : Yojson.Basic.json -> block
