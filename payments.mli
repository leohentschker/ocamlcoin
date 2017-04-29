open Crypto.Signature

class transaction : pub_key -> pub_key -> float -> object
    method to_string : string
    method originator : pub_key
    method target : pub_key
    method amount : float
  end

val string_to_transaction : string -> transaction
