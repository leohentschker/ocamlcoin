open Crypto
open Yojson
open Signature

let c_ORIGINATOR_KEY = "originator"
let c_TARGET_KEY = "target"
let c_AMOUNT_KEY = "amount"

class transaction
    (originator : pub_key)
    (target : pub_key)
    (amount : float) =
  object
    val originator = originator
    val target = target
    val amount = amount
    method originator = originator
    method target = target
    method amount = amount
    (* following https://realworldocaml.org/v1/en/html/handling-json-data.html *)
    method to_string =
      `Assoc[(c_ORIGINATOR_KEY, `String (Signature.pub_to_string originator));
             (c_TARGET_KEY, `String (Signature.pub_to_string target));
             (c_AMOUNT_KEY, `Float (amount))] |> to_string
  end

let string_to_transaction (s : string) : transaction =
  let open Basic.Util in
  let json = Basic.from_string s in
  let originator = string_to_pub (json |> member c_ORIGINATOR_KEY |> to_string) in
  let target = string_to_pub (json |> member c_TARGET_KEY |> to_string) in
  let amount = json |> member c_AMOUNT_KEY |> to_float in
  new transaction originator target amount
