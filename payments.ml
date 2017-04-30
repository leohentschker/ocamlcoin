open Crypto
module Y = Yojson
open Signature

let c_ORIGINATOR_KEY = "originator"
let c_TARGET_KEY = "target"
let c_AMOUNT_KEY = "amount"

class transaction
    (originator : pub_key)
    (target : pub_key)
    (amount : float) =
  object(this)
    val originator = originator
    val target = target
    val amount = amount
    method originator = originator
    method target = target
    method amount = amount
    (* following https://realworldocaml.org/v1/en/html/handling-json-data.html *)
    method to_json =
      `Assoc[(c_ORIGINATOR_KEY, `String (Signature.pub_to_string originator));
             (c_TARGET_KEY, `String (Signature.pub_to_string target));
             (c_AMOUNT_KEY, `Float (amount))]
    method to_string =
      this#to_json |> Y.Basic.to_string
  end

let json_to_transaction (json : Y.Basic.json) : transaction =
  let open Y.Basic.Util in
  let originator = string_to_pub (json |> member c_ORIGINATOR_KEY |> to_string) in
  let target = string_to_pub (json |> member c_TARGET_KEY |> to_string) in
  let amount = json |> member c_AMOUNT_KEY |> to_float in
  new transaction originator target amount

class block (tlist : transaction list) =
  object
    val transactions = tlist
    method transactions = transactions
    method contains_transaction t = List.memq t transactions
    method to_string = List.fold_left (fun a t -> a ^ t#to_string) "" tlist
    method to_json : Y.Basic.json = `List (List.map (fun t -> t#to_json) tlist)
  end

let json_to_block (json : Y.Basic.json) : block =
  match json with
  | `List jsonlist ->
      new block (List.map (fun tjson -> json_to_transaction tjson) jsonlist)
  | _ -> failwith "Blocks can only serialize json lists"
