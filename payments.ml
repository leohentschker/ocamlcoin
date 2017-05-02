module IO = IOHelpers
open Crypto.Keychain
module Y = Yojson

let c_ORIGINATOR_KEY = "originator"
let c_TARGET_KEY = "target"
let c_AMOUNT_KEY = "amount"
let c_TIMESTAMP_KEY = "timestamp"
let c_SIGNATURE = "signature"
let c_BLOCK_SIZE = 10

class transaction
    (originator : pub_key)
    (target : pub_key)
    (amount : float)
    (timestamp : float)
    (priv : priv_key) =
  object(this)
    val originator = originator
    val target = target
    val amount = amount
    val timestamp = timestamp
    method originator = originator
    method target = target
    method amount = amount
    method timestamp = timestamp
    method to_string =
      this#to_json |> Y.Basic.to_string
    method private signature =
      Crypto.Signature.sign priv this#to_string
    method authenticated = Crypto.Signature.verify this#to_string originator this#signature
    (* following https://realworldocaml.org/v1/en/html/handling-json-data.html *)
    method to_json =
      `Assoc[(c_ORIGINATOR_KEY, `String (pub_to_string originator));
             (c_TARGET_KEY, `String (pub_to_string target));
             (c_AMOUNT_KEY, `Float amount);
             (c_SIGNATURE, Crypto.Signature.signature_to_json this#signature)]
  end

let json_to_transaction (json : Y.Basic.json) : transaction =
  let open Y.Basic.Util in
  let originator = string_to_pub (json |> member c_ORIGINATOR_KEY |> to_string) in
  let target = string_to_pub (json |> member c_TARGET_KEY |> to_string) in
  let amount = json |> member c_AMOUNT_KEY |> to_float in
  let timestamp = json |> member c_TIMESTAMP_KEY |> to_float in
  new transaction originator target amount timestamp Profile.User.private_key

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

(* Store a global list of unverified transactions *)
let unmined_transactions = ref []
let add_unmined_transaction (t : transaction) =
  unmined_transactions := t :: !unmined_transactions

exception NoUnverified
let get_unmined_block () =
  match IO.sublist !unmined_transactions 0 c_BLOCK_SIZE with
  | [] -> raise NoUnverified
  | lst -> new block lst
