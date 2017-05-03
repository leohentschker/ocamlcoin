module IO = IOHelpers
open Crypto.Keychain
open Crypto.Signature
module Y = Yojson

let c_ORIGINATOR_KEY = "originator"
let c_TARGET_KEY = "target"
let c_AMOUNT_KEY = "amount"
let c_TIMESTAMP_KEY = "timestamp"
let c_SIGNATURE_KEY = "signature"
let c_SOLUTION_KEY = "solution"
let c_BLOCK_SIZE = 10
let c_UNVERIFIED_TRANSACTIONS_FILE = "files/unverified.json"

module Transaction =
  struct
    let string_of_transaction_data (originator : pub_key)
                                   (target : pub_key)
                                   (amount : float)
                                   (timestamp : float) : string =
      (pub_to_string originator) ^ (pub_to_string target) ^
      (string_of_float amount) ^ (string_of_float timestamp)

    class transaction
        (originator : pub_key)
        (target : pub_key)
        (amount : float)
        (timestamp : float)
        (auth_sig : signature)
        (solution : int) =
      object(this)
        val originator = originator
        val target = target
        val amount = amount
        val timestamp = timestamp
        val signature = auth_sig
        val solution = solution
        method originator = originator
        method target = target
        method amount = amount
        method timestamp = timestamp
        method signature = signature
        method solution = solution
        method equal (t2 : transaction) =
          (this#amount = t2#amount) &&
          (this#originator = t2#originator) &&
          (this#target = t2#target) &&
          (this#timestamp = t2#timestamp)

        method to_string =
          string_of_transaction_data originator target amount timestamp
        method authenticated =
          Crypto.Signature.verify this#to_string originator signature
        (* https://realworldocaml.org/v1/en/html/handling-json-data.html *)
        method to_json : Y.Basic.json =
          `Assoc[(c_ORIGINATOR_KEY, `String (pub_to_string originator));
                 (c_TARGET_KEY, `String (pub_to_string target));
                 (c_AMOUNT_KEY, `Float amount);
                 (c_TIMESTAMP_KEY, `Float timestamp);
                 (c_SIGNATURE_KEY, signature_to_json this#signature);
                 (c_SOLUTION_KEY, `Int solution)]
      end
      (* DO WE WANNA MAKE THIS CONSISTENT???? *)

    let string_of_transaction (t : transaction) : string =
      t#to_string

    let create_transaction (orig : pub_key) (target : pub_key) (amount : float)
                           (timestamp : float) (priv : priv_key) : transaction =
      let signature = Crypto.Signature.sign priv
        (string_of_transaction_data orig target amount timestamp) in
      new transaction orig target amount timestamp signature 0

    let authenticate_transaction (t : transaction) : bool =
        Crypto.Signature.verify (string_of_transaction_data
          t#originator t#target t#amount t#timestamp) t#originator t#signature

    let json_to_transaction (json : Y.Basic.json) : transaction =
      let open Y.Basic.Util in
      let originator = string_to_pub
        (json |> member c_ORIGINATOR_KEY |> to_string) in
      let target = string_to_pub (json |> member c_TARGET_KEY |> to_string) in
      let amount = json |> member c_AMOUNT_KEY |> to_float in
      let timestamp = json |> member c_TIMESTAMP_KEY |> to_float in
      let auth_sig = json |> member c_SIGNATURE_KEY |> json_to_signature in
      let solution = json |> member c_SOLUTION_KEY |> to_int in
      new transaction originator target amount timestamp auth_sig solution
  end

open Transaction

module Block =
  struct
    class block (tlist : transaction list) =
      object
        val transactions = tlist
        method transactions = transactions
        method contains_transaction t = List.memq t transactions
        method to_string = List.fold_left (fun a t -> a ^ t#to_string) "" tlist
        method to_json : Y.Basic.json =
          `List (List.map (fun t -> t#to_json) tlist)
      end

    let json_to_block (json : Y.Basic.json) : block =
      match json with
      | `List jsonlist ->
          new block (List.map (fun tjson -> json_to_transaction tjson) jsonlist)
      | _ -> failwith "Blocks can only serialize json lists"
  end

open Block

(* Store a global list of unverified transactions *)
let unmined_transactions =
  try
    match Yojson.Basic.from_file c_UNVERIFIED_TRANSACTIONS_FILE with
    | `List json_list -> ref (List.map json_to_transaction json_list)
    | _ -> failwith "Unexpected transaction json format"
  with Yojson.Json_error _ | Sys_error _ ->
    ref []

let add_unmined_transaction (t : transaction) =
  unmined_transactions := t :: !unmined_transactions

exception NoUnverified
let get_unmined_transaction () =
  match !unmined_transactions with
  | [] -> raise NoUnverified
  | h :: _t -> h

let export_unverified () =
  IO.write_json (`List (List.map (fun t -> t#to_json) !unmined_transactions))
                c_UNVERIFIED_TRANSACTIONS_FILE

let remove_mined_transaction (t : transaction) : unit =
  unmined_transactions := List.filter (fun t2 -> not (t#equal t2)) !unmined_transactions;
  export_unverified ()
