open Mining
open Payments
module Y = Yojson

let c_TRANSACTION_TYPE_KEY = "transaction_type"
let c_JSON_DATA_KEY = "json_data"
let c_NEW_TRANSACTION_TYPE = "new_transaction"
let c_SOLVED_BLOCK_TYPE = "solved_block"
let c_NONCE_KEY = "nonce"
let c_BLOCK_KEY = "block"

type network_event =
  | NewTransaction of transaction
  | SolvedBlock of (transaction * nonce)

let json_to_event_string (transaction_key : string) (json : Y.Basic.json) : string =
  Y.Basic.to_string (`Assoc [(c_TRANSACTION_TYPE_KEY, `String transaction_key);
                             (c_JSON_DATA_KEY, json)])

let event_to_string (e : network_event) =
  match e with
  | NewTransaction t -> json_to_event_string c_NEW_TRANSACTION_TYPE t#to_json
  | SolvedBlock (b, n) ->
      json_to_event_string
        c_SOLVED_BLOCK_TYPE 
        (`Assoc [(c_NONCE_KEY, `String ""); (c_BLOCK_KEY, b#to_json)])

let string_to_event (s : string) =
  let open Yojson.Basic.Util in
  let json = Y.Basic.from_string s in
  let event_type = json |> member c_TRANSACTION_TYPE_KEY |> to_string in
  let json_data = json |> member c_JSON_DATA_KEY in
  if event_type = c_NEW_TRANSACTION_TYPE then
    let _ = json_to_transaction json_data in
    ()
  else if event_type = c_SOLVED_BLOCK_TYPE then
    let _ = json_to_block json_data in
    ()
  else failwith "Unexpected event type"
