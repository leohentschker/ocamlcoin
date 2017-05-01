open Networking
open Payments
open Mining
module Y = Yojson

let c_TRANSACTION_TYPE_KEY = "transaction_type"
let c_JSON_DATA_KEY = "json_data"
let c_PING_DISCOVERY_TYPE = "ping"
let c_NEW_TRANSACTION_TYPE = "new_transaction"
let c_SOLVED_BLOCK_TYPE = "solved_block"
let c_BROADCAST_NODES_TYPE = "broadcast_nodes"

let c_NONCE_KEY = "nonce"
let c_BLOCK_KEY = "block"

let make_event_json (transaction_key : string) (json : Y.Basic.json) : Y.Basic.json =
  `Assoc [(c_TRANSACTION_TYPE_KEY, `String transaction_key);
          (c_JSON_DATA_KEY, json)]

let event_to_json (e : network_event) : Y.Basic.json =
  match e with
  | PingDiscovery -> make_event_json c_PING_DISCOVERY_TYPE (`Bool true)
  | NewTransaction t -> make_event_json c_NEW_TRANSACTION_TYPE t#to_json
  | SolvedBlock (b, n) ->
      make_event_json c_SOLVED_BLOCK_TYPE 
        (`Assoc [(c_NONCE_KEY, `String (nonce_to_string n)); (c_BLOCK_KEY, b#to_json)])
  | BroadcastNodes nlist ->
      make_event_json c_BROADCAST_NODES_TYPE
        (`List (List.map (fun n -> n#to_json) nlist))

let json_to_event (json : Y.Basic.json) : network_event =
  let open Y.Basic.Util in
  let event_type = json |> member c_TRANSACTION_TYPE_KEY |> to_string in
  let json_data = json |> member c_JSON_DATA_KEY in
  if event_type = c_NEW_TRANSACTION_TYPE then
    NewTransaction(json_to_transaction json_data)
  else if event_type = c_SOLVED_BLOCK_TYPE then
    let nonce = json_data |> member c_NONCE_KEY |> to_string in
    let block = json_to_block (json_data |> member c_BLOCK_KEY) in
    SolvedBlock(block, Mining.string_to_nonce nonce)
  else if event_type = c_BROADCAST_NODES_TYPE then
    match json_data with
    | `List json_list -> BroadcastNodes(List.map OcamlcoinNetwork.json_to_ocamlcoin_node json_list)
    | _ -> failwith "Expected json list type"
  else if event_type = c_PING_DISCOVERY_TYPE then
    PingDiscovery
  else failwith ("Unexpected event type: " ^ event_type)
