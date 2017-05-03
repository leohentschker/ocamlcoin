open Sexplib
open Crypto
open Crypto.Keychain
open Payments.Transaction
open Merkletree
module IO = IOHelpers
open Mining
open Mining.Miner
module Y = Yojson

let c_MASTERKEY_FILE_NAME = "masterkeys.json"
let c_LEDGER_FILE_NAME = "ledger.json"

let (masterpriv, masterpub) =
  try
    let json = Yojson.Basic.from_file c_MASTERKEY_FILE_NAME in
    let open Yojson.Basic.Util in
    string_to_priv (json |> member c_PRIV_JSON_KEY |> to_string),
    string_to_pub (json |> member c_PUB_JSON_KEY |> to_string)
  with Sys_error _ ->
    let priv, pub = generate_keypair () in
    let json = `Assoc[(c_PUB_JSON_KEY, `String(pub_to_string pub));
                      (c_PRIV_JSON_KEY, `String(priv_to_string priv))] in
    IO.write_json json c_MASTERKEY_FILE_NAME;
    priv, pub

module MT = MakeMerkle (TransactionSerializable) (SHA256)

let export_ledger (l : MT.mtree ref) : unit =
  IO.write_json (`List(List.map (fun t -> t#to_json) (MT.children !l)))
    c_LEDGER_FILE_NAME

let ledger =
  let previous_transactions = try
    match Y.Basic.from_file c_LEDGER_FILE_NAME with
    | `List json_list -> List.map json_to_transaction json_list
    | _ -> failwith "Unexpected json format"
    with Sys_error _ ->
      export_ledger (ref MT.empty);
      [] in
  ref (MT.build_tree previous_transactions)

let verify_transaction (t : transaction) (l: MT.mtree ref) : bool =
  let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
  let eltlst = MT.queryid id1 !l in
  let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in
  let total_amount = List.fold_left
    (fun acc x -> if x#originator = id1 then acc -. x#amount
                  else acc +. x#amount) 0. timedlst in
  not (eltlst = [] || id1 = masterpub) && (total_amount < amount) &&
       amount > 0. && authenticate_transaction t &&
       Mining.Miner.verify t#to_string t#solution

let add_transaction (t : transaction) (l : MT.mtree ref) : unit =
  if verify_transaction t l then
    ledger := (MT.add_element t !ledger)

let verify_tree (t : MT.mtree) = 
  let rec verify (t : MT.mtree) (n : int) : bool =
    if n = 0 then true
    else
      let tlist = MT.children t in
      let slist = IO.sublist tlist 0 (n - 1) in
      let tn = List.nth tlist n in
      verify_transaction tn (ref (MT.build_tree slist)) && (verify t (n - 1)) in
  verify t (List.length (MT.children t) - 1)

let merge_ledgers (tree1 : MT.mtree ref)
                  (tree2 : MT.mtree ref) : unit =
  if not ((verify_tree !tree1) || (verify_tree !tree2))
    then raise (Invalid_argument "Stop trying to cheat")
  else if ((MT.root_hash !tree1 = MT.root_hash !tree2)
          || (!tree2 = MT.empty)) then ()
  else List.iter (fun e -> (add_transaction e tree1))
                 (List.filter (fun e -> not (List.memq e (MT.children !tree1)))
                              (MT.children !tree2));;

let query (s : string) (m : MT.mtree ref) : transaction list =
  (MT.queryid (string_to_pub s) !m) @ (MT.queryhash s !m)
