open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Payments.Transaction
open Merkletree
<<<<<<< HEAD
module IO = IOHelpers
open Mining
open Mining.Miner
module Y = Yojson

let c_MASTERKEY_FILE_NAME = "masterkey.json"
let c_LEDGER_FILE_NAME = "ledger.json"

exception MissingMasterkey
let masterpub =
  try
    let json = Yojson.Basic.from_file c_MASTERKEY_FILE_NAME in
    let open Yojson.Basic.Util in
    string_to_pub (json |> member c_PUB_JSON_KEY |> to_string)
  with Sys_error _ ->
    raise MissingMasterkey

module MT = MakeMerkle (TransactionSerializable) (SHA256)

let get_transactions (l : MT.mtree ref) =
  MT.children !l

let export_ledger (l : MT.mtree ref) : unit =
  match get_transactions l with
  | _h :: _t as tlist ->
    IO.write_json
      (`List(List.map (fun t -> t#to_json) tlist)) c_LEDGER_FILE_NAME
  | [] -> ()

let ledger =
  let previous_transactions = try
    match Y.Basic.from_file c_LEDGER_FILE_NAME with
    | `List json_list -> List.map json_to_transaction json_list
    | _ -> failwith "Unexpected json format"
    with Sys_error _ -> [] in
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
=======
open IOHelpers

module Bank =
  struct

    let priv, masterkey = generate_keypair ()
    module MT = MakeMerkle (TransactionSerializable) (SHA256)

    type mtree = MT.mtree
    type ledger = MT.mtree ref

    let book = ref MT.empty

    let empty = MT.empty

    let query (p : pub_key) (m : ledger) : transaction list =
      (MT.queryid p !m) @ (MT.queryhash (pub_to_string p) !m)

    let verify_transaction (t : transaction) (l: ledger) : bool =
      let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
      let eltlst = MT.queryid id1 !l in
      let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in
      let total_amount = List.fold_left
        (fun acc x -> if x#originator = id1 then acc -. x#amount
                      else acc +. x#amount) 0. timedlst in
      authenticate_transaction t
      && ((not (eltlst = [])
           && total_amount < amount
           && amount > 0.
           && Mining.Miner.verify t#to_string t#solution)
          ||
         id1 = masterkey)

    let add_transaction (t : transaction) (l : ledger) : unit =
      if verify_transaction t l then
        l := (MT.add_element t !l)
      else ()

    let verify_ledger (t : ledger) : bool =
      let rec verify (t : ledger) (n : int) : bool =
        if n <= 0 then true
        else
          let tlist = MT.children !t in
          let slist = sublist tlist 0 (n - 1) in
          let tn = List.nth tlist n in
          let subledger = ref (MT.build_tree slist) in
          verify_transaction tn subledger && (verify subledger (n - 1)) in
      verify t (List.length (MT.children !t) - 1)

    (* let merge_ledgers (tree1 : ledger)
                      (tree2 : ledger) : unit =
      if not ((verify_ledger tree1) && (verify_ledger tree2))
        then raise (Invalid_argument "Stop trying to cheat")
      else if ((MT.root_hash !tree1 = MT.root_hash !tree2)
              || (!tree2 = MT.empty)) then ()
      else List.iter (fun e -> (add_transaction e tree1))
                     (List.filter (fun e -> not (List.memq e (MT.children !tree1)))
                                  (MT.children !tree2));; *)

    let bad_amount_transaction () =
      let _, target = generate_keypair () in
      let amount = ~-.(Random.float 1000.) in
      let timestamp = Random.float 100000. in
      create_transaction masterkey target amount timestamp priv

    let generate_transaction () =
      let _, target = generate_keypair () in
      let amount = Random.float 1000. in
      let timestamp = Random.float 100000. in
      create_transaction masterkey target amount timestamp priv

    let generate_transaction_list () =
      TestHelpers.generate_list generate_transaction (Random.int 30)

    let test_add_transaction () =
      let ledger = ref empty in
      let new_transaction = generate_transaction () in
      let transaction_list = generate_transaction_list () in
      add_transaction new_transaction ledger;
      List.iter (fun t -> add_transaction t ledger) transaction_list;
      List.iter (fun t -> assert (List.mem t (MT.children !ledger))) transaction_list

    let test_query () =
      let ledger = ref empty in
      let transaction_list = generate_transaction_list () in
      let other_transaction = generate_transaction () in
      List.iter (fun t -> add_transaction t ledger) transaction_list;
      List.iter (fun t -> assert (List.memq t (query t#originator ledger)))
                transaction_list;
      assert (not (List.memq other_transaction
                             (query other_transaction#originator ledger)))

    (* More tests here *)
    let test_verify_transaction () =
      let ledger = ref empty in
      let priv1, pub1, priv2, pub2 = generate_keypair (), generate_keypair () in
      let transaction1 = create_transaction masterkey pub1 100. 100. priv in
      let transaction2 = create_transaction masterkey pub2 100. 150. priv in
      let good_transaction = create_transaction pub1 pub2 100. 200. priv1 in
      let bad_transaction1 = create_transaction pub1 pub2 100. 250. priv1 in
      let bad_transaction2 = create_transaction pub2 pub1 300. 300. priv2 in
      let bad_transaction3 = create_transaction pub2 pub1 100. ~-.(250.) priv2 in
      let bad_transaction4 = create_transaction pub2 pub1 ~-(100.) 250. pri21 in
      let valid_list = generate_transaction_list () in
      let invalid_transaction = bad_amount_transaction () in
      List.iter (fun t -> add_transaction t ledger) valid_list;
      add_transaction invalid_transaction ledger;
      List.iter (fun t -> assert (verify_transaction t ledger)) valid_list

    let test_verify_ledger () =
      let good_ledger = ref empty in
      let transaction_list = generate_transaction_list () in
      List.iter (fun t -> add_transaction t good_ledger) transaction_list;
      assert (verify_ledger good_ledger)

    (* let test_merge_ledgers () =
      let ledger1, ledger2 = ref empty, ref empty in
      let transaction_list1 = generate_transaction_list () in
      let transaction_list2 = generate_transaction_list () in
      List.iter (fun t -> add_transaction t ledger1) transaction_list1;
      List.iter (fun t -> add_transaction t ledger2) transaction_list2;
      List.iter (fun t -> assert (List.memq t (query t#originator ledger1)))
                (transaction_list1 @ transaction_list2) *)

    let run_tests () =
      TestHelpers.run_tests test_add_transaction;
      TestHelpers.run_tests test_query;
      TestHelpers.run_tests test_verify_transaction;
      TestHelpers.run_tests test_verify_ledger;
      TestHelpers.run_tests test_query;
      print_endline "All tests passed!"
  end
  
let _ = Bank.run_tests ()
>>>>>>> merkle
