open Sexplib
open Crypto
open Crypto.Keychain
open Payments.Transaction
open Merkletree
open IOHelpers
open Bank

let bad_amount_transaction () =
  let _, originator generate_keypair () in
  let bad, _ = generate_keypair () in
  let _, target = generate_keypair () in
  let amount = ~-.(Random.float 1000.) in
  let timestamp = Random.float 100000. in
  create_transaction originator target amount timestamp bad

let generate_transaction_list () =
  let transaction_mlist = ref [] in
  let i = Random.int 30 in
  for _ = 0 to i do
    let new_transaction = Payments_tests.generate_fake_transaction () in
    transaction_mlist := (!transaction) @ [new_transaction]
  done;
  !transaction_mlist

let test_verify_transaction () =
  let valid = Payments_tests.generate_fake_transaction () in
  let invalid = bad_amount_transaction () in
  assert (verify_transaction valid MT.empty);
  assert (not verify_transaction invalid MT.empty)

let test_verify_ledger () =
  let good_ledger, bad_ledger1, bad_ledger2 = MT.empty, MT.empty, MT.empty in
  let transaction_list = generate_transaction_list () in
  let invalid = bad_amount_transaction () in
  List.iter (fun t -> add_transaction t good_ledger) transaction_list;
  List.iter (fun t -> add_transaction t bad_ledger1) transaction_list;
  add_transaction invalid bad_ledger1;
  add_transaction invalid bad_ledger2;
  assert (verify_tree good_ledger);
  assert (not (verify_transaction bad_ledger1));
  assert (not (verify_transaction bad_ledger2))

let test_query () =
  let ledger = MT.empty in
  let transaction_list = generate_transaction_list () in
  let other_transaction = Payments_tests.generate_fake_transaction () in
  List.iter (fun t -> add_transaction t ledger) transaction_list;
  add_transaction other_transaction ledger;
  List.iter (fun t -> assert (query t#originator ledger));
  assert (not (query other_transaction#originator ledger))

let test_merge_ledgers () =
  let ledger1 = MT.empty in
  let ledger2 = MT.empty in
  let transaction_list1 = generate_transaction_list () in
  let transaction_list2 = generate_transaction_list () in
  List.iter (fun t -> add_transaction t ledger1) transaction_list1;
  List.iter (fun t -> add_transaction t ledger2) transaction_list2;
  List.iter (fun t -> assert (query t#originator ledger))
            (transaction_list1 @ transaction_list2)

let run_tests () =
  TestHelpers.run_tests test_verify_transaction;
  TestHelpers.run_tests test_verify_ledger;
  TestHelpers.run_tests test_query;
  TestHelpers.run_tests test_merge_ledgers
