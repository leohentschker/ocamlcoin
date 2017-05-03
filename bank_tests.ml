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
    transaction_mlist = Payments_tests.generate_fake_transaction ()
  done;
  !transaction_mlist

let test_verify_transaction () =
  let valid = Payments_tests.generate_fake_transaction () in
  let invalid = bad_amount_transaction () in
  assert (verify_transaction valid MT.empty);
  assert (not verify_transaction invalid MT.empty)

let test_verify_ledger () =
  let good_ledger, bad_ledger = MT.empty, MT.empty in
  let valid = Payments_tests.generate_fake_transaction () in
  let invalid = bad_amount_transaction () in
  add_transaction valid good_ledger;
  add_transaction valid bad_ledger;
  add_transaction invalid bad_ledger;
  assert (verify_tree good_ledger);
  assert (not verify_transaction bad_ledger)

let test_query () =
  let ledger = MT.empty in
  let transaction1 = Payments_tests.generate_fake_transaction () in
  let transaction2 = Payments_tests.generate_fake_transaction () in
  let transaction3 = Payments_tests.generate_fake_transaction () in
  let transaction4 = Payments_tests.generate_fake_transaction () in
  add_transaction transaction1 ledger;
  add_transaction transaction2 ledger;
  add_transaction transaction3 ledger;
  assert (query transaction1#originator ledger);
  assert (query transaction2#originator ledger);
  assert (query transaction3#originator ledger);
  assert (not (query transaction4#originator ledger))

let test_merge_ledgers () =
  let ledger1 = MT.empty in
  let ledger2 = MT.empty in
  let transaction1 = Payments_tests.generate_fake_transaction () in
  let transaction2 = Payments_tests.generate_fake_transaction () in
  let transaction3 = Payments_tests.generate_fake_transaction () in
  let transaction4 = Payments_tests.generate_fake_transaction () in
  add_transaction transaction1 ledger1;
  add_transaction transaction2 ledger1;
  add_transaction transaction3 ledger2;
  add_transaction transaction4 ledger2;
  assert (query transaction1#originator ledger1);
  assert (query transaction2#originator ledger1);
  assert (query transaction3#originator ledger1);
  assert (query transaction4#originator ledger1)

let run_tests () =
  TestHelpers.run_tests test_verify_transaction ()
  TestHelpers.run_tests test_verify_ledger ()
  TestHelpers.run_tests test_query ()
  TestHelpers.run_tests test_merge_ledre ()
