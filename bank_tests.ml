open Sexplib
open Crypto
open Crypto.Keychain
open Payments.Transaction
open Merkletree
open IOHelpers
open Bank
open TestHelpers ;;
open Networking ;;
open Networking.OcamlcoinNetwork ;;

let fake_transaction_data () =
  let priv, originator = generate_keypair () in
  let _, target = generate_keypair () in
  let amount = Random.float 1000. in
  let timestamp = Random.float 100000. in
  originator, target, amount, timestamp, priv

let generate_fake_transaction () =
  let originator, target, amount, timestamp, priv = fake_transaction_data () in
  create_transaction originator target amount timestamp priv

let generate_valid_transaction () =
  let priv, orig  = generate_keypair () in
  let _, target = generate_keypair () in
  let amount = Random.float 1000000.
  let timestamp = Random.float 1000000.
  let solution = Random.int 999999999999999999999.
  in create_transaction orig target amount timestamp priv solution

let test_verify_transaction () =
  let real, fake =
    generate_valid_transaction (), generate_fake_transaction () in
  assert (verify_transaction real ledger);
  assert (not verify_transaction fake ledger)


let test_add_transaction () =

let test_verify_tree () =

let test_query () =

let run_tests () =
  test_is_valid_ip ();
  TestHelpers.run_tests test_node_instantiation
