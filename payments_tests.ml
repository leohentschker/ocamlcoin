open Payments
open TestHelpers
open Crypto.Signature

let fake_transaction_data () =
  let _, originator = generate_keypair () in
  let _, target = generate_keypair () in
  let amount = Random.float 1000. in
  originator, target, amount

let generate_fake_transaction () =
  let originator, target, amount = fake_transaction_data () in
  new transaction originator target amount

let test_instantiation () =
  let originator, target, amount = fake_transaction_data () in
  let transaction = new transaction originator target amount in
  assert(transaction#originator = originator);
  assert(transaction#target = target);
  assert(transaction#amount = amount)

let test_serialize () =
  let transaction = generate_fake_transaction () in
  let serialized_transaction = string_to_transaction (transaction#to_string) in
  assert (serialized_transaction#amount = transaction#amount);
  assert (serialized_transaction#originator = transaction#originator);
  assert (serialized_transaction#target = transaction#target)

let run_tests () =
  TestHelpers.run_tests test_instantiation;
  TestHelpers.run_tests test_serialize

let _ = run_tests () ;;
