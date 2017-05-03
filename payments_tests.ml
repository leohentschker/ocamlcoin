open Payments
open Transaction
open Block
open TestHelpers
open Crypto.Keychain

let fake_transaction_data () =
  let priv, originator = generate_keypair () in
  let _, target = generate_keypair () in
  let amount = Random.float 1000. in
  let timestamp = Random.float 100000. in
  originator, target, amount, timestamp, priv

let generate_fake_transaction () =
  let originator, target, amount, timestamp, priv = fake_transaction_data () in
  create_transaction originator target amount timestamp priv

let transactions_equal t1 t2 =
  (t1#amount = t2#amount) && (t1#originator = t2#originator)
  && (t1#target = t2#target) && (t1#timestamp = t2#timestamp)

let test_transaction_instantiation () =
  let originator, target, amount, timestamp, priv = fake_transaction_data () in
  let transaction =
    create_transaction originator target amount timestamp priv in
  assert(transaction#originator = originator);
  assert(transaction#target = target);
  assert(transaction#amount = amount);
  assert(transaction#timestamp = timestamp)

let test_transaction_serialize () =
  let transaction = generate_fake_transaction () in
  let serialized_transaction = json_to_transaction (transaction#to_json) in
  assert(transactions_equal transaction serialized_transaction)

let generate_fake_block () =
  new block [generate_fake_transaction (); generate_fake_transaction ();
             generate_fake_transaction ()]

let test_blocks_equal b1 b2 =
  List.iter
    (fun t1 ->
      assert(List.fold_left (fun a t2 -> a || (transactions_equal t1 t2))
                            false
                            b1#transactions))
    b2#transactions

let test_block_instantiation () =
  let t1, t2, t3 =
    generate_fake_transaction (), generate_fake_transaction (),
    generate_fake_transaction () in
  let b = new block [t1; t2; t3] in
  List.iter (fun t -> assert(b#contains_transaction t)) [t1; t2; t3]

let test_block_serialize () =
  let b = generate_fake_block () in
  test_blocks_equal b (json_to_block b#to_json)

let run_tests () =
  TestHelpers.run_tests test_transaction_instantiation;
  TestHelpers.run_tests test_transaction_serialize;
  TestHelpers.run_tests test_block_instantiation;
  TestHelpers.run_tests test_block_serialize
