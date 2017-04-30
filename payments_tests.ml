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

let test_transactions_equal t1 t2 =
  assert (t1#amount = t2#amount);
  assert (t1#originator = t2#originator);
  assert (t1#target = t2#target)

let test_transaction_instantiation () =
  let originator, target, amount = fake_transaction_data () in
  let transaction = new transaction originator target amount in
  assert(transaction#originator = originator);
  assert(transaction#target = target);
  assert(transaction#amount = amount)

let test_transaction_serialize () =
  let transaction = generate_fake_transaction () in
  let serialized_transaction = json_to_transaction (transaction#to_json) in
  test_transactions_equal transaction serialized_transaction

let generate_fake_block () =
  (* new block [generate_fake_transaction (); generate_fake_transaction (); generate_fake_transaction ()] *)
  new block [generate_fake_transaction ()]

let test_blocks_equal b1 b2 =
  Printf.printf "LEN B1 %d Len B2: %d" (List.length b1#transactions) (List.length b2#transactions);
  List.iter
    (fun t1 -> List.fold_left (fun a t2 -> test_transactions_equal t1 t2) false b1#transactions)
    b2#transactions

let test_block_instantiation () =
  let t1, t2, t3 = generate_fake_transaction (), generate_fake_transaction (), generate_fake_transaction () in
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
