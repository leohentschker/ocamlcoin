open TestHelpers
open Payments_tests
open Mining_tests
open Events

let test_new_transaction_serialization () =
  let t = generate_fake_transaction () in
  let serialized_event = string_to_event (event_to_string (NewTransaction t)) in
  match serialized_event with
  | NewTransaction serialized_t -> test_transactions_equal t serialized_t
  | _ -> failwith "Returning the incorrect event type"

let test_solved_block_serialization () =
  let b = generate_fake_block () in
  let fake_nonce = generate_fake_nonce () in
  let serialized_event = string_to_event (event_to_string (SolvedBlock (b, fake_nonce))) in
  match serialized_event with
  | SolvedBlock (b_serialized, serialized_nonce) ->
      test_blocks_equal b b_serialized;
      assert(fake_nonce = serialized_nonce)
  | _ -> failwith "Returning the incorrect event type"

let run_tests () =
  TestHelpers.run_tests test_solved_block_serialization;
  TestHelpers.run_tests test_new_transaction_serialization

let _ = run_tests ()
