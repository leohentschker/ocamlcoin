open TestHelpers
open Networking_tests
open Payments_tests
open Mining_tests
open Events

let test_ping_discovery () =
  let s = event_to_string PingDiscovery in
  let e = string_to_event s in
  match string_to_event (event_to_string PingDiscovery) with
  | PingDiscovery -> ()
  | _ -> failwith "Expected ping discovery"

let test_new_transaction_serialization () =
  let t = generate_fake_transaction () in
  let serialized_event = string_to_event (event_to_string (NewTransaction t)) in
  match serialized_event with
  | NewTransaction serialized_t -> assert(transactions_equal t serialized_t)
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

let test_broadcast_nodes_serialization () =
  let n1, n2, n3 = generate_random_node (), generate_random_node (), generate_random_node () in
  let broadcast = BroadcastNodes([n1; n2; n3]) in
  match string_to_event (event_to_string broadcast) with
  | BroadcastNodes(nlist) ->
      List.iter (fun n -> assert(List.memq n nlist)) nlist
  | _ -> failwith "Broadcast nodes serialization the incorrect event type"

let run_tests () =
  test_ping_discovery();
  TestHelpers.run_tests test_solved_block_serialization;
  TestHelpers.run_tests test_new_transaction_serialization;
  TestHelpers.run_tests test_broadcast_nodes_serialization

let _ = run_tests ()
