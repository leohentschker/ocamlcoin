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

let generate_transaction () =
  let priv, orig  = generate_keypair () in
  let _, target = generate_keypair () in
  let amount = Random.float 1000000.
  let timestamp = Random.float 1000000.
  let solution = Random.int 999999999999999999999.
  in create_transaction orig target amount timestamp priv solution
  
let test_verify_transaction () =
  new ocamlcoin_node (generate_valid_ip ()) (800 + Random.int 100)

let test_add_transaction () =
  assert(is_valid_ip "10.255.255.255");
  assert(is_valid_ip "10.252.197.92");
  assert(not (is_valid_ip "10a.252.197.92"));
  assert(not (is_valid_ip "10.25s2.197.92"));
  assert(not (is_valid_ip "10.252.197s.92"));
  assert(not (is_valid_ip "10.252.197.s92"))

let test_verify () =
  let ip = generate_valid_ip () in
  let port = Random.int 800 in
  let node = new ocamlcoin_node ip port in
  assert(node#ip = ip);
  assert(node#port = port)

let test_verify_tree () =
  let actual_ip = get_private_ip () in
  let personal_node = new ocamlcoin_node actual_ip c_DEFAULT_COIN_PORT in
  let serialized_node = json_to_ocamlcoin_node (personal_node#to_json) in
  assert(personal_node#port = serialized_node#port);
  assert(personal_node#ip = serialized_node#ip)

let test_query () =
  let actual_ip = get_private_ip () in
  let personal_node = new ocamlcoin_node actual_ip c_DEFAULT_COIN_PORT in
  let serialized_node = json_to_ocamlcoin_node (personal_node#to_json) in
  assert(personal_node#port = serialized_node#port);
  assert(personal_node#ip = serialized_node#ip)

let run_tests () =
  test_is_valid_ip ();
  TestHelpers.run_tests test_node_instantiation
