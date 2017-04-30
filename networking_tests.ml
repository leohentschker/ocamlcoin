open TestHelpers ;;
open Networking ;;
open Networking.OcamlcoinNetwork ;;

let test_is_valid_ip () =
  assert(is_valid_ip "10.255.255.255");
  assert(is_valid_ip "10.252.197.92");
  assert(not (is_valid_ip "10a.252.197.92"));
  assert(not (is_valid_ip "10.25s2.197.92"));
  assert(not (is_valid_ip "10.252.197s.92"));
  assert(not (is_valid_ip "10.252.197.s92"))

let test_node_instantiation () =
  let actual_ip = get_private_ip () in
  let personal_node = new ocamlcoin_node actual_ip c_DEFAULT_COIN_PORT in
  assert(personal_node#ip = actual_ip);
  assert(personal_node#port = c_DEFAULT_COIN_PORT)

let test_node_serialization () =
  let actual_ip = get_private_ip () in
  let personal_node = new ocamlcoin_node actual_ip c_DEFAULT_COIN_PORT in
  let serialized_node = json_to_ocamlcoin_node (personal_node#to_json) in
  assert(personal_node#port = serialized_node#port);
  assert(personal_node#ip = serialized_node#ip)

let run_tests () =
  test_is_valid_ip ();
  test_node_instantiation ();
  ()
let _ = run_tests ()
