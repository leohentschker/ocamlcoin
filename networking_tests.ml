open Crypto.Keychain ;;
open TestHelpers ;;
open Networking ;;
open Networking.OcamlcoinNetwork ;;

let generate_valid_ip () =
  (string_of_int 255) ^ "." ^ (string_of_int 255) ^ "." ^ (string_of_int 255) ^ "." ^ (string_of_int 255)

let generate_random_node () =
  new ocamlcoin_node (generate_valid_ip ()) (800 + Random.int 100)
                     (let _, pub = generate_keypair () in pub)

let test_is_valid_ip () =
  assert(is_valid_ip "10.255.255.255");
  assert(is_valid_ip "10.252.197.92");
  assert(not (is_valid_ip "10a.252.197.92"));
  assert(not (is_valid_ip "10.25s2.197.92"));
  assert(not (is_valid_ip "10.252.197s.92"));
  assert(not (is_valid_ip "10.252.197.s92"))

let test_node_instantiation () =
  let ip = generate_valid_ip () in
  let port = Random.int 800 in
  let _, pub = generate_keypair () in
  let node = new ocamlcoin_node ip port pub in
  assert(node#ip = ip);
  assert(node#port = port)

let test_node_serialization () =
  let actual_ip = get_private_ip () in
  let _, pub = generate_keypair () in
  let personal_node = new ocamlcoin_node actual_ip c_DEFAULT_COIN_PORT pub in
  let serialized_node = json_to_ocamlcoin_node (personal_node#to_json) in
  assert(personal_node#port = serialized_node#port);
  assert(personal_node#ip = serialized_node#ip);
  assert(personal_node#pub = serialized_node#pub)

let run_tests () =
  test_is_valid_ip ();
  TestHelpers.run_tests test_node_instantiation
