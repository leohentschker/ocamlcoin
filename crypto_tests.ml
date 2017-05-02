open Nocrypto
open Crypto.Signature

let test_sign () =
  let (priv_key, pub_key) = generate_keypair () in
  let message = string_of_int (Random.int 10000000) in
  let signed = sign priv_key message in
  assert (verify message pub_key signed)

let run_tests () =
  TestHelpers.run_tests test_sign
