open Nocrypto
<<<<<<< HEAD
open Crypto.Signature

let test_sign () =
  let (priv_key, pub_key) = generate_keypair () in
  let message = string_of_int (Random.int 10000000) in
  let signed = sign priv_key message in
  assert (verify message pub_key signed)

let run_tests () =
  TestHelpers.run_tests test_sign
=======
open Crypto

let run_tests () =
  TestHelpers.run_tests MD5.run_tests;
  TestHelpers.run_tests SHA1.run_tests;
  TestHelpers.run_tests SHA224.run_tests;
  TestHelpers.run_tests SHA256.run_tests;
  TestHelpers.run_tests SHA384.run_tests;
  TestHelpers.run_tests SHA512.run_tests;
  TestHelpers.run_tests Keychain.run_tests;
  TestHelpers.run_tests Signature.run_tests
>>>>>>> master
