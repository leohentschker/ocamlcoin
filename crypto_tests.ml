open Nocrypto
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
