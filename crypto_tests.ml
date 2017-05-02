(* Testing in module because verify and sign are abstracted away *)
let test_generate_keypair () =
  let (priv_key, pub_key) = generate_keypair () in
  assert (pub_key = Dsa.pub_of_priv priv_key)

let test_sign () =
  let (priv_key, pub_key) = generate_keypair () in
  let message = string_of_int (Random.int 10000000) in
  let signed = sign priv_key message in
  assert (verify message pub_key signed)

let run_tests () =
  TestHelpers.run_tests test_generate_keypair;
  TestHelpers.run_tests test_sign
