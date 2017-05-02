let test_key_serialize () =
  let priv, pub = generate_keypair () in
  let serialized_priv = string_to_priv (priv_to_string priv) in
  let serialized_pub = string_to_pub (pub_to_string pub) in
  assert(priv = serialized_priv);
  assert(pub = serialized_pub)

(* Testing in module because verify and sign are abstracted away *)
let test_generate_keypair () =
  let (priv_key, pub_key) = generate_keypair () in
  assert (pub_key = Dsa.pub_of_priv priv_key)

let test_signature_serialize () =
  let (priv, pub) = generate_keypair () in
  let signed = sign priv (TestHelpers.random_string ()) in
  assert(signed = json_to_signature (signature_to_json signed))

let test_sign () =
  let (priv_key, pub_key) = generate_keypair () in
  let message = string_of_int (TestHelpers.random_string ()) in
  let signed = sign priv_key message in
  assert (verify message pub_key signed)

let run_tests () =
  TestHelpers.run_tests test_key_serialize;
  TestHelpers.run_tests test_generate_keypair;
  TestHelpers.run_tests test_sign
