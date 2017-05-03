open Sexplib
open Nocrypto
open TestHelpers

let () = Nocrypto_entropy_unix.initialize ()

module Keychain =
  struct
    type priv_key = Dsa.priv
    type pub_key = Dsa.pub
    let priv_to_string (p : priv_key) = Sexp.to_string (Dsa.sexp_of_priv (p))
    let string_to_priv (s : string) = Dsa.priv_of_sexp (Sexp.of_string s)
    let pub_to_string (p : pub_key) = Sexp.to_string (Dsa.sexp_of_pub (p))
    let string_to_pub (s : string) = Dsa.pub_of_sexp (Sexp.of_string s)
    let generate_keypair () =
      let private_key = Dsa.generate (`Fips1024) in
      private_key, Dsa.pub_of_priv private_key
    (* TESTING *)
    let test_key_serialize () =
      let priv, pub = generate_keypair () in
      let serialized_priv = string_to_priv (priv_to_string priv) in
      let serialized_pub = string_to_pub (pub_to_string pub) in
      assert(priv = serialized_priv);
      assert(pub = serialized_pub)
    let test_generate_keypair () =
      let (priv_key, pub_key) = generate_keypair () in
      assert (pub_key = Dsa.pub_of_priv priv_key)
    let run_tests () =
      test_key_serialize ();
      test_generate_keypair ()
  end

open Keychain

module Signature =
  struct
    type signature = Cstruct.t * Cstruct.t
    let signature_to_json ((c1, c2) : signature) =
      `List[`String (Cstruct.to_string c1); `String (Cstruct.to_string c2)]
    let json_to_signature (json : Yojson.Basic.json) : signature =
      match json with
      | `List [`String s1; `String s2] ->
        (Cstruct.of_string s1, Cstruct.of_string s2)
      | _ -> failwith "Unexpected format"
    let sign (pk : priv_key) (s : string) =
      Dsa.sign ~key:pk (Cstruct.of_string s)
    let verify (plaintext : string) (pub : pub_key) (s : signature) =
      Dsa.verify ~key:pub s (Cstruct.of_string plaintext)
    (* TESTING *)
    let test_signature_serialize () =
      let (priv, pub) = generate_keypair () in
      let signed = sign priv (TestHelpers.random_string ()) in
      assert(signed = json_to_signature (signature_to_json signed))
    let test_sign () =
      let (priv_key, pub_key) = generate_keypair () in
      let message = TestHelpers.random_string () in
      let signed = sign priv_key message in
      assert (verify message pub_key signed)
    let run_tests () =
      test_signature_serialize ();
      test_sign ()
end

module type HASH =
  sig
    val hash_text : string -> string
    val run_tests : unit -> unit
  end

module MakeHash(H : Hash.S) : HASH =
  struct
    let hash_text s =
      let init = H.init () in
      H.feed init (Cstruct.of_string s);
      let digest = H.digest (H.get init) in
      Cstruct.to_string digest
    let test_hash_equivalence () =
      let rand_str = random_string () in
      assert(hash_text rand_str = rand_str)
    let run_tests () = test_hash_equivalence ()
  end

module MD5 = MakeHash(Hash.MD5)
module SHA1 = MakeHash(Hash.SHA1)
module SHA224 = MakeHash(Hash.SHA224)
module SHA256 = MakeHash(Hash.SHA256)
module SHA384 = MakeHash(Hash.SHA384)
module SHA512 = MakeHash(Hash.SHA512)
