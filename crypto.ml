open Sexplib
open Nocrypto

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
  end

open Keychain

module Signature =
  struct
    type signature = Cstruct.t * Cstruct.t
    type priv_key = Dsa.priv
    type pub_key = Dsa.pub
    let priv_to_string (p : priv_key) = Sexp.to_string (Dsa.sexp_of_priv p)
    let string_to_priv (s : string) = Dsa.priv_of_sexp (Sexp.of_string s)
    let pub_to_string (p : pub_key) = Sexp.to_string (Dsa.sexp_of_pub p)
    let string_to_pub (s : string) = Dsa.pub_of_sexp (Sexp.of_string s)
    let generate_keypair () =
      let private_key = Dsa.generate (`Fips1024) in
      private_key, Dsa.pub_of_priv private_key
    let sign (pk : priv_key) = Dsa.sign ~key:pk
    let verify (plaintext : string) (pub : pub_key) (s : signature) =
      Dsa.verify ~key:pub s (Cstruct.of_string plaintext)

    (* Testing in module because verify and sign are abstracted away *)
    let test_generate_keypair () =
      let (priv_key, pub_key) = generate_keypair () in
      assert (pub_key = pub_priv priv_key)

    let test_sign () =
      let (priv_key, pub_key) = generate_keypair () in
      let message = string_of_int (Random.int 10000000) in
      let signed = sign priv_key message in
      assert (verify message pub_key signed)

    let run_tests () =
      TestHelpers.run_tests test_generate_keypair;
      TestHelpers.run_tests test_sign
  end

module type HASH = sig val hash_text : string -> string end

module MakeHash(H : Hash.S) : HASH =
  struct
    let hash_text s =
      let init = H.init () in
      H.feed init (Cstruct.of_string s);
      let digest = H.digest (H.get init) in
      Cstruct.to_string digest ;;
  end

module MD5 = MakeHash(Hash.MD5)
module SHA1 = MakeHash(Hash.SHA1)
module SHA224 = MakeHash(Hash.SHA224)
module SHA256 = MakeHash(Hash.SHA256)
module SHA384 = MakeHash(Hash.SHA384)
module SHA512 = MakeHash(Hash.SHA512)
