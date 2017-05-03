open Payments.Transaction
open Payments_tests
exception Nosolution of string

module Miner =
  struct
    type nonce = int
    let nonce_to_string = string_of_int
    let string_to_nonce = int_of_string

    let is_mining = ref false

    (* Expose whether or not we are mining to external modules *)
    let stop_mining () = is_mining := false
    let currently_mining () = !is_mining

    (* control how long it takes to mine by increasing leading_zeros *)
    let leading_zeros = 2

    (* Takes a string and extracts the SHA256 hash of that string*)
    let hash_text = Crypto.SHA256.hash_text

    (* Checks whether or not the hash of str with nonce on the back
       has *size* or more zeroes on the front *)
    let verify (str : string) (n : nonce) : bool =
      let combo = str ^ (string_of_int n) in
      let hashed = hash_text combo in
      let first_chars = String.sub hashed 0 leading_zeros in
      first_chars = String.make leading_zeros '0'

    (* Implementation of the mining algorithm for proof-of-work *)
    let mine (t : transaction) (iters: int) : nonce =
      (* This inner function takes an integer and checks
         the has verify of s^nonce for nonce = 1,..., n *)
      is_mining := true;
      let str = t#to_string in
      let rec iterate_check n =
        if n = 0 then raise (Nosolution "Couldn't solve block")
        else if currently_mining () = false then
          raise (Nosolution "Couldn't solve block")
        else if verify str n then
          let _ = is_mining := false in
          n
        else iterate_check (n - 1) in
      iterate_check iters

    let mine_async () =
      let t = Payments.get_unmined_transaction () in
      let _ = Thread.create (fun () -> mine t max_int) () in
      ()

    let generate_fake_nonce () =
      string_to_nonce (TestHelpers.random_string ())

    let test_mining () = 
      let word = "hello" in
      let bad = generate_fake_nonce () in
      assert (not (verify word bad));
      let t = generate_fake_transaction () in
      let nonce = string_of_int (mine t 100000) in
      assert (String.sub (hash_text(t#to_string ^ nonce)) 0 leading_zeros =
        String.make leading_zeros '0') 
  end
