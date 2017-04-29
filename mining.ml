open Nocrypto

exception Nosolution of string

type nonce = int
let leading_zeros = 2

(* Takes a string and extracts the SHA256 hash of that string*)
let hash_text = Crypto.SHA256.hash_text

(* Checks whether or not the hash of str with nonce on the back
   has *size* or more zeroes on the front *)
let verify (str : string) (n : nonce) : bool =
  let combo = str ^ (string_of_int n) in
  let hashed = hash_text combo in
  print_endline hashed;
  let first_chars = String.sub hashed 0 leading_zeros in
  first_chars = String.make leading_zeros '0'

(* Implementation of the mining algorithm for proof-of-work *)
let mine (s : string) (iters: int) : nonce =
(* Beginning nonce *)
  (* This inner function takes an integer and checks
     the has verify of s^nonce for nonce = 1,..., n *)
  let rec iterate_check n =
    if n = 0 then raise (Nosolution "couldn't solve block in required iterations")
    else if verify s n then
      n
    else iterate_check (n - 1) in
  iterate_check iters

let mine_async (s : string) = Thread.create (fun () -> mine s max_int) ()
