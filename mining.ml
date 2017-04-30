exception Nosolution of string

type nonce = int
let nonce_to_string = string_of_int
let string_to_nonce = int_of_string

let is_mining = ref false

let currently_mining () = !is_mining

let leading_zeros = 2

(* Takes a string and extracts the SHA256 hash of that string*)
let hash_text = Crypto.SHA256.hash_text

(* Checks whether or not the hash of str with nonce on the back
   has *size* or more zeroes on the front *)
let verify (str : string) (n : nonce) : bool =
  let combo = str ^ (string_of_int n) in
  let hashed = hash_text combo in
  print_string " ";
  let first_chars = String.sub hashed 0 leading_zeros in
  first_chars = String.make leading_zeros '0'

(* Implementation of the mining algorithm for proof-of-work *)
let mine (s : string) (iters: int) : nonce =
(* Beginning nonce *)
  (* This inner function takes an integer and checks
     the has verify of s^nonce for nonce = 1,..., n *)
  let rec iterate_check n =
    if n = 0 then raise (Nosolution "Couldn't solve block")
    else if currently_mining () = false then
      raise (Nosolution "Couldn't solve block")
    else if verify s n then
      let _ = is_mining := false in
      n
    else iterate_check (n - 1) in
  iterate_check iters

let mine_async (s : string) =
  let _ = Thread.create (fun () -> mine s max_int) () in
  ()

let stop_mining () = is_mining := false
