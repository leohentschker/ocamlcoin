open Nocrypto

module Mining = 
  struct
  (* Takes a string and extracts the SHA256 hash of that string*)
  let hash_text = Crypto.SHA256.hash_text
  (* Checks whether or not the hash of str with nonce on the back
     has *size* or more zeroes on the front *)
  let verify (size : int) (str : string) (nonce : int) : bool =
    let combo = str ^ (string_of_int nonce) in
    let hashed = hash_text combo in
    let first_chars = String.sub hashed 0 size in
    if first_chars = String.make size '0'
    then true 
    else false
  (* Implementation of the mining algorithm for proof-of-work *)
  let mine (s : string) (iters: int) : string =
  (* Beginning nonce *)
    let nonce = ref 0 in
    let leading_zero_size = 3 in
    (* This inner function takes an integer and checks
       the has verify of s^nonce for nonce = 1,..., n *)
    let rec iterate_check n =
      if n = 0 then failwith "COULDN'T FIND" else
      nonce := !nonce + 1;
      if verify leading_zero_size s !nonce then
        s ^ (string_of_int !nonce)
      else iterate_check (n - 1) in
    iterate_check iters
  end 

  let nonce = Mining.mine "ASD" 20000000 ;;
  print_endline ("WORKING NONCE: " ^ nonce ^ "Hash: " ^ Mining.hash_text nonce) ;;
