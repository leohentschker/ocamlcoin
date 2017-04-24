open Nocrypto

Module Mining = 
  struct
  (* Takes a string and extracts the SHA256 hash of that string*)
  let hash_text s =
      let init = Hash.SHA256.init () in
      Hash.SHA256.feed init (Cstruct.of_string s);
      let digest = Hash.SHA256.digest (Hash.SHA256.get init) in
      Cstruct.to_string digest ;;
  (* Checks whether or not the hash of str with nonce on the back
     has *size* or more zeroes on the front *)
  let verify (size : int) (str : string) (nonce : int) : bool =
    let combo = s ^ (string_of_int nonce) in
    let hashed = hash_text combo in
    let first_chars = Str.first_chars hashed size in
    if first_chars = String.make n "0"
    then true 
    else false
  (* Implementation of the mining algorithm for proof-of-work *)
  let mine (s : string) (iters: int) : string =
  (* Beginning nonce that is check*)
    let nonce = ref 0 in
    let leading_zero_size = 10 in
    (* This inner function takes an integer and checks
       the has verify of s^nonce for nonce = 1,..., n *)
    let rec iterate_check n =
      if n = 0 then failwith "COULDN'T FIND" else
      nonce := !nonce + 1;
      if verify leading_zero_size s !nonce
        combo
      else iterate_check (n - 1) in
    iterate_check iters
  ;;
  end 

let nonce = mine "ASD" ;;
print_endline ("WORKING NONCE: " ^ nonce) ;;
