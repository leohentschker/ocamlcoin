open Nocrypto


let hash_text s =
    let init = Hash.SHA256.init () in
    Hash.SHA256.feed init (Cstruct.of_string s);
    let digest = Hash.SHA256.digest (Hash.SHA256.get init) in
    Cstruct.to_string digest ;;

let mine s =
  let ctr = ref 0 in
  let leading_zero_size = 1 in
  let rec aux n =
    if n = 0 then failwith "COULDNT FIND" else
    ctr := !ctr + 1;
    let combo = s ^ (string_of_int !ctr) in
    let hashed = hash_text combo in
    let first_chars = Str.first_chars hashed leading_zero_size in
    if first_chars = "0" then
      combo
    else aux (n - 1) in
  aux 200
;;

let nonce = mine "ASD" ;;
print_endline ("WORKING NONCE: " ^ nonce) ;;
