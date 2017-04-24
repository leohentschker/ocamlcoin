open Nocrypto


let hash_text s =
    let init = Hash.SHA256.init () in
    Hash.SHA256.feed init (Cstruct.of_string s);
    let digest = Hash.SHA256.digest (Hash.SHA256.get init) in
    Cstruct.to_string digest ;;

let mine =
  let ctr = ref 0 in
  let rec aux s =
    ctr := !ctr + 1;
    let hashed = hash_text (s ^ (string_of_int !ctr)) in
    print_endline hashed in
  aux
;;

let _ = mine "ASD" ;;
