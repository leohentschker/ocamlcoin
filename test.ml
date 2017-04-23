open Sexplib
open Nocrypto

let () = Nocrypto_entropy_unix.initialize ()

let private_key_unsex = Dsa.generate (`Fips1024) ;;
let pub = Dsa.pub_of_priv private_key_unsex ;;

let digest = Cstruct.of_string "asd" ;;
let signed = Dsa.sign ~key:private_key_unsex digest ;;

let string_version = Sexp.to_string (Dsa.sexp_of_priv private_key_unsex) ;;
let private_key = Sexp.of_string string_version ;;

let verified = Dsa.verify ~key:pub signed digest in
if verified then print_endline "FUCK YEAH VERIFIED" else print_endline "SHIT DIDN't WORK" ;;
