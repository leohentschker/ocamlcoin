(*
                             signature.ml
                Here we encrypt and decrypt messages
*)

open Sexplib
open Nocrypto

let () = Nocrypto_entropy_unix.initialize ()

let keys () =
  let private_key = Dsa.generate (`Fips1024) in
  (private_key, Dsa.pub_of_priv private_key) ;;

let to_file (p : Dsa.priv * Dsa.pub) (file : string) : unit =
  Printf.fprintf (open_out file) "Private: %s\n"
    (Sexp.to_string (Dsa.sexp_of_priv (fst p)));
  Printf.fprintf (open_out file) "Public: %s\n"
    (Sexp.to_string (Dsa.sexp_of_pub (snd p))) ;;


(* Here we have some unit tests
*)

let digest = Cstruct.of_string "asd343rdxfdfd" in
let (private_key_unsex, public_key_unsex) = keys () in
let signed = Dsa.sign ~key: private_key_unsex digest in
let verified = Dsa.verify ~key:public_key_unsex signed digest in
if verified then print_endline "Verified!" else raise (Failure "Unverified.") ;;
