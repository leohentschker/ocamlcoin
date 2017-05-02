open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Merkletree

(* let MasterId = generate_keypair () *)
module MT = MakeMerkle (TransactionSerializable) (SHA256)

let ledger = ref MT.empty

let verify_transaction (t : transaction) : bool =
  let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
  let eltlst = MT.queryid id1 !ledger in 
  let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in 
  let total_amount = List.fold_left (fun acc x -> acc +. x#amount) 0. timedlst in
  not (eltlst = []) && (total_amount < amount) && 
  (if amount < 0. then not (MT.queryid id2 !ledger = []) else true) &&
  (Crypto.Signature.verify t#to_string t#pub_key t#signature)

let add_transaction (t : transaction) : unit =
  if verify_transaction t then
    ledger := (MT.add_element t !ledger)

let merge_trees (t1 : MT.mtree ref)
                (t2 : MT.mtree ref) : unit =
  t1 := !t2

let query (s : string) (m : MT.mtree ref) : transaction list =
  (MT.queryid (string_to_pub s) !m) @ (MT.queryhash s !m)