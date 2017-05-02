open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Merkletree
open Wallet

module MT = MakeMerkle (TransactionSerializable) (SHA256)
module W = MakeWallet

let verify_transaction (t : transaction) (w : W.dict ref) : bool =
  let id1, id2, amount = t#originator, t#target, t#amount in
  match W.find !w id1 with
  | None -> false
  | Some b -> not (W.find !w id2 = None) && b < amount

let add_transaction (t : transaction) (w : W.dict ref) (m : MT.mtree ref) : unit =
  if verify_transaction t w then
    m := (MT.add_element t !m);
    let id1, id2, amount = t#originator, t#target, t#amount in
    let currb1, currb2 = W.find id1, W.find id2 in
    w := (W.insert (W.insert !w id1 (currb1 -. amount)) id2 (currb2 +. amount))

let merge_trees (t1 : MT.mtree ref) (t2 : MT.mtree ref) : unit =
    t1 := !t2

let query (s : string) (m : MT.mtree ref) : transaction list =
  (queryid (string_to_pub s) !m) @ (queryhash s !m)