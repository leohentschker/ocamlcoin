open Sexplib
open Crypto
open Crypto.Signature
open Payments
open Merkletree
open Wallet

module MT = MakeMerkle (TransactionSerializable) (SHA256)
module W = MakeWallet

let verify_transaction (t : transaction) (w : W.dict) (m : MT.mtree) : bool =
  let id1, id2, amount = t#originator, t#target, t#amount in
  match W.find id1 with
  | None -> false
  | Some b -> not (W.find id2 = None) && b < amount


let add_transaction (t : transaction) (w : W.dict ref) (m : MT.tree) : unit =
  if verify_transaction t !w m then
    m := (MT.add_element t m);
    let id1, id2, amount = t#originator, t#target, t#amount
    let currb1, currb2 = W.find id1, W.find id2 in
    w := (W.insert (W.insert !w id1 (currb1 -. amount)) id2 (currb2 +. amount))

let query (s : string) (m : MT.tree) : transaction list =
  (queryid (string_to_pub s) m) @ (queryhash s m)



