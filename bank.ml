open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Merkletree
module MT = MakeMerkle (TransactionSerializable) (SHA256)
module W = Wallet.MakeWallet

let ledger = ref MT.empty
let wallet = ref W.empty

let verify_transaction (t : transaction) : bool =
  let id1, id2, amount = t#originator, t#target, t#amount in
  match W.find !wallet id1 with
  | None -> false
  | Some b -> not (W.find !wallet id2 = None) && b < amount

let add_transaction (t : transaction) : unit =
  if verify_transaction t then
    ledger := (MT.add_element t !ledger);
    let id1, id2, amount = t#originator, t#target, t#amount in
    match W.find !wallet id1, W.find !wallet id2 with
    | (Some currb1), (Some currb2) ->
        wallet :=
          (W.insert (W.insert !wallet id1 (currb1 -. amount)) id2 (currb2 +. amount))
    | _, _ -> ()

let merge_trees (t1 : MT.mtree ref)
                (t2 : MT.mtree ref) : unit =
  t1 := !t2

let query (s : string) (m : MT.mtree ref) : transaction list =
  (MT.queryid (string_to_pub s) !m) @ (MT.queryhash s !m)
