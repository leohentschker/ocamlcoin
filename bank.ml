open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Merkletree
open IOhelpers

(* let MasterId = generate_keypair () *)
module MT = MakeMerkle (TransactionSerializable) (SHA256)

let ledger = ref MT.empty

let verify_transaction (t : transaction) (l: MT.mtree ref): bool =
  let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
  let eltlst = MT.queryid id1 !l in
  let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in
  let total_amount = List.fold_left (fun acc x -> if x#originator = id1 then acc -. x#amount
                                     else acc +. x#amount) 0. timedlst
  in
  not (eltlst = []) && (total_amount < amount) &&
  (if amount < 0. then not (MT.queryid id2 !l = []) else true) &&
  authenticate_transaction t

let add_transaction (t : transaction) (l : MT.mtree ref) : unit =
  if verify_transaction t l then
    ledger := (MT.add_element t !ledger)

let rec verify (t : MT.mtree) (n : int) : bool =
  if n = 0 then true
  else
    let tlist = MT.children t in
    let slist = sublist tlist 0 (n - 1) in
    let tn = List.nth tlist n in
    verify_transaction tn (ref (MT.build_tree slist)) && (verify t (n - 1))

let verify_tree (t : MT.mtree) = verify t (List.length (MT.children t) - 1)

let query (s : string) (m : MT.mtree ref) : transaction list =
  (MT.queryid (string_to_pub s) !m) @ (MT.queryhash s !m)
