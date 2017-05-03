open Sexplib
open Crypto
open Crypto.Keychain
open Payments.Transaction
open Merkletree
open IOHelpers
open Mining
open Mining.Miner

let masterkey = snd (generate_keypair ())
module MT = MakeMerkle (TransactionSerializable) (SHA256)

let ledger = ref MT.empty

let verify_transaction (t : transaction) (l: MT.mtree ref) : bool =
  let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
  let eltlst = MT.queryid id1 !l in
  let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in
  let total_amount = List.fold_left
    (fun acc x -> if x#originator = id1 then acc -. x#amount
                  else acc +. x#amount) 0. timedlst in
  not (eltlst = [] || id1 = masterkey) && (total_amount < amount) &&
       amount > 0. && authenticate_transaction t &&
       Mining.Miner.verify t#to_string t#solution

let add_transaction (t : transaction) (l : MT.mtree ref) : unit =
  if verify_transaction t l then
    ledger := (MT.add_element t !ledger)

let verify_tree (t : MT.mtree) =
  let rec verify (t : MT.mtree) (n : int) : bool =
    if n = 0 then true
    else
      let tlist = MT.children t in
      let slist = sublist tlist 0 (n - 1) in
      let tn = List.nth tlist n in
      verify_transaction tn (ref (MT.build_tree slist)) && (verify t (n - 1)) in
  verify t (List.length (MT.children t) - 1)

let merge_ledgers (tree1 : MT.mtree ref)
                  (tree2 : MT.mtree ref) : unit =
  if not ((verify_tree !tree1) || (verify_tree !tree2))
    then raise (Invalid_argument "Stop trying to cheat")
  else if ((MT.root_hash !tree1 = MT.root_hash !tree2)
          || (!tree2 = MT.empty)) then ()
  else List.iter (fun e -> (add_transaction e tree1))
                 (List.filter (fun e -> not (List.memq e (MT.children !tree1)))
                              (MT.children !tree2));;

let query (s : string) (m : MT.mtree ref) : transaction list =
  (MT.queryid (string_to_pub s) !m) @ (MT.queryhash s !m)
