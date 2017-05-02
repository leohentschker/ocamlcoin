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

let merge_trees (tree1 : MT.mtree ref)
                (tree2 : MT.mtree ref) : unit =
  let rec merge_helper (subtree1 : MT.mtree ref)
                       (subtree2 : MT.mtree ref) : unit =
    if not ((verify_tree !subtree1) || (verify_tree !subtree2))
      then raise (Invalid_argument "Stop trying to cheat")
    else if (MT.root_hash subtree1 = MT.root_hash subtree2) then ()
    else
      match !subtree1, !subtree2 with
      | Empty, _ -> subtree1 := !subtree2
      | _, Empty -> subtree2 := !subtree1
      | _, Leaf (s2, e2) -> add_transaction e2 t1
      | Leaf (_, _), Tree (_, _, _, lt, lr) -> merge_helper t1 lt;
                                               merge_helper t1 lr
      | Tree (_, _, _, lt1, lr1), Tree (_, _, _, lt2, lr2) ->
          merge_helper lt1 lt2;
          merge_helper lr1 lr2
  in
merge_helper tree1 tree2 ;;

let query (s : string) (m : MT.mtree ref) : transaction list =
  (MT.queryid (string_to_pub s) !m) @ (MT.queryhash s !m)
