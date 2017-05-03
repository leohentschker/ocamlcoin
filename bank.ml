open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Payments.Transaction
open Merkletree
open IOHelpers
open Mining
open Mining.Miner

module Bank =
  struct

    let masterkey = snd (generate_keypair ())
    module MT = MakeMerkle (TransactionSerializable) (SHA256)

    type mtree = MT.mtree
    type ledger = MT.mtree ref

    let account = ref MT.empty

    let empty = ref MT.empty

    let query (p : pub_key) (m : ledger) : transaction list =
      (MT.queryid p !m) @ (MT.queryhash (pub_to_string p) !m)

    let verify_transaction (t : transaction) (l: ledger) : bool =
      let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
      let eltlst = MT.queryid id1 !l in
      let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in
      let total_amount = List.fold_left
        (fun acc x -> if x#originator = id1 then acc -. x#amount
                      else acc +. x#amount) 0. timedlst in
      not (eltlst = [] || id1 = masterkey) && (total_amount < amount) &&
           amount > 0. && authenticate_transaction t &&
           Mining.Miner.verify t#to_string t#solution

    let add_transaction (t : transaction) (l : ledger) : unit =
      if verify_transaction t l then
        account := (MT.add_element t !account)

    let verify_ledger (t : ledger) : bool =
      let rec verify (t : ledger) (n : int) : bool =
        if n = 0 then true
        else
          let tlist = MT.children !t in
          let slist = sublist tlist 0 (n - 1) in
          let tn = List.nth tlist n in
          verify_transaction tn (ref (MT.build_tree slist)) && (verify t (n - 1)) in
      verify t (List.length (MT.children !t) - 1)

    let merge_ledgers (tree1 : ledger)
                      (tree2 : ledger) : unit =
      if not ((verify_ledger tree1) && (verify_ledger tree2))
        then raise (Invalid_argument "Stop trying to cheat")
      else if ((MT.root_hash !tree1 = MT.root_hash !tree2)
              || (!tree2 = MT.empty)) then ()
      else List.iter (fun e -> (add_transaction e tree1))
                     (List.filter (fun e -> not (List.memq e (MT.children !tree1)))
                                  (MT.children !tree2));;
  end
