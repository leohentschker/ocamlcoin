open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Merkletree
open IOhelpers
open Bank

Tree of string * id list * time * mtree * mtree
(* let MasterId = generate_keypair () *)
module MT = MakeMerkle (TransactionSerializable) (SHA256)

let merge_trees (tree1 : MT.mtree ref)
                (tree2 : MT.mtree ref) : unit =
  let rec merge_helper (subtree1 : MT.mtree ref)
                       (subtree2 : MT.mtree ref) : unit =
    if not ((verify_tree !subtree1) || (verify_tree !subtree2))
      then raise (Invalid_argument "Stop trying to cheat")
    else if (MT.root_hash subtree1 = MT.root_hash subtree2)
            || (subtree2 = MT.empty)
      then ()
    else
      match !subtree1, !subtree2 with
      | Empty, _ -> subtree1 := !subtree2
      | _, Leaf (s2, e2) -> add_transaction e2 t1
      | Leaf (_, _), Tree (_, _, _, lt, lr) -> merge_helper t1 lt;
                                               merge_helper t1 lr
      | Tree (_, _, _, lt1, lr1), Tree (_, _, _, lt2, lr2) ->
          merge_helper lt1 lt2;
          merge_helper lr1 lr2
  in
merge_helper tree1 tree2 ;;
