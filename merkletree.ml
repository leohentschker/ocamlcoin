open Sexplib
(* open Crypto_fake *)

open Crypto
open Keychain
open Payments


module type SERIALIZE =
  sig
    type t
    val serialize : t -> string
    val gen : unit -> t
  end

module IntSerializable : SERIALIZE =
  struct
    type t = int
    let serialize = string_of_int
    let gen =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
  end

module TransactionSerializable : SERIALIZE =
  struct
    type t = transaction
    let serialize t = t#to_string
    let fake_transaction_data () =
      let _, originator = generate_keypair () in
      let _, target = generate_keypair () in
      let amount = Random.float 1000. in
      originator, target, amount
    let gen () =
      let originator, target, amount = fake_transaction_data () in
      new transaction originator target amount
  end


module type MERKLETREE =
  sig
    type element
    val serializelist : element list -> string list
    val base_hash : element -> string
    val tree_hash : string -> string
    type mtr
    type mtree
    val root_hash: mtree -> string
    val sublist : 'a list -> int -> int -> 'a list
    (* Don't think we need log2 or exp2? Come back. *)
    val half_list : 'a list -> 'a list * 'a list
    val split_list : 'a list -> 'a list * 'a list
    val combine_trees : mtree -> mtree -> mtree
    val tree_helper : element list -> mtree
    val build_tree : element list -> mtree
    val merge_trees : mtree -> mtree -> unit
    val children : mtree -> string list
    val add_element : element -> mtree -> mtree
    val run_tests : unit -> unit
    (* add testing *)
  end

module MakeMerkle (S : SERIALIZE) (H : HASH) : (MERKLETREE with type element = S.t) =
  struct

    type element = S.t

    let serializelist = List.map S.serialize

    let base_hash (data : element) : string =
      H.hash_text (S.serialize data)

    let tree_hash (s : string) : string =
      H.hash_text s

    type mtr =
      Leaf of string | Tree of string * mtree * mtree
    and mtree = mtr ref

    let root_hash (t : mtree) : string =
      match !t with
      | Leaf s -> s
      | Tree (s, _, _) -> s

    let rec sublist (lst : 'a list) (a : int) (b : int) : 'a list =
      if b < a then []
      else match lst with
           | [] -> failwith "Empty List"
           | h :: t ->
              (match (a, b) with
               | (0, 0) -> [h]
               | (0, _) -> [h] @ sublist t a (b - 1)
               | (_, _) -> sublist t (a - 1) (b - 1))

    let log2 (n : int) : int =
      truncate (log (float n) /. (log 2.))

    let rec exp2 (n : int) : int =
      match n with
      | 0 -> 1
      | _ -> 2 * exp2 (n - 1)

    let half_list (lst : 'a list) : 'a list * 'a list =
      let len = List.length lst in
      (sublist lst 0 (len / 2 - 1), sublist lst (len / 2) (len - 1))

    let rec split_list (lst : 'a list) : 'a list * 'a list =
      let len = List.length lst in
      (sublist lst 0 (exp2 (log2 len) - 1), sublist lst (exp2 (log2 len)) (len - 1))

    let combine_trees (t1 : mtree) (t2 : mtree) : mtree =
      ref (Tree (tree_hash ((root_hash t1) ^ (root_hash t2)), t1, t2))

    let rec tree_helper (lst : element list) : mtree =
      let (l, r) = half_list lst in
      match List.length lst with
      | 0 -> failwith "Empty Tree"
      | 1 -> ref (Leaf (base_hash (List.hd lst)))
      | _ -> let ltree, rtree = tree_helper l, tree_helper r in
             combine_trees ltree rtree

    let rec build_tree (datalist : element list) : mtree=
      let (l, r) = split_list datalist in
      if r = [] then tree_helper datalist
      else match List.length datalist with
           | 0 | 1 -> tree_helper datalist
           | _ -> let ltree, rtree = tree_helper l, build_tree r in
                  combine_trees ltree rtree

    let merge_trees (t1 : mtree) (t2 : mtree) : unit =
      t1 := !t2

    let rec children (t : mtree) : string list =
      match !t with
      | Leaf s -> [s]
      | Tree (_, t1, t2) -> (children t1) @ (children t2)

    let rec add_element (e : element) (t : mtree) : mtree =
      let newleaf = build_tree [e] in
      match !t with
      | Leaf _ ->
        combine_trees t newleaf
      | Tree (_, t1, t2) ->
        if List.length (children t1) = List.length (children t2)
          then combine_trees t newleaf
        else combine_trees t1 (add_element e t2)

    let test1 () =
      let e1 = S.gen () in
      let e2 = S.gen () in
      let e3 = S.gen () in
      let e4 = S.gen () in
      let e5 = S.gen () in
      let l1 = [e1; e2; e3; e4] in
      let l2 = l1 @ [e5] in
      let t1 = add_element e5 (build_tree l1) in
      let t2 = build_tree l2 in
      assert (root_hash t1 = root_hash t2)

    let run_tests () =
      test1 () ;
      print_endline "All tests passed" ;
      ()

  end

module FakeMerkle = MakeMerkle (IntSerializable) (SHA256) ;;

module RealMerkle = MakeMerkle (TransactionSerializable) (SHA256) ;;

let _ = RealMerkle.run_tests () ;;
