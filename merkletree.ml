open Sexplib
open Crypto
open Crypto.Keychain
open Payments

type ordering = L | G | E

module type SERIALIZE =
  sig
    type amount
    type time
    type t
    type id
    val serialize : t -> string
    val gen : unit -> t
    val get : t -> (id * id * amount * time)
    val compare : time -> time -> ordering
    val min : time -> time -> time
  end


module TransactionSerializable : (SERIALIZE with type amount = float 
                                             and type time = float 
                                             and type t = transaction
                                             and type id = pub_key) =
  struct
    type amount = float
    type time = float
    type t = transaction
    type id = pub_key
    let serialize t = t#to_string
    let fake_transaction_data () =
      let _, originator = generate_keypair () in
      let _, target = generate_keypair () in
      let amount = Random.float 1000. in
      let timestamp = Random.float 10000. in
      originator, target, amount, timestamp
    let gen () =
      let originator, target, amount, timestamp = fake_transaction_data () in
      new transaction originator target amount timestamp
    let get (t : transaction) = (t#originator, t#target, t#amount, t#timestamp)
    let compare (t1 : time) (t2 : time) : ordering =
      if t1 < t2 then L
      else if t1 > t2 then G
      else E
    let min (t1 : time) (t2 : time) : time =
      if compare t1 t2 = L then t1 else t2
  end


module type MERKLETREE =
  sig
    type element
    type amount
    type id
    type time
    val get : element -> id * id * amount * time
    val serializelist : element list -> string list
    val base_hash : element -> string
    val tree_hash : string -> string
    type mtr
    type mtree
    val root_hash: mtree -> string
    val sublist : 'a list -> int -> int -> 'a list
    val half_list : 'a list -> 'a list * 'a list
    val split_list : 'a list -> 'a list * 'a list
    val combine_trees : mtree -> mtree -> mtree
    val tree_helper : element list -> mtree
    val build_tree : element list -> mtree
    val merge_trees : mtree -> mtree -> unit
    val children : mtree -> element list
    val add_element : element -> mtree -> mtree
    val queryid : id -> mtree -> element list
    val queryhash : string -> mtree -> element list
    val run_tests : unit -> unit
  end

module MakeMerkle (S : SERIALIZE) (H : HASH) : (MERKLETREE with type element = S.t
                                                            and type id = S.id
                                                            and type amount = S.amount
                                                            and type time = S.time) =
  struct

    type element = S.t

    type id = S.id
    type amount = S.amount
    type time = S.time

    let get = S.get

    let serializelist = List.map S.serialize

    let base_hash (data : element) : string =
      H.hash_text (S.serialize data)

    let tree_hash (s : string) : string =
      H.hash_text s

    type mtr =
      Leaf of string * element | Tree of string * id list * time * mtree * mtree
    and mtree = mtr ref

    let root_hash (t : mtree) : string =
      match !t with
      | Leaf (s, _) -> s
      | Tree (s, _, _, _, _) -> s

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

    let union (l1 : 'a list) (l2 : 'a list) : 'a list =
      List.fold_left (fun xs x -> if not (List.mem x l1) then xs @ [x] else xs) l1 l2;;

    let combine_trees (t1 : mtree) (t2 : mtree) : mtree =
      match !t1, !t2 with
      | Leaf (s1, e1), Leaf (s2, e2) ->
          let (id11, id12, _, time1), (id21, id22, _, time2) = get e1, get e2 in
          ref (Tree (tree_hash (s1 ^ s2), union [id11; id12] [id21; id22], S.min time1 time2, t1, t2))
      | Leaf (s1, e), Tree (s2, lst, time2, _, _) ->
          let (id1, id2, _, time1) = get e in
          ref (Tree (tree_hash (s1 ^ s2), union [id1; id2] lst, S.min time1 time2, t1, t2))
      | Tree (s1, lst, time1, _, _), Leaf (s2, e) ->
          let (id1, id2, _, time2) = get e in
          ref (Tree (tree_hash (s1 ^ s2), union lst [id1; id2], S.min time1 time2, t1, t2))
      | Tree (s1, l1, time1, _, _), Tree (s2, l2, time2, _, _) ->
          ref (Tree (tree_hash (s1 ^ s2), union l1 l2, S.min time1 time2, t1, t2))

    let rec tree_helper (lst : element list) : mtree =
      let (l, r) = half_list lst in
      match List.length lst with
      | 0 -> failwith "Empty Tree"
      | 1 -> let e = List.hd lst in ref (Leaf (base_hash e, e))
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

    let rec children (t : mtree) : element list =
      match !t with
      | Leaf (_, e) -> [e]
      | Tree (_, _, _, t1, t2) -> (children t1) @ (children t2)

    let rec add_element (e : element) (t : mtree) : mtree =
      let newleaf = build_tree [e] in
      match !t with
      | Leaf (_, _) ->
          combine_trees t newleaf
      | Tree (_, _, _, t1, t2) ->
          if List.length (children t1) = List.length (children t2)
            then combine_trees t newleaf
          else combine_trees t1 (add_element e t2)

    let rec queryid (id : id) (t : mtree) : element list =
      match !t with
      | Leaf (_, e) ->
          let (id1, id2, _, _) = get e in
          if id = id1 || id = id2 then [e] else []
      | Tree (_, lst, _, l, r) ->
          if List.mem id lst then (queryid id l) @ (queryid id r)
          else []

    let queryhash (hash : string) (t : mtree) : element list =
      match !t with
      | Leaf (s, _) | Tree (s, _, _, _, _) -> if hash = s then (children t) else []

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

    let test_combine_trees () = 
      let e1 = S.gen () in
      let e2 = S.gen () in
      let e3 = S.gen () in
      let e4 = S.gen () in
      let e5 = S.gen () in 
      let e6 = S.gen () in
      let l1 = [e1; e2; e3] in
      let l2 = [e4; e5; e6] in
      let lcomb = l1 @ l2 in 
      let t1 = build_tree l1 in
      let t2 = build_tree l2 in
      assert (root_hash (combine_trees t1 t2) = root_hash (build_tree lcomb))

    let run_tests () =
      test1 () ;
      test_combine_trees () ; 
      print_endline "All tests passed" ;
      ()

  end

module FakeMerkle = MakeMerkle (TransactionSerializable) (SHA256) ;;

let _ = FakeMerkle.run_tests ();;


