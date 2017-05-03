open Sexplib
open IOHelpers
open Crypto
open Crypto.Keychain
open Payments.Transaction

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
      let priv, pub = Keychain.generate_keypair () in
      create_transaction originator target amount timestamp priv
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
    type mtree
    val empty : mtree
    val root_hash: mtree -> string
    val combine_trees : mtree -> mtree -> mtree
    val build_tree : element list -> mtree
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

    type mtree =
      Empty | Leaf of string * element | Tree of string * id list * time * mtree * mtree

    let empty = Empty

    let root_hash (t : mtree) : string =
      match t with
      | Empty -> ""
      | Leaf (s, _) -> s
      | Tree (s, _, _, _, _) -> s

    let union (l1 : 'a list) (l2 : 'a list) : 'a list =
      List.fold_left (fun xs x -> if not (List.mem x l1) then xs @ [x] else xs) l1 l2;;

    let combine_trees (t1 : mtree) (t2 : mtree) : mtree =
      match t1, t2 with
      | Leaf (s1, e1), Leaf (s2, e2) ->
          let (id11, id12, _, time1), (id21, id22, _, time2) = get e1, get e2 in
          (Tree (tree_hash (s1 ^ s2), union [id11; id12] [id21; id22], S.min time1 time2, t1, t2))
      | Leaf (s1, e), Tree (s2, lst, time2, _, _) ->
          let (id1, id2, _, time1) = get e in
          (Tree (tree_hash (s1 ^ s2), union [id1; id2] lst, S.min time1 time2, t1, t2))
      | Tree (s1, lst, time1, _, _), Leaf (s2, e) ->
          let (id1, id2, _, time2) = get e in
          (Tree (tree_hash (s1 ^ s2), union lst [id1; id2], S.min time1 time2, t1, t2))
      | Tree (s1, l1, time1, _, _), Tree (s2, l2, time2, _, _) ->
          (Tree (tree_hash (s1 ^ s2), union l1 l2, S.min time1 time2, t1, t2))
      | Empty, _ -> t2
      | _, Empty -> t1

    let rec build_tree (datalist : element list) : mtree =
      let log2 (n : int) : int =
        truncate (log (float n) /. (log 2.)) in
      let rec exp2 (n : int) : int =
        match n with
        | 0 -> 1
        | _ -> 2 * exp2 (n - 1) in
      let half_list (lst : 'a list) : 'a list * 'a list =
        let len = List.length lst in
        (sublist lst 0 (len / 2 - 1), sublist lst (len / 2) (len - 1)) in
      let rec split_list (lst : 'a list) : 'a list * 'a list =
        let len = List.length lst in
        (sublist lst 0 (exp2 (log2 len) - 1), sublist lst (exp2 (log2 len)) (len - 1)) in
      let rec tree_helper (lst : element list) : mtree =
        let (l, r) = half_list lst in
        match List.length lst with
        | 0 -> Empty
        | 1 -> let e = List.hd lst in (Leaf (base_hash e, e))
        | _ -> let ltree, rtree = tree_helper l, tree_helper r in
               combine_trees ltree rtree in
      let (l, r) = split_list datalist in
      if r = [] then tree_helper datalist
      else match List.length datalist with
           | 0 | 1 -> tree_helper datalist
           | _ -> let ltree, rtree = tree_helper l, build_tree r in
                  combine_trees ltree rtree

    let rec children (t : mtree) : element list =
      match t with
      | Empty -> failwith "empty"
      | Leaf (_, e) -> [e]
      | Tree (_, _, _, t1, t2) -> (children t1) @ (children t2)

    let rec add_element (e : element) (t : mtree) : mtree =
      let newleaf = build_tree [e] in
      match t with
      | Empty | Leaf (_, _) ->
          combine_trees t newleaf
      | Tree (_, _, _, t1, t2) ->
          if List.length (children t1) = List.length (children t2)
            then combine_trees t newleaf
          else combine_trees t1 (add_element e t2)

    let rec queryid (id : id) (t : mtree) : element list =
      match t with
      | Empty -> []
      | Leaf (_, e) ->
          let (id1, id2, _, _) = get e in
          if id = id1 || id = id2 then [e] else []
      | Tree (_, lst, _, l, r) ->
          if List.mem id lst then (queryid id l) @ (queryid id r)
          else []

    let queryhash (hash : string) (t : mtree) : element list =
      match t with
      | Empty -> []
      | Leaf (s, _) | Tree (s, _, _, _, _) -> if hash = s then (children t) else []

    let test_add_element () =
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

    let test_combine_trees_and_children () =
      let e1 = S.gen () in
      let e2 = S.gen () in
      let e3 = S.gen () in
      let e4 = S.gen () in
      let e5 = S.gen () in
      let e6 = S.gen () in
      let e7 = S.gen () in
      let l1 = [e1; e2; e3; e4] in
      let l2 = [e5; e6; e7] in
      let lcomb = l1 @ l2 in
      let t1 = build_tree l1 in
      let t2 = build_tree l2 in
      let tbig = build_tree lcomb in
      let tcomb = combine_trees t1 t2 in
      assert (root_hash tbig = root_hash tcomb);
      assert (children tbig = children tcomb)

    let test_queryid () = 
      let e1 = S.gen () in 
      let e2 = S.gen () in
      let e3 = S.gen () in
      let e4 = S.gen () in
      assert (1 = 1)

    let run_tests () =
      test1 () ;
      test_combine_trees () ;
      print_endline "All tests passed" ;
      ()
  end

module FakeMerkle = MakeMerkle (TransactionSerializable) (SHA256) ;;

let _ = FakeMerkle.run_tests ();;
