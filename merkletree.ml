open Sexplib
open IOHelpers
open Crypto
open Crypto.Keychain
open Payments.Transaction
(* Need an ordering on your serializable elements,
   because we're putting the elements into a tree*)

(* Abstraction that is largely modeled after transaction in payments.ml *)
module type SERIALIZE =
  sig
    type amount
    type ordering
    type time
    type t
    type id
    val serialize : t -> string
    val gen : unit -> t
    val get : t -> (id * id * amount * time * id)
    val compare : time -> time -> ordering
    val min : time -> time -> time
  end

(* Instantation of Serializable in terms of Transactions. This is the
    basis of our Merkle tree for our global ledger for rhe OCamlcoin
    network. Various types are exposed for use in future functions. *)

module TransactionSerializable : (SERIALIZE with type amount = float
                                             and type time = float
                                             and type t = transaction
                                             and type id = pub_key) =
  struct
    type amount = float

    type ordering = int

    type time = float

    type t = transaction

    type id = pub_key

    let serialize t = t#to_string
    (* Sample generation of randomt transaction data*)
    let fake_transaction_data () =
      let _, originator = generate_keypair () in
      let _, target = generate_keypair () in
      let amount = Random.float 1000. in
      let timestamp = Random.float 10000. in
      originator, target, amount, timestamp
      (* Generates a random transaction *)

    let gen () =
      let originator, target, amount, timestamp = fake_transaction_data () in
      let priv, pub = generate_keypair () in
      create_transaction originator target amount timestamp priv

    let get (t : transaction) = (t#originator, t#target, t#amount, t#timestamp, t#solver)

    let compare (t1 : time) (t2 : time) : ordering = compare t1 t2

    let min (t1 : time) (t2 : time) : time = if compare t1 t2 = -1 then t1
                                             else t2
  end

(* Type signature for the MERKLETREE module, which includes functions
   on trees. *)
module type MERKLETREE =
  sig
    type element
    type amount
    type id
    type time
    val get : element -> id * id * amount * time * id
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
    val querysolver : id -> mtree -> element list
    val run_tests : unit -> unit
  end

(* Functor that takes in a serializable and a hash module, which is just a
   module that contains a cryptographic function, and returns a merkle tree
   with appropriate types *)
module MakeMerkle (S : SERIALIZE) (H : HASH) :
  (MERKLETREE with type element = S.t
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
    (* *)
    let base_hash (data : element) : string =
      H.hash_text (S.serialize data)

    let tree_hash (s : string) : string =
      H.hash_text s
      (* Our merkle tree will, in generate, store at each node a hash of
         the transactions, a list of the users who were involved in the
         transactions, and a timestamp *)
    type mtree =
      | Empty
      | Leaf of string * element
      | Tree of string * id list * time * mtree * mtree

    let empty = Empty
    (* root_hash function returns the top root of the tree, which is the result
       of hashing all of the other transactions together *)
    let root_hash (t : mtree) : string =
      match t with
      | Empty -> ""
      | Leaf (s, _) -> s
      | Tree (s, _, _, _, _) -> s
    (* Combines merkle trees, creating a new tree with a correct top hash and
       the initial trees as subtrees. Combines left to right. *)
    let combine_trees (t1 : mtree) (t2 : mtree) : mtree =
      let union (l1 : 'a list) (l2 : 'a list) : 'a list =
        List.fold_left (fun xs x -> if not (List.mem x l1)
                                      then xs @ [x]
                                    else xs) l1 l2 in
      match t1, t2 with
      | Leaf (s1, e1), Leaf (s2, e2) ->
          let (id11, id12, _, time1, _), (id21, id22, _, time2, _) = get e1, get e2 in
          (Tree (tree_hash (s1 ^ s2), union [id11; id12] [id21; id22],
                 S.min time1 time2, t1, t2))
      | Leaf (s1, e), Tree (s2, lst, time2, _, _) ->
          let (id1, id2, _, time1, _) = get e in
          (Tree (tree_hash (s1 ^ s2), union [id1; id2] lst,
                 S.min time1 time2, t1, t2))
      | Tree (s1, lst, time1, _, _), Leaf (s2, e) ->
          let (id1, id2, _, time2, _) = get e in
          (Tree (tree_hash (s1 ^ s2), union lst [id1; id2],
                 S.min time1 time2, t1, t2))
      | Tree (s1, l1, time1, _, _), Tree (s2, l2, time2, _, _) ->
          (Tree (tree_hash (s1 ^ s2), union l1 l2,
                 S.min time1 time2, t1, t2))
      | Empty, _ -> t2
      | _, Empty -> t1

    (* Given an element list, creates the corresponding merkle tree left to
       right. Contains helper functions. *)
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
      (* Splits a list into two lists, the first of which with length as the
         greatest power of 2 less than or equal to the initial list length. This
         is done so our trees satisfy the invariant that any left subtree is
         always a complete binary tree. *)
      let rec split_list (lst : 'a list) : 'a list * 'a list =
        let len = List.length lst in
        (sublist lst 0 (exp2 (log2 len) - 1),
         sublist lst (exp2 (log2 len)) (len - 1)) in
      (* Allows us to build trees from lists that are exact powers of 2. *)
      let rec tree_helper (lst : element list) : mtree =
        let (l, r) = half_list lst in
        match List.length lst with
        | 0 -> Empty
        | 1 -> let e = List.hd lst in (Leaf (base_hash e, e))
        | _ -> let ltree, rtree = tree_helper l, tree_helper r in
               combine_trees ltree rtree in
      (* Recursively creates a tree from an element list by splitting it into
         its base 2 representation and using tree_helper to create the separate
         trees, making every left subtree a complete binary tree *)
      let (l, r) = split_list datalist in
      if r = [] then tree_helper datalist
      else match List.length datalist with
           | 0 | 1 -> tree_helper datalist
           | _ -> let ltree, rtree = tree_helper l, build_tree r in
                  combine_trees ltree rtree

    (* Given a tree, returns the elements stored at the leaves, left to right.*)
    let rec children (t : mtree) : element list =
      match t with
      | Empty -> []
      | Leaf (_, e) -> [e]
      | Tree (_, _, _, t1, t2) -> (children t1) @ (children t2)

    (* Adds an element to a tree, returning a new tree with a new calculated
       root hash *)
    let rec add_element (e : element) (t : mtree) : mtree =
      let newleaf = build_tree [e] in
      match t with
      | Empty | Leaf (_, _) ->
          combine_trees t newleaf
      | Tree (_, _, _, t1, t2) ->
          if List.length (children t1) = List.length (children t2)
            then combine_trees t newleaf
          else combine_trees t1 (add_element e t2)

    (* Some querying functions. *)
    let rec queryid (id : id) (t : mtree) : element list =
      match t with
      | Empty -> []
      | Leaf (_, e) ->
          let (id1, id2, _, _, _) = get e in
          if id = id1 || id = id2 then [e] else []
      | Tree (_, lst, _, l, r) ->
          if List.mem id lst then (queryid id l) @ (queryid id r)
          else []

    let queryhash (hash : string) (t : mtree) : element list =
      match t with
      | Empty -> []
      | Leaf (s, _) | Tree (s, _, _, _, _) -> if hash = s then (children t)
                                              else []

    let querysolver (id : id) (t : mtree) : element list =
      match t with
      | Empty -> []
      | Leaf (_, _) | Tree (_, _, _, _, _) ->
          let lst = children t in
          let rlst = List.filter (fun x -> let (_, _, _, _, s) = get x in s = id) lst

    let test_add_element () =
      let l1, l2 = TestHelpers.generate_list S.gen (Random.int 20),
                   TestHelpers.generate_list S.gen (Random.int 20) in
      let t1 = List.fold_left (fun t e -> add_element e t) (build_tree l1) l2 in
      (* Also tests build_tree *)
      let t2 = build_tree (l1 @ l2) in
      assert (root_hash t1 = root_hash t2)

    let test_combine () =
      let l1, l2 = TestHelpers.generate_list S.gen 4,
                   TestHelpers.generate_list S.gen 3 in
      let lcomb = l1 @ l2 in
      let t1 = build_tree l1 in
      let t2 = build_tree l2 in
      let tbig = build_tree lcomb in
      let tcomb = combine_trees t1 t2 in
      assert (children t1 = l1);
      assert (children t2 = l2);
      (* Also tests root_Hash and children *)
      assert (root_hash tbig = root_hash tcomb);
      assert (children tbig = children tcomb)

    let test_queryid () =
      let lst = TestHelpers.generate_list S.gen (Random.int 20) in
      let tree = build_tree lst in
        match tree with
        | Tree (str, id_list, t, l, r) ->
            List.iter
              (fun id -> (List.iter (fun e -> assert (List.memq e lst)))
                         (queryid id tree))
              id_list
        | Leaf (s, e) -> assert ([e] = lst)
        | _ -> ()

    let run_tests () =
      TestHelpers.run_tests test_add_element;
      TestHelpers.run_tests test_combine;
      TestHelpers.run_tests test_queryid
  end
