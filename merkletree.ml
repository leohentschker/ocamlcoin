open Sexplib
open Crypto
  
    let base_hash (data : string) : string =
      SHA256.hash_text data

    let tree_hash (s : string) : string =
      SHA256.hash_text s

    type mtr =
      Leaf of string | Tree of string * mtree * mtree
    and mtree = mtr ref

    let root_hash (t : mtree) : string =
      match !t with
      | Leaf s -> s
      | Tree (s, l, r) -> s

    let rec sublist (lst : string list) (a : int) (b : int) : string list =
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

(*
    let rec split_list (lst : string list) : (string list) list =
      match List.length lst with
      | 0 -> []
      | len ->
      (sublist lst 0 (exp2 (log2 len) - 1)) :: split_list (sublist lst (exp2 (log2 len)) (len - 1))
*)

    let half_list (lst : string list) : string list * string list =
      let len = List.length lst in
      (sublist lst 0 (len / 2 - 1), sublist lst (len / 2) (len - 1))

    let rec split_list (lst : string list) : string list * string list =
      let len = List.length lst in
      (sublist lst 0 (exp2 (log2 len) - 1), sublist lst (exp2 (log2 len)) (len - 1))

    let combine_trees (t1 : mtree) (t2 : mtree) : mtree =
      ref (Tree (tree_hash (root_hash t1) ^ (root_hash t2), t1, t2))

    let rec helpertree (lst : string list) : mtree =
      let (l, r) = half_list lst in
      match List.length lst with
      | 0 -> failwith "Empty Tree"
      | 1 -> ref (Leaf (base_hash (List.hd lst)))
      | _ -> let ltree, rtree = helpertree l, helpertree r in
             combine_trees ltree rtree

    let rec build_tree (datalist : string list) : mtree=
      let (l, r) = split_list datalist in
      if r = [] then helpertree datalist
      else match List.length datalist with
           | 0 | 1 -> helpertree datalist
           | _ -> let ltree, rtree = helpertree l, build_tree r in
                  combine_trees ltree rtree

    let merge_trees (t1 : mtree) (t2 : mtree) : unit =
      t1 := !t2

    let rec children (t : mtree) : string list = 
      match !t with
      | Leaf s -> [s]
      | Tree (s, t1, t2) -> (children t1) @ (children t2)

    let rec add_string (s : string) (t : mtree) : mtree = 
      let hashed = base_hash s in
      let sLeaf = helpertree [hashed] in
      match !t with
      | Leaf root -> 
        ref (Tree (tree_hash (root ^ hashed), t, sLeaf))
      | Tree (root, t1, t2) -> 
        if List.length (children t1) = List.length (children t2) 
        then ref (Tree (tree_hash (root ^ hashed), t, sLeaf))
        else combine_trees t1 (add_string s t2)

    let test_add_string () = 
    let hello_list = ["Hello"; "From"; "The"; "Other"; "Side"] in
    let hello2 = hello_list @ ["World"] in
    let tree1 = build_tree hello2 in
    let tree2 = add_string "World" (build_tree hello_list) in
    assert (!tree1 = !tree2)


