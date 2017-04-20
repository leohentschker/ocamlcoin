module Merkle =
  struct

    let base_hash (data : 'a) : string =
      ""

    let tree_hash (s : string) : string =
      ""

    type mtree =
      Leaf of string | Tree of string * tree * tree

    let root_hash (t : mtree) =
      match t with
      | Leaf s -> s
      | Tree (s, l, r) -> s

    let rec sublist (lst : 'a list) (a : int) (b : int) : 'a list =
      match lst with
      | [] -> failwith "Empty List"
      | h :: t ->
          (match (a, b) with
           | (0, 0) -> [h]
           | (0, _) -> [h] @ sublist t a (b - 1)
           | (_, _) -> sublist t (a - 1) (b - 1))

    let split_list (lst : 'a list) : 'a list * 'a list =
      let len = List.length lst in
      (sublist lst 0 (len / 2), sublist lst (len / 2 + 1) (len - 1))

    let rec build_tree (datalist : 'a list) =
      let (l, r) = split_list lst in
      match List.length datalist with
      | 1 -> Leaf (base_hash (List.hd datalist)) ;;
      | _ -> let ltree, rtree = build_tree l, build_tree r in
               Tree (tree_hash (root_hash ltree) (root_hash rtree), ltree, rtree)

  end
