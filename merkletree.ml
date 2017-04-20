

    let base_hash (data : 'a) : string =
      ""

    let tree_hash (s : string) : string =
      ""

    type mtree =
      Empty | Leaf of string | Tree of string * mtree * mtree

    let root_hash (t : mtree) =
      match t with
      | Empty -> ""
      | Leaf s -> s
      | Tree (s, l, r) -> s

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

(*
    let rec split_list (lst : 'a list) : ('a list) list =
      match List.length lst with
      | 0 -> []
      | len ->
      (sublist lst 0 (exp2 (log2 len) - 1)) :: split_list (sublist lst (exp2 (log2 len)) (len - 1))
*)

    let half_list (lst : 'a list) : 'a list * 'a list =
      let len = List.length lst in
      (sublist lst 0 (len / 2 - 1), sublist lst (len / 2) (len - 1))

    let rec split_list (lst : 'a list) : 'a list * 'a list =
      let len = List.length lst in
      (sublist lst 0 (exp2 (log2 len) - 1), sublist lst (exp2 (log2 len)) (len - 1))

    let merge_trees (t1 : mtree) (t2 : mtree) : mtree =
      Tree (tree_hash (root_hash t1) ^ (root_hash t2), t1, t2)

    let rec helpertree (lst : 'a list) =
      let (l, r) = half_list lst in
      match List.length lst with
      | 0 -> failwith "Empty Tree"
      | 1 -> Leaf (base_hash (List.hd lst))
      | _ -> let ltree, rtree = helpertree l, helpertree r in
             merge_trees ltree rtree

    let rec build_tree (datalist : 'a list) =
      let (l, r) = split_list datalist in
      if r = [] then helpertree datalist
      else match List.length datalist with
           | 0 | 1 -> helpertree datalist
           | _ -> let ltree, rtree = helpertree l, build_tree r in
                  merge_trees ltree rtree

