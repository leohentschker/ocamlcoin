(* commenting for now
(* The functions we want for our merkle tree implementation. *)
module type MERKLETREE
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

*)
