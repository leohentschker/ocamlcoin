
(* The functions we want for our merkle tree implementation. *)

	val base_hash : string -> string
	val tree_hash : string -> string
	type mtr
	type mtree
	val root_hash: mtree -> string
	val sublist : string list -> int -> int -> string list
	(* Don't think we need log2 or exp2? Come back. *)
	val half_list : string list -> string list * string list
	val split_list : string list -> string list * string list
	val combine_trees : mtree -> mtree -> mtree
	val helpertree : string list -> mtree
	val build_tree : string list -> mtree
	val merge_trees : mtree -> mtree -> unit
	val children : mtree -> string list
	val add_string : string -> mtree -> mtree

