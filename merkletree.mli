open Sexplib
open IOHelpers
open Crypto
open Crypto.Keychain
open Payments.Transaction

type ordering

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
                                             and type id = pub_key)

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

module  MakeMerkle (S : SERIALIZE) (H : HASH) : (MERKLETREE with type element = S.t
                                                            and type id = S.id
                                                            and type amount = S.amount
                                                            and type time = S.time)
