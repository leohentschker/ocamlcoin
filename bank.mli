(* open Crypto.Keychain
open Crypto.Signature

module Bank :
  sig
    val masterkey : pub_key
    val ledger : mtree ref
    val query : string -> MT.mtree ref -> transaction list
    val add_transaction : translation -> Merkletree.mtree ref -> unit
    val verify_transaction : transaction -> Merkletree.mtree ref -> bool
    val verify_ledger : Merkletree.mtree -> bool
    val merge_ledgers : Merkletree.mtree ref -> Merkletree.mtree ref -> unit
  end *)
