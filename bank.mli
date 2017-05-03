open Crypto.Keychain
open Crypto.Signature
open Merkletree


	module MT : Merkletree
    val masterkey : pub_key
    val ledger : MT.mtree ref
    val query : string -> MT.mtree ref -> transaction list
    val add_transaction : translation -> Merkletree.mtree ref -> unit
    val verify_transaction : transaction -> Merkletree.mtree ref -> bool
    val verify_ledger : Merkletree.mtree -> bool
    val merge_ledgers : Merkletree.mtree ref -> Merkletree.mtree ref -> unit
