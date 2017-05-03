open Crypto.Keychain
open Crypto.Signature
open Payments.Transaction

module Bank :
  sig
    type mtree
    type ledger = mtree ref
    val masterkey : pub_key
    val empty : mtree
    val book : ledger
    val query : pub_key -> ledger -> transaction list
    val add_transaction : transaction -> ledger -> unit
    val verify_transaction : transaction -> ledger -> bool
    val verify_ledger : ledger -> bool
    val run_tests : unit -> unit
  end
