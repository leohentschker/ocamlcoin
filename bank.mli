open Crypto.Keychain
open Crypto.Signature
open Payments.Transaction

module Bank :
  sig
    type mtree
    type ledger = mtree ref
    val masterkey : pub_key
    val empty : ledger
    val book : ledger
    val query : pub_key -> ledger -> transaction list
    val add_transaction : transaction -> ledger -> unit
    val verify_transaction : transaction -> ledger -> bool
    val verify_ledger : ledger -> bool
    val merge_ledgers : ledger -> ledger -> unit
    val run_tests : unit -> unit
  end
