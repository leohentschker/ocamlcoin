open Crypto.Keychain
open Crypto.Signature
open Payments.Transaction

module Bank :
  sig
    type mtree
    type ledger = mtree ref
    val masterpub : pub_key
    val book : ledger
    val query : pub_key -> ledger -> transaction list
    val add_transaction : transaction -> ledger -> unit
    val get_balance : pub_key -> ledger -> float
    val verify_transaction : transaction -> ledger -> bool
    val verify_ledger : ledger -> bool
    val run_tests : unit -> unit
    val export_ledger : ledger -> unit
    val get_transactions : ledger -> transaction list
  end
