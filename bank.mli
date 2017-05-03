open Crypto.Keychain
open Crypto.Signature
open Payments.Transaction

module Bank :
  sig
    type ledger
    val masterkey : pub_key
    val ledger : ledger
    val query : string -> ledger -> transaction list
    val add_transaction : transaction -> ledger -> unit
    val verify_transaction : transaction -> ledger -> bool
    val verify_ledger : ledger -> bool
    val merge_ledgers : ledger -> ledger -> unit
  end
