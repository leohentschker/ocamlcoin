open Crypto.Keychain

module User :
  sig
    val private_key : priv_key
    val public_key : pub_key
  end
