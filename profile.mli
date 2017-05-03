open Networking.OcamlcoinNetwork
open Crypto.Keychain

module User :
  sig
    val private_key : priv_key
    val public_key : pub_key
    val stored_nodes : ocamlcoin_node list
    val export_nodes : ocamlcoin_node list -> unit
    val personal_node : ocamlcoin_node
  end
