open Networking ;;
module ON = OcamlcoinNetwork

module Transaction =
  object
    type transaction = {source : ON.ocamlcoin_node; target : ON.ocamlcoin_node;
                        amount : float}
    let make_transaction (source : ocamlcoin_node)
                         (target : ocamlcoin_node)
                         (amount : float)
                         : transaction =
      {source; target; amount}
    let serialize_transaction (t : transaction) : string =
      "SERIALIZED TRANSACTION!"
  end