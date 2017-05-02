open Payments
open Crypto
open Crypto.Signature
open Order

module type DICT =
  sig
    type key
    type value
    type dict
    val empty : dict
    val compare : value -> value -> ordering
    val find : dict -> key -> value option
    val add : dict -> key -> value -> dict
  end

module MakeWallet : (DICT with type key = pub_key and type value = float) =
  struct
    type key = pub_key
    type value = float
    type dict =
      Empty | Tree of dict * key * value * dict

    let empty = Empty

    let compare key1 key2 =
      let s1, s2 = pub_to_string key1, pub_to_string key2 in
        let i = String.compare s1 s2 in
        if i = 0 then E else if i < 0 then G else L

    let find d k =
      match d with
      | Empty -> None
      | Tree (d1, k', v', d2) ->
          match compare k k' with
          | E -> Some v'
          | L -> find d1 k
          | G -> find d2 k

    let rec insert d k v =
      match d with
        | Empty -> Tree (Empty, (k,v), Empty)
        | Tree (dl, ((k1, _v1) as kv), dr) ->
           (match D.compare k k1 with
            | Equal -> Tree (dl, (k,v), dr)
            | Less -> Tree (insert dl k v, kv, dr)
            | Greater -> Tree (dl, kv, insert dr k v)) ;;
  end



