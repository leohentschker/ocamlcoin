module type SERIALIZER =
  sig
    type t
    val to_json : t -> Yojson.Basic.json
    val from_json : Yojson.Basic.json -> t
  end