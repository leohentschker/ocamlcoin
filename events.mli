open Networking
open Mining
open Payments

val event_to_json : network_event -> Yojson.Basic.json
val json_to_event : Yojson.Basic.json -> network_event
