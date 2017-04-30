open Mining
open Payments

type network_event =
  | NewTransaction of transaction
  | SolvedBlock of (block * nonce)
val event_to_string : network_event -> string
val string_to_event : string -> network_event
