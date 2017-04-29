open Mining
open Payments


type network_event =
  | NewTransaction of transaction
  | SolvedBlock of (transaction * nonce)


let event_to_string (e : network_event) =
  match e with
  | NewTransaction t -> "ASD"
    
  | SolvedBlock (t, n) -> "SOLVED BLOCK"


let string_of_event (s : string) = ""
