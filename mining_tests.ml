open TestHelpers
open Mining.Miner

let generate_fake_nonce () =
  string_to_nonce (string_of_int (Random.int 1000))
