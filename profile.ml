module IO = IOHelpers
open Crypto.Keychain
module Y = Yojson

let c_PROFILE_FILE_NAME = "profile.json"

let c_PUB_JSON_KEY = "public_key"
let c_PRIV_JSON_KEY = "private_key"

module User =
  struct
    open Y.Basic.Util
    let profile_json = 
      try Y.Basic.from_file c_PROFILE_FILE_NAME
      with Sys_error s ->
        print_endline "Generating a User Profile";
        let priv, pub = generate_keypair () in
        let profile_json =
          `Assoc [(c_PUB_JSON_KEY, `String (pub_to_string pub));
                  (c_PRIV_JSON_KEY, `String (priv_to_string priv))] in
        IO.write_file (Y.Basic.to_string profile_json) c_PROFILE_FILE_NAME;
        profile_json
    let public_key =
      string_to_pub (profile_json |> member c_PUB_JSON_KEY |> to_string)
    let private_key =
      string_to_priv (profile_json |> member c_PRIV_JSON_KEY |> to_string)
  end
