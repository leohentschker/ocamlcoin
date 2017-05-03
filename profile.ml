module IO = IOHelpers
open Crypto
open Crypto.Keychain
open Networking.OcamlcoinNetwork
module Y = Yojson

let c_PROFILE_FILE_NAME = "profile.json"
let c_STORED_NODES_KEY = "nodes"

module User =
  struct
    open Y.Basic.Util
    let make_profile_json priv pub nodes =
      print_endline "MAKING PROFILE";
      let out = `Assoc[(c_PUB_JSON_KEY, `String (pub_to_string pub));
             (c_STORED_NODES_KEY, `List (List.map (fun n -> n#to_json) nodes));
             (c_PRIV_JSON_KEY, `String (priv_to_string priv))] in
      print_endline "PLZ GOD";
      out
    let profile_json = 
      try Y.Basic.from_file c_PROFILE_FILE_NAME
      with Sys_error _ ->
        print_endline "Generating a User Profile";
        let priv, pub = generate_keypair () in
        let json = make_profile_json priv pub [] in
        IO.write_json json c_PROFILE_FILE_NAME;
        json
    let public_key =
      string_to_pub (profile_json |> member c_PUB_JSON_KEY |> to_string)
    let private_key =
      string_to_priv (profile_json |> member c_PRIV_JSON_KEY |> to_string)
    let stored_nodes =
      print_endline "STORED";
      match profile_json |> member c_STORED_NODES_KEY with
      | `List jsonlist -> List.map json_to_ocamlcoin_node jsonlist
      | _ -> failwith "Unexpected json format for stored nodes"
    let export_nodes (nlist : ocamlcoin_node list) =
      IO.write_json (make_profile_json private_key public_key nlist)
        c_PROFILE_FILE_NAME;
      print_endline "EXPORTED NODES SUCCESS"
  end
