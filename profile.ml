module IO = IOHelpers
open Crypto
open Crypto.Keychain
open Networking
open Networking.OcamlcoinNetwork
module Y = Yojson

let c_PROFILE_FILE_NAME = "files/profile.json"
let c_STORED_NODES_KEY = "nodes"

module User =
  struct
    open Y.Basic.Util
    let make_profile_json priv pub nodes =
      `Assoc[(c_PUB_JSON_KEY, `String (pub_to_string pub));
             (c_STORED_NODES_KEY, `List (List.map (fun n -> n#to_json) nodes));
             (c_PRIV_JSON_KEY, `String (priv_to_string priv))]
    let profile_json = 
      try Y.Basic.from_file c_PROFILE_FILE_NAME
      with
      | Sys_error _ | Yojson.Json_error(_) ->
        print_endline "Unable to load profile";
        let priv, pub = generate_keypair () in
        let json = make_profile_json priv pub [] in
        IO.write_json json c_PROFILE_FILE_NAME;
        json
    let public_key =
      string_to_pub (profile_json |> member c_PUB_JSON_KEY |> to_string)
    let private_key =
      string_to_priv (profile_json |> member c_PRIV_JSON_KEY |> to_string)
    let personal_node = new ocamlcoin_node (Networking.get_private_ip ())
      c_DEFAULT_COIN_PORT public_key
    let stored_nodes =
      let stored = match profile_json |> member c_STORED_NODES_KEY with
      | `List jsonlist -> List.map json_to_ocamlcoin_node jsonlist
      | _ -> failwith "Unexpected json format for stored nodes" in
      match stored with
      | _h :: _t -> stored
      | [] -> [personal_node; default_node]
    let export_nodes (nlist : ocamlcoin_node list) =
      IO.write_json (make_profile_json private_key public_key nlist)
        c_PROFILE_FILE_NAME
  end
