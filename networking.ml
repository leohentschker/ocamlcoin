module IO = IOHelpers ;;
open Payments ;;
open Yojson ;;

let c_IP_JSON_KEY = "ip"
let c_PORT_JSON_KEY = "port"
let c_DATA_JSON_KEY = "message_data"
let c_DEFAULT_COIN_PORT = 8332

let c_USE_LOCAL_NETWORK = ref true

let is_valid_ip ip_str =
  Str.string_match (Str.regexp "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)") ip_str 0 ;;

(* attempt to determine a private ip *)
let rec get_private_ip () =
  (* uses regex to check if the ip we entered was valid *)
  let mac_output = IO.syscall "ifconfig en0 | grep 'inet ' | awk '{print $2}'" in
  let ubuntu_output = IO.syscall "ifconfig -a | grep 'inet addr' | awk {'print $2'} | sed -e 's/^addr://' | sed -n 2p" in
  if is_valid_ip mac_output then
    mac_output
  else if is_valid_ip ubuntu_output then
    ubuntu_output
  else
    let _ = print_string "Enter private ip: " in
    let manually_entered_ip = read_line () in
    print_endline "";
    if is_valid_ip manually_entered_ip then
      manually_entered_ip
    else
      let _ = Printf.printf "Invalid IP: %s" manually_entered_ip in
      get_private_ip ()

(* exposes two-way port communication over the network *)
class coinserver =
  object(this)
    (* listeners that get called on receiving data over the network *)
    val listeners : (string -> unit) list ref = ref []
    val ip = get_private_ip ()
    method ip = ip
    (* helper method to initialize the connection over a socket *)
    method initialize_sock inet_addr port =
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sock_addr = Unix.ADDR_INET (inet_addr, port) in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      fd, sock_addr
    (* handle an incoming message over the network *)
    method handle_message s = List.iter (fun a -> a s) !listeners
    (* sends the message s over the internet address *)
    method send_message s inet_addr port =
      if !c_USE_LOCAL_NETWORK then
        let _ = print_endline "USING LOCAL NETWORK" in
        let _ = this#handle_message s in
        true
      else try
        let fd, sock_addr = this#initialize_sock inet_addr port in
        Unix.connect fd sock_addr;
        let buf = Bytes.of_string s in
        let _ = Unix.send fd buf 0 (String.length buf) [] in
        true
      with
        | Unix.Unix_error (_, _, _) -> false
    method add_listener f =
      listeners := f :: !listeners
    method run_server () : unit =
      (* bind to a local socket *)
      let fd, sock_addr = this#initialize_sock Unix.inet_addr_any c_DEFAULT_COIN_PORT in
      Unix.bind fd sock_addr;
      Unix.listen fd 5;
      let rec server_loop () =
        (* pull new data over the socket *)
        let (client_fd, _) = Unix.accept fd in
        let buf = Bytes.create 4096 in
        let len = Unix.recv client_fd buf 0 (String.length buf) [] in
        let request = String.sub buf 0 len in
        this#handle_message request;
        Unix.close client_fd;
        (* wait for the next connection *)
        server_loop() in
      server_loop ()
    method run_server_async () : unit =
      let _ = Thread.create this#run_server () in
      ()
  end

exception EmptyNetwork ;;

(* represents the collection of nodes in the network *)
module OcamlcoinNetwork =
  struct
    (* run a coinserver *)
    let server : coinserver = new coinserver
    (* describe the nodes in our network *)
    type peer = string
    class ocamlcoin_node ip_addr port_number =
      object(this)
        val ip = String.trim ip_addr
        val port = port_number
        method ip = ip
        method port = port
        method serialize = this#ip ^ "," ^ (string_of_int this#port)
        method send_message s =
          server#send_message s (Unix.inet_addr_of_string ip) port
        method to_json : Basic.json =
          `Assoc [(c_IP_JSON_KEY, `String ip); (c_PORT_JSON_KEY, `Int port)]
      end
    let json_to_ocamlcoin_node json =
      let open Basic.Util in
      new ocamlcoin_node
        (json |> member c_IP_JSON_KEY |> to_string)
        (json |> member c_PORT_JSON_KEY |> to_int)
    let attach_broadcast_listener f =
      server#add_listener
        (fun s ->
          let open Yojson.Basic.Util in
          let json = Yojson.Basic.from_string s in
          f (json |> member c_DATA_JSON_KEY)
            (new ocamlcoin_node (json |> member c_IP_JSON_KEY |> to_string)
              (json |> member c_PORT_JSON_KEY |> to_int)))
    let broadcast_to_node (json_msg : Yojson.Basic.json)
                           (node : ocamlcoin_node)  =
      (* attach the port and the ip to the json *)
      let _ = node#send_message(Yojson.Basic.to_string
        (`Assoc [(c_DATA_JSON_KEY, json_msg);
                 (c_PORT_JSON_KEY, `Int c_DEFAULT_COIN_PORT);
                 (c_IP_JSON_KEY, `String server#ip)])) in
      ()
    let run () =
      (* run the server on an asynchronous thread *)
      server#run_server_async ();
  end

type network_event =
  | PingDiscovery
  | NewTransaction of transaction
  | SolvedBlock of (block * Mining.Miner.nonce)
  | BroadcastNodes of (OcamlcoinNetwork.ocamlcoin_node list)
