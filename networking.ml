module IO = IOHelpers ;;

module type COINSERVER =
  sig
    val coin_port : int
    val send_message : string -> Unix.inet_addr -> unit
    val attach_listener : (string -> unit) -> unit
    val run_server : unit -> unit
  end

module CoinServer : COINSERVER =
  struct
    let coin_port = 8332
    let test_ip = "10.252.10.214"
    let listeners = ref []
    let initialize_sock inet_addr =
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sock_addr = Unix.ADDR_INET (inet_addr, coin_port) in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      fd, sock_addr
    let send_message s ip =
      let fd, sock_addr = initialize_sock ip in
      let _ = Unix.connect fd sock_addr in
      let buf = Bytes.of_string s in
      let _ = Unix.send fd buf 0 (String.length buf) [] in
      ()
    let run_server () =
      let fd, sock_addr = initialize_sock Unix.inet_addr_any in
      Unix.bind fd sock_addr;
      Unix.listen fd 5;
      let rec server_loop () =
        let (client_fd, _) = Unix.accept fd in
        let buf = Bytes.create 4096 in
        let len = Unix.recv client_fd buf 0 (String.length buf) [] in
        let request = String.sub buf 0 len in
        List.iter (fun a -> a request) !listeners;
        Unix.close client_fd;
        server_loop() in
      server_loop ()
    let attach_listener f =
      listeners := f :: !listeners
  end

class type network_node =
  object
    method ip : unit -> string
    method port : unit -> string
    method broadcast_message : string -> bool
    method request_message : string -> bool
    method active : bool
  end

class ocamlcoin_node description : network_node =
  let i, p = match Str.split (Str.regexp ",") description with
    | [ip; port] ->
        ip, port
    | _ -> raise (Invalid_argument "Unable to parse peer file") in
  object
    val ip = i
    val port = p
    method ip () = ip
    method port () = port
    method broadcast_message s = true
    method request_message s = true
    method active = false
  end

class type p2p_network =
  object
    method load_peers : ?peer_file:string -> unit -> unit
    method synchonize : unit
    method broadcast_network : network_node -> unit
  end

(* store nodes as pairs of (ip address, port) *)
class ocamlcoin_network : p2p_network =
  object(this)
    (* store the other people in our network *)
    val mutable peers : network_node list = []
    (* load the peers we are aware of *)
    method load_peers ?(peer_file : string = "peers.txt") () =
      let loaded_peers = List.map
        (fun ip_string -> new ocamlcoin_node ip_string) (IO.page_lines peer_file) in
      let active_peers = List.filter (fun n -> n#active) loaded_peers in
      if active_peers = [] then
        this#initialize_peers
      else
        peers <- active_peers;
        this#synchonize
    method synchonize =
      print_endline "synchonize"
    method broadcast_network (n : network_node) =
      print_endline "broadcast"
    method private initialize_peers =
      print_endline "INITIALIZE PEERS"
  end