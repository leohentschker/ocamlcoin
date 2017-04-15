module IO = IOHelpers ;;

module type COINSERVER =
  sig
    val default_port : int
    val send_message : string -> Unix.inet_addr -> int -> bool
    val attach_listener : (string -> unit) -> unit
    val run_server : unit -> unit
  end

module CoinServer : COINSERVER =
  struct
    let default_port = 8332
    let listeners = ref []
    (* helper method to initialize the connection over a socket *)
    let initialize_sock inet_addr port =
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sock_addr = Unix.ADDR_INET (inet_addr, port) in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      fd, sock_addr
    (* sends the message s over the internet address *)
    let send_message s inet_addr port =
      try
        let fd, sock_addr = initialize_sock inet_addr port in
        let _ = Unix.connect fd sock_addr in
        let buf = Bytes.of_string s in
        let _ = Unix.send fd buf 0 (String.length buf) [] in
        true
      with
        | Unix.Unix_error (_, _, _) ->
            false
    let run_server () =
      (* bind to a local socket *)
      let fd, sock_addr = initialize_sock Unix.inet_addr_any default_port in
      Unix.bind fd sock_addr;
      Unix.listen fd 5;
      let rec server_loop () =
        (* pull new data over the socket *)
        let (client_fd, _) = Unix.accept fd in
        let buf = Bytes.create 4096 in
        let len = Unix.recv client_fd buf 0 (String.length buf) [] in
        let request = String.sub buf 0 len in
        (* call the listeners on data we receive *)
        List.iter (fun a -> a request) !listeners;
        Unix.close client_fd;
        (* wait for the next connection *)
        server_loop() in
      server_loop ()
    let attach_listener f =
      listeners := f :: !listeners
  end

class type network_node =
  object
    method ip : unit -> string
    method port : unit -> int
    method send_message : string -> bool
    method active : bool
  end

class ocamlcoin_node description : network_node =
  (* parse a string to extract ip information *)
  let i, p = match Str.split (Str.regexp ",") description with
    | [ip; port] ->
        ip, port
    | _ -> raise (Invalid_argument "Unable to parse peer file") in
  object(this)
    val ip = i
    val port = int_of_string p
    method ip () = ip
    method port () = port
    method send_message s =
      CoinServer.send_message s (Unix.inet_addr_of_string ip) port
    method active =
      this#send_message "ping"
  end

class type p2p_network =
  object
    method load_peers : ?peer_file:string -> unit -> unit
    method synchonize : unit
    method broadcast_network : network_node -> unit
  end

class ocamlcoin_network : p2p_network =
  object(this)
    (* store the other people in our network *)
    val mutable peers : network_node list = []
    (* load the peers we are aware of *)
    method load_peers ?(peer_file : string = "peers.txt") () =
      let loaded_peers = List.map
        (fun ip_string -> new ocamlcoin_node ip_string) (IO.page_lines peer_file) in
      (* filter out unactive nodes *)
      let active_peers = List.filter (fun n -> n#active) loaded_peers in
      let _ = if active_peers = [] then this#initialize_peers
        else peers <- active_peers in
      this#synchonize
    method synchonize =
      ()
    method broadcast_network (n : network_node) =
      ()
    method initialize_peers =
      ()
  end

let network = new ocamlcoin_network ;;
network#load_peers () ;;
(* let _ = CoinServer.run_server () ;; *)