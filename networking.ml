module IO = IOHelpers ;;

(* exposes two-way port communication over the network *)
class coinserver =
  object(this)
    val default_port : int = 8332
    val listeners : (string -> unit) list ref = ref []
    (* helper method to initialize the connection over a socket *)
    method initialize_sock inet_addr port =
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sock_addr = Unix.ADDR_INET (inet_addr, port) in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      fd, sock_addr
    (* sends the message s over the internet address *)
    method send_message s inet_addr port =
      try
        let fd, sock_addr = this#initialize_sock inet_addr port in
        Unix.connect fd sock_addr;
        let buf = Bytes.of_string s in
        let _ = Unix.send fd buf 0 (String.length buf) [] in
        true
      with
        | Unix.Unix_error (_, _, _) -> false
    method run_server () : unit =
      (* bind to a local socket *)
      let fd, sock_addr = this#initialize_sock Unix.inet_addr_any default_port in
      Unix.bind fd sock_addr;
      Unix.listen fd 5;
      let rec server_loop () =
        (* pull new data over the socket *)
        let (client_fd, _) = Unix.accept fd in
        let buf = Bytes.create 4096 in
        let len = Unix.recv client_fd buf 0 (String.length buf) [] in
        let request = String.sub buf 0 len in
        List.iter (fun a -> a request) !listeners;
        Unix.close client_fd;
        (* wait for the next connection *)
        server_loop() in
      server_loop ()
    method add_listener f =
      listeners := f :: !listeners
  end

exception EmptyNetwork ;;

(* represents the collection of nodes in the network *)
module OcamlcoinNetwork =
  struct
    (* run a coinserver *)
    let server : coinserver = new coinserver
    (* describe the nodes in our network *)
    class ocamlcoin_node description =
      let i, p = match Str.split (Str.regexp ",") description with
        | [ip; port] ->
            ip, port
        | _ -> raise (Invalid_argument "Unable to parse peer description") in
      object(this)
        val ip = i
        val port = int_of_string p
        method ip () = ip
        method port () = port
        method send_message s =
          server#send_message s (Unix.inet_addr_of_string ip) port
        method active =
          this#send_message "PING"
      end
    (* store the other people in our network *)
    let peers : ocamlcoin_node list ref = ref []
    let synchonize () =
      ()
    (* load the peers we are aware of *)
    let load_peers ?(peer_file : string = "peers.txt") () =
      let loaded_peers = List.map
        (fun ip_string -> new ocamlcoin_node ip_string)
        (IO.page_lines peer_file) in
      match List.filter (fun n -> n#active) loaded_peers with
      | _hd :: _tl as active_peers ->
          peers := active_peers;
          synchonize ()
      | [] ->
          raise EmptyNetwork
    let attach_broadcast_listener = server#add_listener
    let broadcast_over_network d =
      List.iter
        (fun n -> let _ = n#send_message (Yojson.Basic.to_string d) in ()) !peers
    let run () =
      (* run the server on an asynchronous thread *)
      let _ = Thread.create server#run_server () in
      load_peers ()
  end