open Networking

let run_network () =
  OcamlcoinNetwork.run ();
  Unix.sleep 1000 ;;


let _ = run_network () ;;
