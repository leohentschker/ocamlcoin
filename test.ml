module ThreadTester =
  struct
    let listeners = ref []
    let add_listener (f : unit -> unit) = listeners := f :: !listeners
    let rec run_async () : unit =
      Unix.sleep 1;
      List.iter (fun f -> f ()) !listeners ;
      run_async ()

    let run () =
      Thread.create run_async ();
      add_listener (fun () -> print_endline "Listener1");
      add_listener (fun () -> print_endline "Listener2");
      Unix.sleep 10;
  end

let _ = ThreadTester.run ();;