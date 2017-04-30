let c_TEST_ITERATIONS = 10 

let run_tests (test_func : unit -> unit) : unit =
  for _ = 1 to c_TEST_ITERATIONS do
    test_func ()
  done
