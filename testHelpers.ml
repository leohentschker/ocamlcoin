(* mark whether tests are running so we know if we should network locally *)
let c_TESTS_RUNNING = ref false

let c_TEST_ITERATIONS = 10 

let run_tests (test_func : unit -> unit) : unit =
  for _ = 1 to c_TEST_ITERATIONS do
    test_func ()
  done
