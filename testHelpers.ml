let c_TEST_ITERATIONS = 10

let random_string () =
  string_of_int (Random.int 10000)

let run_tests (test_func : unit -> unit) : unit =
  for _ = 1 to c_TEST_ITERATIONS do
    test_func ()
  done

let generate_list (generator : unit -> 'a) : 'a list =
  let mlist = ref [] in
  let i = Random.int 30 in
  for _ = 0 to i do
    let anotha_one = generator () in
    mlist := (!mlist) @ [anotha_one]
  done;
  !mlist
