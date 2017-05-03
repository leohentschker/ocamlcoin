open Sexplib
open Crypto
open Crypto.Keychain
open Payments
open Payments.Transaction
open Merkletree
open Mining
module IO = IOHelpers
module Y = Yojson


let c_MASTERKEY_FILE_NAME = "masterkey.json"
let c_LEDGER_FILE_NAME = "ledger.json"
exception MissingMasterkey

let masterpriv_test, masterpub_test = generate_keypair ()

module Bank =
  struct
    module MT = MakeMerkle (TransactionSerializable) (SHA256)
    let masterpub =
      try
        let json = Yojson.Basic.from_file c_MASTERKEY_FILE_NAME in
        let open Yojson.Basic.Util in
        string_to_pub (json |> member c_PUB_JSON_KEY |> to_string)
      with Sys_error _ ->
        raise MissingMasterkey

    let get_transactions (l : MT.mtree ref) = MT.children !l

    let export_ledger (l : MT.mtree ref) : unit =
      match get_transactions l with
      | _h :: _t as tlist ->
        IO.write_json
          (`List(List.map (fun t -> t#to_json) tlist)) c_LEDGER_FILE_NAME
      | [] -> ()

    type mtree = MT.mtree

    type ledger = MT.mtree ref
    let book =
      let previous_transactions = try
        match Y.Basic.from_file c_LEDGER_FILE_NAME with
        | `List json_list -> List.map json_to_transaction json_list
        | _ -> failwith "Unexpected json format"
        with Sys_error _ -> [] in
      ref (MT.build_tree previous_transactions)

    let empty = MT.empty

    let query (p : pub_key) (m : ledger) : transaction list =
      (MT.queryid p !m) @ (MT.queryhash (pub_to_string p) !m)

    let verify_transaction (t : transaction) (l: ledger) : bool =
      let id1, id2, amount, timestamp = t#originator, t#target, t#amount, t#timestamp in
      let eltlst = MT.queryid id1 !l in
      let timedlst = List.filter (fun x -> x#timestamp < timestamp) eltlst in
      let total_amount = List.fold_left
        (fun acc x -> if x#originator = id1 then acc -. x#amount
                      else acc +. x#amount) 0. timedlst in
      authenticate_transaction t
      && ((not (eltlst = [])
           && total_amount >= amount
           && amount > 0.
           && timestamp > 0.
           && Mining.Miner.verify t#to_string t#solution)
          ||
         (id1 = masterpub || id1 = masterpub_test))

    let add_transaction (t : transaction) (l : ledger) : unit =
      if verify_transaction t l then
        l := (MT.add_element t !l)
      else ()

    let verify_ledger (t : ledger) : bool =
      let rec verify (t : ledger) (n : int) : bool =
        if n <= 0 then true
        else
          let tlist = MT.children !t in
          let slist = IO.sublist tlist 0 (n - 1) in
          let tn = List.nth tlist n in
          let subledger = ref (MT.build_tree slist) in
          verify_transaction tn subledger && (verify subledger (n - 1)) in
      verify t (List.length (MT.children !t) - 1)

    let generate_transaction () =
      let _, target = generate_keypair () in
      let amount = Random.float 1000. in
      let timestamp = Random.float 100000. in
      create_transaction masterpub_test target amount timestamp masterpriv_test

    let generate_transaction_list () =
      TestHelpers.generate_list generate_transaction (Random.int 30)

    let test_add_transaction () =
      let ledger = ref empty in
      let new_transaction = generate_transaction () in
      let transaction_list = generate_transaction_list () in
      add_transaction new_transaction ledger;
      List.iter (fun t -> add_transaction t ledger) transaction_list;
      List.iter (fun t -> assert (List.mem t (MT.children !ledger))) transaction_list

    let test_query () =
      let ledger = ref empty in
      let transaction_list = generate_transaction_list () in
      let other_transaction = generate_transaction () in
      List.iter (fun t -> add_transaction t ledger) transaction_list;
      List.iter (fun t -> assert (List.memq t (query t#originator ledger)))
                transaction_list;
      assert (not (List.memq other_transaction
                             (query other_transaction#originator ledger)))

    (* More tests here *)
    let test_verify_transaction () =
      let ledger = ref empty in
      let priv1, pub1 = generate_keypair () in
      let priv2, pub2 = generate_keypair () in
      let transaction1 = create_transaction masterpub_test pub1 100. 100.
                                            masterpriv_test in
      let transaction2 = create_transaction masterpub_test pub2 100. 150.
                                            masterpriv_test in
      let good_transaction1 = create_transaction pub1 pub2 75. 200. priv1 in
      let good_transaction2 = create_transaction pub1 pub2 100. 200. priv1 in
      let bad_transaction1 = create_transaction pub1 pub2 150. 250. priv1 in
      let bad_transaction2 = create_transaction pub2 pub1 300. 300. priv2 in
      let bad_transaction3 = create_transaction pub2 pub1 100. ~-.(250.) priv2 in
      let bad_transaction4 = create_transaction pub2 pub1 ~-.(100.) 250. priv1 in
      let valid_list = generate_transaction_list () in
      Miner.leading_zeros := 0;
      List.iter (fun t -> add_transaction t ledger) valid_list;
      List.iter (fun t -> assert (verify_transaction t ledger)) valid_list;
      add_transaction transaction1 ledger;
      add_transaction transaction2 ledger;
      assert (verify_transaction good_transaction1 ledger);
      assert (verify_transaction good_transaction2 ledger);
      assert (not (verify_transaction bad_transaction1 ledger));
      assert (not (verify_transaction bad_transaction2 ledger));
      assert (not (verify_transaction bad_transaction3 ledger));
      assert (not (verify_transaction bad_transaction4 ledger));
      Miner.leading_zeros := 2

    let test_verify_ledger () =
      let good_ledger = ref empty in
      let transaction_list = generate_transaction_list () in
      List.iter (fun t -> add_transaction t good_ledger) transaction_list;
      assert (verify_ledger good_ledger)

    let run_tests () =
      TestHelpers.run_tests test_add_transaction;
      TestHelpers.run_tests test_query;
      TestHelpers.run_tests test_verify_transaction;
      TestHelpers.run_tests test_verify_ledger;
      TestHelpers.run_tests test_query;
      print_endline "All tests passed!"
  end
