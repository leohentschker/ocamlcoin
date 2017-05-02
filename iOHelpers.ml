(* helper function to get a sublist *)
let rec sublist (lst : 'a list) (a : int) (b : int) : 'a list =
  if b < a then []
  else match lst with
       | [] -> []
       | h :: t ->
          (match (a, b) with
           | (0, 0) -> [h]
           | (0, _) -> [h] @ sublist t a (b - 1)
           | (_, _) -> sublist t (a - 1) (b - 1))

(* Drawn from ps5/http_services *)
let page_lines (page : string) : string list =

  (* read in all the lines from a file and concatenate them into a big
     string *)
  let rec input_lines (inchan : in_channel) (lines: string list)
    : string list =
    try
      input_lines inchan ((input_line inchan) :: lines)
    with End_of_file -> List.rev lines in

  input_lines (open_in page) []

(* following https://ocaml.org/learn/tutorials/file_manipulation.html *)
let write_file (s : string) (fname : string) : unit =
  let oc = open_out fname in
  Printf.fprintf oc "%s" s;
  close_out oc

let write_json (json : Yojson.Basic.json) (fname : string) : unit =
  write_file (Yojson.Basic.to_string json) fname

(* following http://www.rosettacode.org/wiki/Execute_a_system_command#OCaml *)
let syscall (s : string) : string =
  let ic, oc = Unix.open_process s in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)
