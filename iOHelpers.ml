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