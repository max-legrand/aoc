(** Read a file and return the lines as a list of strings*)
let read_file (filename : string) : string list =
  let ic = open_in filename in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file ->
      close_in ic;
      List.rev acc
    | exception Sys_error msg ->
      close_in ic;
      failwith msg
  in
  read_lines []
;;

let print_lines (lines : string list) : unit = List.iter print_endline lines
