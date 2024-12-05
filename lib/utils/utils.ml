let _read_file_inner (filename : string) : string list =
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

(** Read a file and return the lines as a list of strings*)
let read_file (filename : string) () : string list = _read_file_inner filename

let print_str_lines (lines : string list) : unit = List.iter print_endline lines
let print_int_lines (lines : int list) : unit = List.iter (Printf.printf "%d\n") lines

let read_file_single (filename : string) : string =
  _read_file_inner filename |> String.concat "\n"
;;
