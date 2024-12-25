open Core

let _read_file_inner (filename : string) : string list =
  let ic = In_channel.create filename in
  let rec read_lines acc =
    match In_channel.input_line ic with
    | Some line -> read_lines (line :: acc)
    | None -> acc
    | exception End_of_file ->
      In_channel.close ic;
      List.rev acc
    | exception Sys_error msg ->
      In_channel.close ic;
      failwith msg
  in
  read_lines []
;;

(** Read a file and return the lines as a list of strings*)
let read_file (filename : string) : string list = List.rev (_read_file_inner filename)

let print_str_lines (lines : string list) : unit = List.iter ~f:print_endline lines
let print_int_lines (lines : int list) : unit = List.iter ~f:(Printf.printf "%d\n") lines

let read_file_single (filename : string) : string =
  _read_file_inner filename |> String.concat ~sep:"\n"
;;

module PrioQ = struct
  type 'a t = ('a * int) list ref

  let create () = ref []

  let add queue item prio =
    queue := (item, prio) :: !queue;
    queue := List.sort ~compare:(fun (_, p1) (_, p2) -> Int.compare p1 p2) !queue
  ;;

  let pop queue =
    match !queue with
    | [] -> None
    | (item, prio) :: rest ->
      queue := rest;
      Some (item, prio)
  ;;

  let is_empty inst = List.is_empty !inst
end

let print_map map =
  map
  |> Array.iter ~f:(fun row ->
    let row_string =
      row |> Array.fold ~init:"" ~f:(fun acc x -> acc ^ Char.to_string x)
    in
    Spice.debugf "%s" row_string)
;;

let print_array_string array =
  "[" ^ (array |> Array.to_list |> String.concat ~sep:",") ^ "]"
;;

let print_array array = Spice.debugf "%s" (array |> print_array_string)
let print_list_string lst = "[" ^ (lst |> String.concat ~sep:",") ^ "]"
let print_list lst = Spice.debugf "%s" (lst |> print_list_string)
