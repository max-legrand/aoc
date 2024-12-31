open Core
open Utils

let () = Spice.info "2024 Day 25"

(* let input = "bin/2024/day25/data/test.txt" *)
let input = "bin/2024/day25/data/puzzle.txt"

let process_keys_and_locks input =
  let items = Str.split (Str.regexp_string "\n\n") input in
  let rec helper keys locks items =
    match items with
    | [] -> keys, locks
    | item :: rest ->
      let lines = item |> String.split_lines in
      if String.equal (List.nth_exn lines 0) "#####"
      then helper keys (item :: locks) rest
      else helper (item :: keys) locks rest
  in
  helper [] [] items
;;

let transpose_matrix matrix =
  let transposed =
    Array.make_matrix ~dimx:(Array.length matrix) ~dimy:(Array.length matrix.(0)) '.'
  in
  Array.iteri matrix ~f:(fun i row ->
    Array.iteri row ~f:(fun j cell -> transposed.(j).(i) <- cell));
  transposed
;;

let convert_to_height item_string =
  (* Convert the string to a matrix *)
  let array = Array.make_matrix ~dimx:5 ~dimy:5 '.' in
  let item = String.split_lines item_string in
  List.iteri item ~f:(fun i line ->
    if i = 0 || i = 6
    then ()
    else String.iteri line ~f:(fun j char -> array.(i - 1).(j) <- char));
  let transposed = transpose_matrix array in
  transposed |> Array.map ~f:(fun row -> Array.count row ~f:(fun x -> Char.equal x '#'))
;;

let check_key_lock lock key =
  let rec helper idx =
    if idx = Array.length lock
    then true
    else if lock.(idx) + key.(idx) <= 5
    then helper (idx + 1)
    else false
  in
  helper 0
;;

let find_combinations locks keys =
  locks
  |> List.fold ~init:0 ~f:(fun acc lock ->
    keys
    |> List.fold ~init:acc ~f:(fun acc key ->
      Spice.debugf
        "Checking lock:%s with key:%s"
        (print_array_string (lock |> Array.map ~f:(fun x -> Int.to_string x)))
        (print_array_string (key |> Array.map ~f:(fun x -> Int.to_string x)));
      let result = check_key_lock lock key in
      Spice.debugf "Result: %b" result;
      if result then acc + 1 else acc))
;;

let part1 () =
  Spice.set_log_level Spice.DEBUG;
  let data = read_file_single input in
  let keys, locks = process_keys_and_locks data in
  Spice.infof "%d keys, %d locks" (List.length keys) (List.length locks);
  let keys = keys |> List.map ~f:convert_to_height in
  let locks = locks |> List.map ~f:convert_to_height in
  locks
  |> List.iter ~f:(fun lock ->
    print_array (lock |> Array.map ~f:(fun x -> Int.to_string x)));
  Spice.debugf "--------";
  keys
  |> List.iter ~f:(fun key ->
    print_array (key |> Array.map ~f:(fun x -> Int.to_string x)));
  let result = find_combinations locks keys in
  Spice.infof "Result: %d" result;
  ()
;;

part1 ()
