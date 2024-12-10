open Base

let () = Spice.info "2024 Day 9"

(* let input = "bin/2024/day9/data/test.txt" *)
let input = "bin/2024/day9/data/puzzle.txt"

(*
   Part 1 has us finding the checksum of some blocks of files.
*)
let parse_input ~(input : string) =
  let total =
    String.fold input ~init:0 ~f:(fun acc c -> acc + (Char.to_int c - Char.to_int '0'))
  in
  let array = Array.create ~len:total (-1) in
  let _ =
    String.fold input ~init:(0, 0, true) ~f:(fun (array_idx, id, is_file) c ->
      let num = Char.to_int c - Char.to_int '0' in
      if is_file
      then
        for i = 0 to num - 1 do
          array.(array_idx + i) <- id
        done;
      array_idx + num, (if is_file then id + 1 else id), not is_file)
  in
  array
;;

let reorder_and_checksum ~(arr : int array) =
  let len = Array.length arr in
  let left = ref 0 in
  let right = ref (len - 1) in
  let checksum = ref 0 in
  while !left < !right do
    while !left < len && arr.(!left) <> -1 do
      checksum := !checksum + (arr.(!left) * !left);
      Int.incr left
    done;
    while !right >= 0 && arr.(!right) = -1 do
      Int.decr right
    done;
    Spice.debugf "left: %d right: %d" !left !right;
    if !left < !right
    then (
      arr.(!left) <- arr.(!right);
      checksum := !checksum + (arr.(!left) * !left);
      arr.(!right) <- -1;
      Int.incr left)
  done;
  !checksum
;;

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let line = Utils.read_file_single input in
  Spice.debugf "line: %s" line;
  let array = parse_input ~input:line in
  let str =
    array
    |> Array.fold ~init:"" ~f:(fun acc x ->
      match x with
      | -1 -> acc ^ "."
      | _ -> acc ^ Printf.sprintf "%d" x)
  in
  Spice.debugf "str: %s" str;
  let checksum = reorder_and_checksum ~arr:array in
  Spice.infof "checksum: %d" checksum;
  ()
;;

part1 ()

let get_sequence ~(arr : int array) =
  let len = Array.length arr in
  let filled = ref [] in
  let empty = ref [] in
  let rec find_sequence pos =
    if pos >= len
    then ()
    else (
      let curr = arr.(pos) in
      if curr = -1
      then (
        let seq_start = pos in
        let rec count_dots p =
          if p >= len || arr.(p) <> -1 then p - seq_start else count_dots (p + 1)
        in
        let dot_len = count_dots pos in
        empty := (seq_start, dot_len) :: !empty;
        find_sequence (pos + dot_len))
      else (
        let seq_start = pos in
        let value = curr in
        let rec count_nums p =
          if p >= len || arr.(p) <> value then p - seq_start else count_nums (p + 1)
        in
        let num_len = count_nums pos in
        filled := (value, seq_start, num_len) :: !filled;
        find_sequence (pos + num_len)))
  in
  find_sequence 0;
  List.rev !filled, List.rev !empty
;;

let reorder_and_checksum_full ~(arr : int array) =
  let filled, empty = get_sequence ~arr in
  Spice.debug "Empty sets:";
  let empty = ref (empty |> List.to_array) in
  let rec add_checksum acc value start len =
    match len with
    | 0 -> acc
    | _ -> add_checksum (acc + (value * start)) value (start + 1) (len - 1)
  in
  !empty
  |> Array.iter ~f:(fun (start, len) -> Spice.debugf "Start: %d Length: %d" start len);
  filled
  |> List.rev
  |> List.fold ~init:0 ~f:(fun acc (value, start, len) ->
    Spice.debugf "Value: %d Start: %d Length: %d" value start len;
    let counter = ref 0 in
    let array_len = Array.length !empty in
    let found = ref false in
    while !counter < array_len && not !found do
      let space_start, space_len = !empty.(!counter) in
      if space_start < start && space_len >= len then found := true else Int.incr counter
    done;
    if !found
    then (
      let space_start, space_len = !empty.(!counter) in
      Spice.debugf "Space start: %d Space len: %d" space_start space_len;
      if space_len > len
      then !empty.(!counter) <- space_start + len, space_len - len
      else empty := Array.filteri !empty ~f:(fun idx _ -> idx <> !counter);
      acc + add_checksum 0 value space_start len)
    else acc + add_checksum 0 value start len)
;;

(* For part 2 we only move files if they can fit in the full space *)
let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let line = Utils.read_file_single input in
  Spice.debugf "line: %s" line;
  let array = parse_input ~input:line in
  let str =
    array
    |> Array.fold ~init:"" ~f:(fun acc x ->
      match x with
      | -1 -> acc ^ "."
      | _ -> acc ^ Printf.sprintf "%d" x)
  in
  Spice.debugf "str: %s" str;
  let checksum = reorder_and_checksum_full ~arr:array in
  Spice.infof "checksum: %d" checksum
;;

part2 ()
