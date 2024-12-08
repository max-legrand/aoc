open Base

let ( let* ) x f = Result.bind ~f x
let () = Spice.info "2024 Day 6"

(* let input = "bin/2024/day6/data/test.txt" *)
let input = "bin/2024/day6/data/puzzle.txt"

(*
   Part 1 has us solving an ice puzzle.
   If you've played a Pokemon game before this is similar to any of the ice puzzles
   in those games.

   We want to mark off how many tiles we hit by always turning right when we come across an
   obstacle.
*)
type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving equal, sexp, compare, hash]

let direction_to_coordinate = function
  | Up -> -1, 0
  | Down -> 1, 0
  | Left -> 0, -1
  | Right -> 0, 1
;;

let next_direction = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
;;

let create_matrix data =
  let process_row row = row |> String.to_list in
  let rec parse_rows acc rows =
    match rows with
    | [] -> acc
    | row :: rows -> parse_rows (process_row row :: acc) rows
  in
  List.rev (parse_rows [] data)
;;

let find_starting_point (matrix : char list list) =
  let rec find_in_cols (cols : char list) col_idx =
    match cols with
    | [] -> None
    | col :: cols ->
      if Char.equal col '^' then Some col_idx else find_in_cols cols (col_idx + 1)
  in
  let rec find_in_matrix rows row_idx =
    match rows with
    | [] -> Error "No starting point found"
    | row :: rows ->
      (* Convert the char list to a string *)
      Spice.debugf "row %d=%s" row_idx (row |> String.of_char_list);
      (match find_in_cols row 0 with
       | Some col_idx -> Ok (row_idx, col_idx)
       | None -> find_in_matrix rows (row_idx + 1))
  in
  find_in_matrix matrix 0
;;

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash, equal]
  end

  include T
  include Comparator.Make (T)
end

let solve_puzzle (x, y) matrix =
  let rows = List.length matrix in
  let cols = List.length (List.nth_exn matrix 0) in
  let rec move x y (dir : direction) seen =
    match x, y with
    | x, y
      when (x = 0 && [%equal: direction] dir Up)
           || (x = rows - 1 && [%equal: direction] dir Down)
           || (y = 0 && [%equal: direction] dir Left)
           || (y = cols - 1 && [%equal: direction] dir Right) -> Set.add seen (x, y)
    | _ ->
      (* Take a step in the direction *)
      let next_x, next_y = direction_to_coordinate dir in
      let new_x = x + next_x in
      let new_y = y + next_y in
      (* If the character at the new location is a '#', change direction *)
      (match List.nth_exn (List.nth_exn matrix new_x) new_y with
       | '#' -> move x y (next_direction dir) seen
       | _ -> move new_x new_y dir (Set.add seen (x, y)))
  in
  move x y Up (Set.singleton (module Coord) (x, y))
;;

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let matrix = create_matrix data in
  Spice.debugf
    "Processing a %d x %d matrix"
    (List.length matrix)
    (List.length (List.nth_exn matrix 0));
  let* starting_point = find_starting_point matrix in
  let x, y = starting_point in
  Spice.infof "Starting point is at (%d, %d)" x y;
  let seen = solve_puzzle (x, y) matrix in
  let result = Set.length seen in
  Spice.infof "Result=%d" result;
  Ok ()
;;

part1 ()

(*
   For part 2, we need to add a blockage to the guards path so that they enter a loop.
*)

module CoorDir = struct
  module T = struct
    type t = int * int * direction [@@deriving compare, sexp, hash, equal]
  end

  include T
  include Comparator.Make (T)
end

let array_of_matrix matrix = Array.of_list (List.map matrix ~f:Array.of_list)

(* Using list *)
(* let check_for_loop matrix (sx, sy) = *)
(*   let rows = List.length matrix in *)
(*   let cols = List.length (List.nth_exn matrix 0) in *)
(*   (* Now we'll track (position, direction) pairs instead of just positions *) *)
(*   let rec move x y (dir : direction) seen = *)
(*     if x < 0 || x >= rows || y < 0 || y >= cols *)
(*     then false *)
(*     else ( *)
(*       (* Create a state tuple that represents our current position and direction *) *)
(*       let current_state = x, y, dir in *)
(*       (* If we've seen this exact state before, we've found a cycle *) *)
(*       if Set.mem seen current_state *)
(*       then true *)
(*       else ( *)
(*         match x, y with *)
(*         | x, y *)
(*           when (x = 0 && [%equal: direction] dir Up) *)
(*                || (x = rows - 1 && [%equal: direction] dir Down) *)
(*                || (y = 0 && [%equal: direction] dir Left) *)
(*                || (y = cols - 1 && [%equal: direction] dir Right) -> false *)
(*         | _ -> *)
(*           let next_x, next_y = direction_to_coordinate dir in *)
(*           let new_x = x + next_x in *)
(*           let new_y = y + next_y in *)
(*           if new_x < 0 || new_x >= rows || new_y < 0 || new_y >= cols *)
(*           then false *)
(*           else ( *)
(*             match List.nth_exn (List.nth_exn matrix new_x) new_y with *)
(*             | '#' -> move x y (next_direction dir) (Set.add seen current_state) *)
(*             | _ -> move new_x new_y dir (Set.add seen current_state)))) *)
(*   in *)
(*   (* Create a set that can store (position, direction) pairs *) *)
(*   let initial_seen = Set.empty (module CoorDir) in *)
(*   move sx sy Up initial_seen *)
(* ;; *)

(* let has_cycle starting_point (matrix : char list list) coord = *)
(*   (* Change the coordinate to a '#' *) *)
(*   let rows = List.length matrix in *)
(*   let cols = List.length (List.nth_exn matrix 0) in *)
(*   let test_row, test_col = coord in *)
(*   if test_row < 0 || test_row >= rows || test_col < 0 || test_col >= cols *)
(*   then false *)
(*   else ( *)
(*     let current_char = List.nth_exn (List.nth_exn matrix test_row) test_col in *)
(*     if Char.equal current_char '#' || Char.equal current_char '^' *)
(*     then false *)
(*     else ( *)
(*       let new_matrix = *)
(*         List.mapi matrix ~f:(fun row_idx row -> *)
(*           if row_idx = test_row *)
(*           then *)
(*             List.mapi row ~f:(fun col_idx char -> *)
(*               if col_idx = test_col then '#' else char) *)
(*           else row) *)
(*       in *)
(*       let result = check_for_loop new_matrix starting_point in *)
(*       result)) *)
(* ;; *)

let check_for_loop ~matrix ~obstacle ~start:(sx, sy) =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  (* Now we'll track (position, direction) pairs instead of just positions *)
  let rec move x y (dir : direction) seen =
    if x < 0 || x >= rows || y < 0 || y >= cols
    then false
    else (
      (* Create a state tuple that represents our current position and direction *)
      let current_state = x, y, dir in
      (* If we've seen this exact state before, we've found a cycle *)
      if Set.mem seen current_state
      then true
      else (
        match x, y with
        | x, y
          when (x = 0 && [%equal: direction] dir Up)
               || (x = rows - 1 && [%equal: direction] dir Down)
               || (y = 0 && [%equal: direction] dir Left)
               || (y = cols - 1 && [%equal: direction] dir Right) -> false
        | _ ->
          let next_x, next_y = direction_to_coordinate dir in
          let new_x = x + next_x in
          let new_y = y + next_y in
          if new_x < 0 || new_x >= rows || new_y < 0 || new_y >= cols
          then false
          else (
            (* If the new point is on the obstacle, treat it as such *)
            match matrix.(new_x).(new_y) with
            | '#' -> move x y (next_direction dir) (Set.add seen current_state)
            | _ -> (
                if [%equal: Coord.t] obstacle (new_x, new_y) then 
                    move x y (next_direction dir) (Set.add seen current_state)
                else 
                    move new_x new_y dir (Set.add seen current_state)
            ))))
  in
  (* Create a set that can store (position, direction) pairs *)
  let initial_seen = Set.empty (module CoorDir) in
  move sx sy Up initial_seen
;;
(**)
(* let _has_cycle starting_point (matrix : char array array) coord = *)
(*   (* Change the coordinate to a '#' *) *)
(*   let rows = Array.length matrix in *)
(*   let cols = Array.length matrix.(0) in *)
(*   let test_row, test_col = coord in *)
(*   if test_row < 0 || test_row >= rows || test_col < 0 || test_col >= cols *)
(*   then false *)
(*   else ( *)
(*     let current_char = matrix.(test_row).(test_col) in *)
(*     if Char.equal current_char '#' || Char.equal current_char '^' *)
(*     then false *)
(*     else ( *)
(*       let original = matrix.(test_row).(test_col) in *)
(*       matrix.(test_row).(test_col) <- '#'; *)
(*       let result = check_for_loop matrix starting_point in *)
(*       matrix.(test_row).(test_col) <- original; *)
(*       result)) *)
(* ;; *)

let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let list_matrix = create_matrix data in
  let matrix = array_of_matrix list_matrix in
  Spice.debugf
    "Processing a %d x %d matrix"
    (Array.length matrix)
    (Array.length (Array.get matrix 0));
  let* starting_point = find_starting_point list_matrix in
  let seen = solve_puzzle (starting_point |> fst, starting_point |> snd) list_matrix in
  let x, y = starting_point in
  (* First pass, iterate through all the items in the found path *)
  (* let items = seen |> Set.to_list in *)
  (* let loop_items = *)
  (*   items *)
  (*   |> List.fold ~init:0 ~f:(fun acc coord -> *)
  (*     match has_cycle (x, y) matrix coord with *)
  (*     | true -> acc + 1 *)
  (*     | false -> acc) *)
  (* in *)
  let items = seen |> Set.to_list |> List.dedup_and_sort ~compare:[%compare: Coord.t] in
  let chunk_size = max 1 (List.length items / 4) in
  let chunks = List.chunks_of ~length:chunk_size items in
  let results =
    List.map chunks ~f:(fun chunk ->
      let domain =
        Domain.spawn (fun () ->
          List.count chunk ~f:(fun coord ->
            check_for_loop ~start:(x, y) ~matrix ~obstacle:coord))
      in
      domain)
  in
  let loop_items = List.sum (module Int) results ~f:(fun domain -> Domain.join domain) in
  Spice.infof "Result=%d" loop_items;
  Ok ()
;;

part2 ()
