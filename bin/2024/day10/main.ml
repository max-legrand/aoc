open Base

let () = Spice.info "2024 Day 10"

(* let input = "bin/2024/day10/data/test.txt" *)
let input = "bin/2024/day10/data/puzzle.txt"

(*
   Part 1 has us doing a graph traversal. The constraints here are each step MUST increase by 1
   and we can only move in cardinal directions.
*)
let create_matrix ~(data : string list) =
  data
  |> List.map ~f:(fun line ->
    line
    |> String.to_list
    |> Array.of_list
    |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0'))
  |> Array.of_list
;;

let print_matrix ~(matrix : int array array) =
  let rec print_matrix' ~acc ~(matrix : int array array) ~(row : int) =
    if row >= Array.length matrix
    then acc
    else (
      let new_acc = acc ^ "\n" in
      let row_array = matrix.(row) in
      let row_string =
        Array.fold row_array ~init:"" ~f:(fun acc2 col -> acc2 ^ Printf.sprintf "%d " col)
      in
      print_matrix' ~acc:(new_acc ^ "  " ^ row_string) ~matrix ~row:(row + 1))
  in
  print_matrix' ~acc:"[" ~matrix ~row:0 ^ "\n]"
;;

let find_endpoints ~(matrix : int array array) ~row ~col =
  let total_rows = Array.length matrix in
  let total_cols = Array.length matrix.(0) in
  let directions = [ 1, 0; 0, 1; -1, 0; 0, -1 ] in
  let unique_ones = ref Set.Poly.empty in
  let rec find_endpoints' ~acc ~(row : int) ~(col : int) =
    let current = matrix.(row).(col) in
    match current with
    | 9 ->
      if Set.Poly.mem !unique_ones (row, col)
      then acc
      else (
        Spice.debugf "Found unique end_of_path at %d %d" row col;
        unique_ones := Set.Poly.add !unique_ones (row, col);
        acc + 1)
    | _ ->
      (*
         Apply the directions to the current position
         and filter based on points within the matrix
      *)
      let neighbors =
        directions
        |> List.map ~f:(fun (dx, dy) -> row + dx, col + dy)
        |> List.filter ~f:(fun (x, y) ->
          (x >= 0 && x < total_rows && y >= 0 && y < total_cols)
          && matrix.(x).(y) - current = 1)
      in
      acc
      + List.fold neighbors ~init:0 ~f:(fun acc' (x, y) ->
        acc' + find_endpoints' ~acc:0 ~row:x ~col:y)
  in
  find_endpoints' ~acc:0 ~row ~col
;;

let parse_matrix ~(matrix : int array array) ~score_func =
  Array.foldi matrix ~init:0 ~f:(fun row_idx acc row ->
    acc
    + Array.foldi row ~init:0 ~f:(fun col_idx acc2 col ->
      match col with
      | 0 ->
        let score = score_func ~matrix ~row:row_idx ~col:col_idx in
        Spice.debugf "Trailhead @ (%d, %d) has score=%d" row_idx col_idx score;
        acc2 + score
      | _ -> acc2))
;;

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let matrix = create_matrix ~data in
  Spice.debugf "matrix:\n%s" (print_matrix ~matrix);
  let score = parse_matrix ~matrix ~score_func:find_endpoints in
  Spice.infof "score: %d" score;
  ()
;;

part1 ()

(*
   For part 2, instead of finding the number of unique end points, we are finding the
   number of unique paths.
*)

let find_paths ~(matrix : int array array) ~row ~col =
  let total_rows = Array.length matrix in
  let total_cols = Array.length matrix.(0) in
  let directions = [ 1, 0; 0, 1; -1, 0; 0, -1 ] in
  let unique_paths = ref Set.Poly.empty in
  let rec find_paths' ~acc ~(row : int) ~(col : int) ~path =
    let current = matrix.(row).(col) in
    match current with
    | 9 ->
      if Set.Poly.mem !unique_paths path
      then acc
      else (
        Spice.debugf "Found unique end_of_path at %d %d" row col;
        unique_paths := Set.Poly.add !unique_paths (9 :: path);
        acc + 1)
    | value ->
      (*
         Apply the directions to the current position
         and filter based on points within the matrix
      *)
      let neighbors =
        directions
        |> List.map ~f:(fun (dx, dy) -> row + dx, col + dy)
        |> List.filter ~f:(fun (x, y) ->
          (x >= 0 && x < total_rows && y >= 0 && y < total_cols)
          && matrix.(x).(y) - current = 1)
      in
      acc
      + List.fold neighbors ~init:0 ~f:(fun acc' (x, y) ->
        acc' + find_paths' ~acc:0 ~row:x ~col:y ~path:(value :: path))
  in
  find_paths' ~acc:0 ~row ~col ~path:[ 0 ]
;;

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let data = Utils.read_file input in
  let matrix = create_matrix ~data in
  Spice.debugf "matrix:\n%s" (print_matrix ~matrix);
  let score = parse_matrix ~matrix ~score_func:find_paths in
  Spice.infof "score: %d" score;
  ()
;;

part2 ()
