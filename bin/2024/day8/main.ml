open Base

let () = Spice.info "2024 Day 8"

(* let input = "bin/2024/day8/data/alttest2.txt" *)
(* let input = "bin/2024/day8/data/test.txt" *)
let input = "bin/2024/day8/data/puzzle.txt"

let create_matrix data =
  let process_row row = row |> String.to_list in
  let rec parse_rows acc rows =
    match rows with
    | [] -> acc
    | row :: rows -> parse_rows (process_row row :: acc) rows
  in
  List.rev (parse_rows [] data)
;;

let get_points (matrix : char list list) =
  let points_lookup = Hashtbl.create (module Char) in
  List.iteri matrix ~f:(fun row_idx row ->
    List.iteri row ~f:(fun col_idx char ->
      match char with
      | '.' -> ()
      | _ ->
        (* Add the point to the lookup table *)
        (match Hashtbl.find points_lookup char with
         | None -> Hashtbl.set points_lookup ~key:char ~data:[ row_idx, col_idx ]
         | Some points ->
           Hashtbl.set points_lookup ~key:char ~data:((row_idx, col_idx) :: points))));
  points_lookup
;;

(** Apply the distance formula to find the integer distance between two points *)
let get_distance (point1 : int * int) (point2 : int * int) =
  let x1, y1 = point1 in
  let x2, y2 = point2 in
  let x_diff = x1 - x2 in
  let y_diff = y1 - y2 in
  (x_diff ** 2) + (y_diff ** 2)
  |> Float.of_int
  |> Float.sqrt
  |> Float.abs
  |> Float.round_up
  |> Int.of_float
;;

let print_pair (point : int * int) = Printf.sprintf "(%d,%d)" (fst point) (snd point)
let pair_equal (p1 : int * int) (p2 : int * int) = fst p1 = fst p2 && snd p1 = snd p2

let get_antinode_pair (point1 : int * int) (point2 : int * int) =
  (*
     There are four combinations of possible arrangements:
     1.
     . . . . .
     . x . . .
     . . . . .
     . . . y .
     . . . . .

     2.
     . . . . .
     . . . y .
     . . . . .
     . x . . .
     . . . . .

     3.
     . . . . .
     . . . . .
     . x . . y
     . . . . .
     . . . . .

     4.
     . . . . .
     . . x . .
     . . . . .
     . . . . .
     . . y . .
  *)
  let x1, y1 = point1 in
  let x2, y2 = point2 in
  let left, right =
    match y1 < y2 with
    | true -> point1, point2
    | false -> point2, point1
  in
  let top, bottom =
    match x1 < x2 with
    | true -> point1, point2
    | false -> point2, point1
  in
  Spice.debugf "top=%s bottom=%s" (top |> print_pair) (bottom |> print_pair);
  Spice.debugf "left=%s right=%s" (left |> print_pair) (right |> print_pair);
  match x1 - x2, y1 - y2 with
  (* Same point *)
  | 0, 0 -> []
  (* Horizontal *)
  | 0, y ->
    Spice.debugf "Horizontal";
    [ fst left, snd left - y; fst right, snd right + y ]
  (* Vertical *)
  | x, 0 ->
    Spice.debugf "Vertical";
    [ fst top - x, snd top; fst bottom + x, snd bottom ]
  (* Case 1 *)
  | _, _ ->
    if pair_equal left top && pair_equal right bottom
    then (
      (* Case 1*)
      Spice.debugf "Case 1";
      let x_diff = fst bottom - fst top in
      let y_diff = snd bottom - snd top in
      [ fst top - x_diff, snd top - y_diff; fst bottom + x_diff, snd bottom + y_diff ])
    else (
      (* Case 2 *)
      Spice.debugf "Case 2";
      let x_diff = fst bottom - fst top in
      let y_diff = snd top - snd bottom in
      [ fst bottom + x_diff, snd bottom - y_diff; fst top - x_diff, snd top + y_diff ])
;;

(** Process the antenna points and return the antinodes for the specific antenna *)
let process_antenna (points : (int * int) list) =
  let antinodes =
    List.foldi points ~init:[] ~f:(fun i acc point_a ->
      let pairs =
        List.init
          (List.length points - (i + 1))
          ~f:(fun k ->
            let point_b = List.nth_exn points (i + k + 1) in
            Spice.debugf
              "point_a=(%d,%d) point_b=(%d,%d)"
              (fst point_a)
              (snd point_a)
              (fst point_b)
              (snd point_b);
            let distance = get_distance point_a point_b in
            if distance < 2
            then []
            else (
              let nodes = get_antinode_pair point_a point_b in
              let nodes_str =
                List.map nodes ~f:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y)
                |> String.concat ~sep:", "
              in
              Spice.debugf "nodes=%s" nodes_str;
              nodes))
        |> List.concat
      in
      List.append acc pairs)
  in
  antinodes
;;

let generate_antinodes dimensions (points : (char, (int * int) list) Hashtbl.t) =
  let unique_points = Set.Poly.empty in
  points
  |> Hashtbl.fold ~init:unique_points ~f:(fun ~key:char ~data:items acc ->
    Spice.debugf "char=%c [%d items]" char (List.length items);
    let antinodes = process_antenna items in
    List.fold antinodes ~init:acc ~f:Set.Poly.add)
  |> Set.Poly.filter ~f:(fun (x, y) ->
    x >= 0 && x < fst dimensions && y >= 0 && y < snd dimensions)
;;

(* Part 1 we are finding unique points that are in line with nodes on a matrix *)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let matrix = create_matrix data in
  Spice.debugf
    "Matrix size=%d x %d"
    (List.length matrix)
    (List.length (List.hd_exn matrix));
  let lookup = get_points matrix in
  let dimensions = List.length matrix, List.length (List.hd_exn matrix) in
  let antinodes = generate_antinodes dimensions lookup in
  Set.Poly.iter antinodes ~f:(fun (x, y) -> Spice.debugf "(%d,%d)" x y);
  Spice.infof "Unique points=%d" (Set.Poly.length antinodes)
;;

part1 ()

(* Part 2 the antinodes occur whenever a location is in line with two nodes *)

let get_ext_antinode_pairs (point1 : int * int) (point2 : int * int) bounds =
  let rows, cols = bounds in
  (*
     There are four combinations of possible arrangements:
     1.
     . . . . .
     . x . . .
     . . . . .
     . . . y .
     . . . . .

     2.
     . . . . .
     . . . y .
     . . . . .
     . x . . .
     . . . . .

     3.
     . . . . .
     . . . . .
     . x . . y
     . . . . .
     . . . . .

     4.
     . . . . .
     . . x . .
     . . . . .
     . . . . .
     . . y . .
  *)
  let _x1, y1 = point1 in
  let _x2, y2 = point2 in
  let left, right =
    match y1 < y2 with
    | true -> point1, point2
    | false -> point2, point1
  in
  let x_diff = fst right - fst left in
  let y_diff = snd right - snd left in
  (* Starting from the left point, walk in the reverse of the slope to find all points *)
  let rec walk_left (x : int) (y : int) acc =
    if x < 0 || y < 0 then acc else walk_left (x - x_diff) (y - y_diff) ((x, y) :: acc)
  in
  let rec walk_right (x : int) (y : int) acc =
    if x >= rows || y >= cols
    then acc
    else walk_right (x + x_diff) (y + y_diff) ((x, y) :: acc)
  in
  let left_points = walk_left (fst left) (snd left) [] in
  let right_points = walk_right (fst right) (snd right) [] in
  List.concat [ left_points; right_points ]
;;

let process_antenna_ext (points : (int * int) list) bounds =
  let antinodes =
    List.foldi points ~init:[] ~f:(fun i acc point_a ->
      let pairs =
        List.init
          (List.length points - (i + 1))
          ~f:(fun k ->
            let point_b = List.nth_exn points (i + k + 1) in
            Spice.debugf
              "point_a=(%d,%d) point_b=(%d,%d)"
              (fst point_a)
              (snd point_a)
              (fst point_b)
              (snd point_b);
            let nodes = get_ext_antinode_pairs point_a point_b bounds in
            let nodes_str =
              List.map nodes ~f:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y)
              |> String.concat ~sep:", "
            in
            Spice.debugf "nodes=%s" nodes_str;
            nodes)
        |> List.concat
      in
      List.append acc pairs)
  in
  if List.length points > 2 then List.concat [ antinodes; points ] else antinodes
;;

let generate_antinodes_ext dimensions (points : (char, (int * int) list) Hashtbl.t) =
  let unique_points = Set.Poly.empty in
  points
  |> Hashtbl.fold ~init:unique_points ~f:(fun ~key:char ~data:items acc ->
    Spice.debugf "char=%c [%d items]" char (List.length items);
    let antinodes = process_antenna_ext items dimensions in
    List.fold antinodes ~init:acc ~f:Set.Poly.add)
  |> Set.Poly.filter ~f:(fun (x, y) ->
    x >= 0 && x < fst dimensions && y >= 0 && y < snd dimensions)
;;

let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let matrix = create_matrix data in
  Spice.debugf
    "Matrix size=%d x %d"
    (List.length matrix)
    (List.length (List.hd_exn matrix));
  let lookup = get_points matrix in
  let dimensions = List.length matrix, List.length (List.hd_exn matrix) in
  let antinodes = generate_antinodes_ext dimensions lookup in
  Set.Poly.iter antinodes ~f:(fun (x, y) -> Spice.debugf "(%d,%d)" x y);
  Spice.infof "Unique points=%d" (Set.Poly.length antinodes)
;;

part2 ()
