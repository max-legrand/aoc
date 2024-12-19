open Core

let () = Spice.info "2024 Day 15"

(* let input = "bin/2024/day15/data/test.txt" *)
(* let input = "bin/2024/day15/data/smalltest.txt" *)
let input = "bin/2024/day15/data/puzzle.txt"

let print_map map =
  map
  |> Array.iter ~f:(fun row ->
    let row_string =
      row |> Array.fold ~init:"" ~f:(fun acc x -> acc ^ Char.to_string x)
    in
    Spice.debugf "%s" row_string)
;;

let generate_single_wide_map map =
  String.split_lines map |> List.map ~f:String.to_array |> List.to_array
;;

let generate_double_wide_map map =
  String.split_lines map
  |> List.map ~f:(fun line ->
    let arr = line |> String.to_list in
    List.concat
      (List.map arr ~f:(fun x ->
         match x with
         | 'O' -> [ '['; ']' ]
         | '#' -> [ '#'; '#' ]
         | '.' -> [ '.'; '.' ]
         | '@' -> [ '@'; '.' ]
         | _ -> failwith "Invalid character"))
    |> List.to_array)
  |> List.to_array
;;

let parse_input data map_func =
  (* First, split the string on the double newline to get the map and the directions *)
  let lines = Str.split (Str.regexp_string "\n\n") data in
  let map = List.nth_exn lines 0 in
  let directions = List.nth_exn lines 1 in
  let directions = String.substr_replace_all ~pattern:"\n" ~with_:"" directions in
  let map = map_func map in
  map, directions
;;

type bounds =
  { rows : int
  ; cols : int
  }

let rec move bounds map direction (x, y) =
  let nx, ny = x + (direction |> fst), y + (direction |> snd) in
  Spice.debugf "nx: %d ny: %d" nx ny;
  if nx < 0 || nx >= bounds.rows || ny < 0 || ny >= bounds.cols
  then x, y
  else if Char.equal map.(nx).(ny) '#'
  then x, y
  else if Char.equal map.(nx).(ny) '.'
  then (
    map.(nx).(ny) <- map.(x).(y);
    map.(x).(y) <- '.';
    nx, ny)
  else (
    let moved_x, moved_y = move bounds map direction (nx, ny) in
    Spice.debugf "moved_x: %d moved_y: %d" moved_x moved_y;
    if moved_x = nx && moved_y = ny then x, y else move bounds map direction (x, y))
;;

let move_boxes movefn map directions start =
  let bounds = { rows = Array.length map; cols = Array.length map.(0) } in
  Spice.debugf "start: %d,%d" (start |> fst) (start |> snd);
  String.fold ~init:start directions ~f:(fun acc direction ->
    Spice.debugf "point: %d, %d - direction: %c" (acc |> fst) (acc |> snd) direction;
    let d =
      match direction with
      | '^' -> -1, 0
      | 'v' -> 1, 0
      | '>' -> 0, 1
      | '<' -> 0, -1
      | _ -> failwith "Invalid direction"
    in
    movefn bounds map d acc)
  |> ignore;
  ()
;;

let find_starting_position map =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let rec helper row col =
    if Char.equal map.(row).(col) '@'
    then row, col
    else if col + 1 < cols
    then helper row (col + 1)
    else if row + 1 < rows
    then helper (row + 1) 0
    else failwith "Could not find starting position"
  in
  helper 0 0
;;

let calculate_score map =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let rec helper acc row col =
    if row = rows && col = cols
    then acc
    else (
      let char = map.(row).(col) in
      match char with
      | 'O' ->
        let new_acc = acc + (100 * row) + col in
        if col + 1 < cols
        then helper new_acc row (col + 1)
        else if row + 1 < rows
        then helper new_acc (row + 1) 0
        else new_acc
      | _ ->
        if col + 1 < cols
        then helper acc row (col + 1)
        else if row + 1 < rows
        then helper acc (row + 1) 0
        else acc)
  in
  helper 0 0 0
;;

(* Part 1 has a robot moving boxes, very similar to rock puzzles in Pokemon games *)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let map, directions = parse_input data generate_single_wide_map in
  let start = find_starting_position map in
  move_boxes move map directions start;
  map
  |> Array.iter ~f:(fun row ->
    let row_string =
      row |> Array.fold ~init:"" ~f:(fun acc x -> acc ^ Char.to_string x)
    in
    Spice.debugf "%s" row_string);
  let score = calculate_score map in
  Spice.infof "Score: %d" score
;;

part1 ()

(* Part 2 is doublly wide... *)
let move_double map direction (x, y) =
  print_map map;
  let seen = Hash_set.Poly.create () in
  let rec build_next_points list acc =
    match list with
    | [] -> List.rev (Hash_set.Poly.to_list seen), acc
    | (x, y) :: points ->
      if Hash_set.Poly.mem seen (x, y)
      then build_next_points points acc
      else (
        Hash_set.Poly.add seen (x, y);
        let nx, ny = x + (direction |> fst), y + (direction |> snd) in
        if Char.equal map.(nx).(ny) '#'
        then List.rev (Hash_set.Poly.to_list seen), false
        else if Char.equal map.(nx).(ny) '[' || Char.equal map.(nx).(ny) ']'
        then (
          let next =
            if Char.equal map.(nx).(ny) '['
            then [ nx, ny; nx, ny + 1 ]
            else [ nx, ny; nx, ny - 1 ]
          in
          build_next_points (next @ points) acc)
        else build_next_points points acc)
  in
  let next_points, valid = build_next_points [ x, y ] true in
  if valid
  then (
    let map_clone = Array.copy_matrix map in
    List.iter next_points ~f:(fun (x, y) -> map_clone.(x).(y) <- '.');
    List.iter next_points ~f:(fun (x, y) ->
      map_clone.(x + fst direction).(y + snd direction) <- map.(x).(y));
    Array.iteri map ~f:(fun row_idx row ->
      row
      |> Array.iteri ~f:(fun col_idx _ ->
        map.(row_idx).(col_idx) <- map_clone.(row_idx).(col_idx)));
    x + fst direction, y + snd direction)
  else x, y
;;

let move_boxes_double map directions start =
  Spice.debugf "start: %d,%d" (start |> fst) (start |> snd);
  let endpoint =
    String.fold ~init:start directions ~f:(fun acc direction ->
      Spice.debugf "point: %d, %d - direction: %c" (acc |> fst) (acc |> snd) direction;
      let d =
        match direction with
        | '^' -> -1, 0
        | 'v' -> 1, 0
        | '>' -> 0, 1
        | '<' -> 0, -1
        | _ -> failwith "Invalid direction"
      in
      move_double map d acc)
  in
  map.(fst endpoint).(snd endpoint) <- '@';
  ()
;;

let double_wide () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let map, directions = parse_input data generate_double_wide_map in
  print_map map;
  let start = find_starting_position map in
  move_boxes_double map directions start;
  print_map map;
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let rec calculate_score acc (x, y) =
    if x = rows || y = cols
    then acc
    else if Char.equal map.(x).(y) '['
    then (
      let new_acc = acc + (100 * x) + y in
      if y + 1 < cols
      then calculate_score new_acc (x, y + 1)
      else calculate_score new_acc (x + 1, 0))
    else if y + 1 < cols
    then calculate_score acc (x, y + 1)
    else calculate_score acc (x + 1, 0)
  in
  let score = calculate_score 0 (0, 0) in
  Spice.infof "Score: %d" score;
  ()
;;

double_wide ()
