open Base

let () = Spice.info "2024 Day 12"

(* let input = "bin/2024/day12/data/test.txt" *)
let input = "bin/2024/day12/data/puzzle.txt"

(** Convert the input list into a 2D array *)
let generate_array lines =
  let rec build_array acc lines =
    match lines with
    | [] -> acc
    | line :: lines ->
      let row = line |> String.to_array in
      build_array (row :: acc) lines
  in
  build_array [] lines |> List.rev |> Array.of_list
;;

let print_matrix ~(matrix : char array array) =
  let rec print_matrix' ~acc ~(matrix : char array array) ~(row : int) =
    if row >= Array.length matrix
    then acc
    else (
      let new_acc = acc ^ "\n" in
      let row_array = matrix.(row) in
      let row_string =
        Array.fold row_array ~init:"" ~f:(fun acc2 col -> acc2 ^ Printf.sprintf "%c " col)
      in
      print_matrix' ~acc:(new_acc ^ "  " ^ row_string) ~matrix ~row:(row + 1))
  in
  print_matrix' ~acc:"[" ~matrix ~row:0 ^ "\n]"
;;

let is_valid ~(matrix : char array array) ~(row : int) ~(col : int) =
  let total_rows = Array.length matrix in
  let total_cols = Array.length matrix.(0) in
  row >= 0 && row < total_rows && col >= 0 && col < total_cols
;;

let traverse_group
  ~(matrix : char array array)
  ~visited
  ~(row_start : int)
  ~(col_start : int)
  =
  let character = matrix.(row_start).(col_start) in
  let rec traverse_group' ~(row : int) ~(col : int) ~perimiter ~area =
    if not (is_valid ~matrix ~row ~col)
    then perimiter, area
    else if visited.(row).(col) || not (Char.equal matrix.(row).(col) character)
    then perimiter, area
    else (
      visited.(row).(col) <- true;
      let directions = [ 1, 0; -1, 0; 0, 1; 0, -1 ] in
      let edge_count = ref 0 in
      directions
      |> List.iter ~f:(fun (dr, dc) ->
        let next_row = row + dr in
        let next_col = col + dc in
        if not (is_valid ~matrix ~row:next_row ~col:next_col)
        then Int.incr edge_count
        else if not (Char.equal matrix.(next_row).(next_col) character)
        then Int.incr edge_count);
      let new_permiter = perimiter + !edge_count in
      directions
      |> List.fold
           ~init:(new_permiter, area + 1)
           ~f:(fun (perimiter, area) (dr, dc) ->
             let new_row, new_col = row + dr, col + dc in
             let next_p, next_a =
               traverse_group' ~row:new_row ~col:new_col ~perimiter ~area
             in
             next_p, next_a))
  in
  traverse_group' ~row:row_start ~col:col_start ~perimiter:0 ~area:0
;;

let next_coordinate ~(rows : int) ~(row : int) ~(cols : int) ~(col : int) =
  if col < cols - 1 then row, col + 1 else if row < rows - 1 then row + 1, 0 else -1, -1
;;

let parse_matrix ~(matrix : char array array) traverse_func =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let visited = Array.make_matrix ~dimx:rows ~dimy:cols false in
  let rec iterate_matrix ~(row : int) ~(col : int) acc =
    if row < 0 || col < 0
    then acc
    else if visited.(row).(col)
    then (
      let next_row, next_col = next_coordinate ~rows ~row ~cols ~col in
      iterate_matrix ~row:next_row ~col:next_col acc)
    else (
      let perimiter, area =
        traverse_func ~matrix ~visited ~row_start:row ~col_start:col
      in
      let new_acc = (matrix.(row).(col), perimiter, area) :: acc in
      let next_row, next_col = next_coordinate ~rows ~row ~cols ~col in
      iterate_matrix ~row:next_row ~col:next_col new_acc)
  in
  iterate_matrix ~row:0 ~col:0 []
;;

let calculate_price groups =
  groups |> List.fold ~init:0 ~f:(fun acc (_c, p, a) -> acc + (p * a))
;;

(* Part 1 has us finding the area and perimiter of groups within a grid *)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let lines = Utils.read_file input in
  let grid = generate_array lines in
  Spice.debugf "Grid:\n%s" (print_matrix ~matrix:grid);
  let result = parse_matrix ~matrix:grid traverse_group in
  result
  |> List.iter ~f:(fun (c, p, a) ->
    Spice.debugf "Character: %c, Perimiter: %d, Area: %d" c p a);
  let price = calculate_price result in
  Spice.infof "Price: %d" price;
  ()
;;

part1 ()

let directions = [ 1, 0; -1, 0; 0, 1; 0, -1 ]

let get_perimeter_points ~(matrix : char array array) ~points =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let perimeter = ref [] in
  Hash_set.Poly.iter points ~f:(fun (row, col) ->
    List.iter directions ~f:(fun (dr, dc) ->
      let x', y' = row + dr, col + dc in
      if x' < 0
         || y' < 0
         || x' >= rows
         || y' >= cols
         || not (Hash_set.Poly.mem points (x', y'))
      then perimeter := ((row, col), (x', y')) :: !perimeter));
  Hash_set.Poly.of_list !perimeter
;;

let traverse_sides
  ~(matrix : char array array)
  ~visited
  ~(row_start : int)
  ~(col_start : int)
  =
  let character = matrix.(row_start).(col_start) in
  let group = Hash_set.Poly.create () in
  let rec traverse_group' ~(row : int) ~(col : int) ~(area : int) : int =
    if (not (is_valid ~matrix ~row ~col))
       || visited.(row).(col)
       || not (Char.equal matrix.(row).(col) character)
    then area
    else (
      visited.(row).(col) <- true;
      Hash_set.Poly.add group (row, col);
      let a =
        directions
        |> List.fold ~init:(area + 1) ~f:(fun acc (dr, dc) ->
          let next_row = row + dr in
          let next_col = col + dc in
          if (not (is_valid ~matrix ~row:next_row ~col:next_col))
             || not (Char.equal matrix.(next_row).(next_col) character)
          then acc
          else traverse_group' ~row:next_row ~col:next_col ~area:acc)
      in
      a)
  in
  let area = traverse_group' ~row:row_start ~col:col_start ~area:0 in
  Spice.debugf "Group for character %c" character;
  group |> Hash_set.Poly.iter ~f:(fun (x, y) -> Spice.debugf "(%d, %d)" x y);
  let perimeter = get_perimeter_points ~matrix ~points:group in
  let sides =
    perimeter
    |> Hash_set.Poly.filter ~f:(fun (p1, p2) ->
      let dirs = [ 1, 0; 0, 1 ] in
      List.fold dirs ~init:true ~f:(fun acc (dr, dc) ->
        let p1_next = (p1 |> fst) + dr, (p1 |> snd) + dc in
        let p2_next = (p2 |> fst) + dr, (p2 |> snd) + dc in
        if Hash_set.Poly.mem perimeter (p1_next, p2_next) then false else acc))
    |> Hash_set.Poly.length
  in
  Spice.debugf "Area: %d, Sides: %d" area sides;
  area, sides
;;

(* For part 2, we need to track sides instead of perimiter for price calculation *)
let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let lines = Utils.read_file input in
  let grid = generate_array lines in
  Spice.debugf "Grid:\n%s" (print_matrix ~matrix:grid);
  let result = parse_matrix ~matrix:grid traverse_sides in
  let price = calculate_price result in
  Spice.infof "Price: %d" price;
  ()
;;

part2 ()
