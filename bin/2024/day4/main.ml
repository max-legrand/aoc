Log.info "2024 Day 4"

(* let input = "bin/2024/day4/data/test.txt" *)
let input = "bin/2024/day4/data/puzzle.txt"

(*
   Part 1 has us doing graph traversals to find instances of "XMAS" in a word search.
   Generally, the approach should be to iterate through each row & column until we get to the start of an `X` character
   and then search all possible directions.
*)

let directions =
  [ (* Up *)
    -1, 0
  ; (* Down *)
    1, 0
  ; (* Left *)
    0, -1
  ; (* Right *)
    0, 1
  ; (* Up-Left *)
    -1, -1
  ; (* Up-Right *)
    -1, 1
  ; (* Down-Left *)
    1, -1
  ; (* Down-Right *)
    1, 1
  ]
;;

let cross = [ -1, -1; -1, 1; 1, -1; 1, 1 ]
let in_bounds (row, col) rows cols = row >= 0 && row < rows && col >= 0 && col < cols

let rec expand matrix x y rows cols dir last =
  let dirs =
    match dir with
    | Some d -> [ d ]
    | None -> directions
  in
  let new_points =
    dirs
    |> List.map (fun (dx, dy) -> x + dx, y + dy, (dx, dy))
    |> List.filter (fun (x, y, _) -> in_bounds (x, y) rows cols)
  in
  (* Now we have the new points to look at *)
  List.fold_left
    (fun acc (x, y, dir) ->
      let current = List.nth (List.nth matrix x) y in
      let dir = Some dir in
      let next_result =
        match last, current with
        | "X", 'M' -> expand matrix x y rows cols dir "XM"
        | "X", _ -> 0
        | "XM", 'A' -> expand matrix x y rows cols dir "XMA"
        | "XM", _ -> 0
        | "XMA", 'S' -> 1
        | "XMA", _ -> 0
        | _ -> 0
      in
      acc + next_result)
    0
    new_points
;;

let search_graph matrix =
  let rows = List.length matrix in
  let cols = List.length (List.nth matrix 0) in
  let rec traverse (row, col) acc =
    if not (in_bounds (row, col) rows cols)
    then acc
    else (
      let current = List.nth (List.nth matrix row) col in
      let new_point =
        match col with
        | x when x = cols - 1 -> row + 1, 0
        | _ -> row, col + 1
      in
      match current with
      | 'X' ->
        Log.debug (Printf.sprintf "Found X at %d,%d" row col);
        (match expand matrix row col rows cols None "X" with
         | 0 -> traverse new_point acc
         | x ->
           Log.debug (Printf.sprintf "Found starting @ XMAS at %d,%d" row col);
           traverse new_point (acc + x))
      | _ -> traverse new_point acc)
  in
  traverse (0, 0) 0
;;

let part1 () =
  (* Log.set_log_level Log.DEBUG; *)
  let lines = Utils.read_file input in
  let matrix = List.map (fun x -> String.to_seq x |> List.of_seq) lines in
  let result = search_graph matrix in
  Log.info (Printf.sprintf "Result: %d" result);
  ()
;;

part1 ()

(*
   Part 2 is a bit more involved, we need to find instances of "MAS" forming an X shape.
   For example:
   M . S
   . A .
   M . S

   The key here, is there are only 4 combinations to check AND all of them have the `A` character at the center.
*)

let check_for_cross matrix (row, col) =
  let rows = List.length matrix in
  let cols = List.length (List.nth matrix 0) in
  let new_point = cross |> List.map (fun (dx, dy) -> row + dx, col + dy) in
  (* If any points in the new_point list are out of bounds, we need to return *)
  let all_points_in_bounds points =
    List.for_all (fun (x, y) -> in_bounds (x, y) rows cols) points
  in
  match all_points_in_bounds new_point with
  | false -> 0
  | true ->
    (* Check that we have a valid cross *)
    let top_left = List.nth (List.nth matrix (row - 1)) (col - 1) in
    let top_right = List.nth (List.nth matrix (row - 1)) (col + 1) in
    let bottom_left = List.nth (List.nth matrix (row + 1)) (col - 1) in
    let bottom_right = List.nth (List.nth matrix (row + 1)) (col + 1) in
    (match top_left, top_right, bottom_left, bottom_right with
     | 'M', 'S', 'M', 'S' | 'M', 'M', 'S', 'S' | 'S', 'S', 'M', 'M' | 'S', 'M', 'S', 'M'
       -> 1
     | _ -> 0)
;;

let search_for_mas matrix =
  let rows = List.length matrix in
  let cols = List.length (List.nth matrix 0) in
  let rec traverse (row, col) acc =
    if not (in_bounds (row, col) rows cols)
    then acc
    else (
      let current = List.nth (List.nth matrix row) col in
      let new_point =
        match col with
        | x when x = cols - 1 -> row + 1, 0
        | _ -> row, col + 1
      in
      match current with
      | 'A' ->
        Log.debug (Printf.sprintf "Found A at %d,%d" row col);
        let new_result = check_for_cross matrix (row, col) in
        traverse new_point (new_result + acc)
      | _ -> traverse new_point acc)
  in
  traverse (0, 0) 0
;;

let part2 () =
  (* Log.set_log_level Log.DEBUG; *)
  let lines = Utils.read_file input in
  let matrix = List.map (fun x -> String.to_seq x |> List.of_seq) lines in
  let result = search_for_mas matrix in
  Log.info (Printf.sprintf "Result: %d" result)
;;

part2 ()
