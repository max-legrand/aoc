open Core

let () = Spice.info "2024 Day 20"

(* let input = "bin/2024/day20/data/test.txt" *)
let input = "bin/2024/day20/data/puzzle.txt"
let map = ref [||]
let rows = ref 0
let cols = ref 0
let start = ref [||]
let goal = ref [||]

let create_matrix (data : string list) =
  rows := List.length data;
  cols := String.length (List.nth_exn data 0);
  let matrix = Array.make_matrix ~dimx:!rows ~dimy:!cols '*' in
  map := matrix;
  data
  |> List.iteri ~f:(fun row_idx row ->
    row
    |> String.iteri ~f:(fun col_idx char ->
      if Char.equal char 'S'
      then start := [| row_idx; col_idx |]
      else if Char.equal char 'E'
      then goal := [| row_idx; col_idx |];
      !map.(row_idx).(col_idx) <- char))
;;

(* This code is copy+pasted from day 18 *)
let points_equal item1 item2 = Array.equal Int.equal item1 item2

let is_valid_point coord =
  if coord.(0) >= 0
     && coord.(0) < !rows
     && coord.(1) >= 0
     && coord.(1) < !cols
     && not (Char.equal !map.(coord.(0)).(coord.(1)) '#')
  then true
  else false
;;

let get_next_tiles current =
  let left = [| current.(0); current.(1) - 1 |] in
  let right = [| current.(0); current.(1) + 1 |] in
  let up = [| current.(0) - 1; current.(1) |] in
  let down = [| current.(0) + 1; current.(1) |] in
  let next = [ left; right; up; down ] in
  List.filter next ~f:is_valid_point
;;

let find_min_path () =
  let queue = Utils.PrioQ.create () in
  Utils.PrioQ.add queue !start 0;
  let distances = Hashtbl.Poly.create () in
  Hashtbl.Poly.set ~key:!start ~data:0 distances;
  let rec process_queue () =
    match Utils.PrioQ.pop queue with
    | None -> distances
    | Some (current, cost) ->
      if points_equal current !goal
      then distances
      else if cost > Hashtbl.Poly.find_exn distances current
      then process_queue ()
      else (
        List.iter (get_next_tiles current) ~f:(fun next ->
          let new_cost = cost + 1 in
          match Hashtbl.Poly.find distances next with
          | None ->
            Hashtbl.Poly.set ~key:next ~data:new_cost distances;
            Utils.PrioQ.add queue next new_cost
          | Some old_cost when new_cost < old_cost ->
            Hashtbl.Poly.set ~key:next ~data:new_cost distances;
            Utils.PrioQ.add queue next new_cost
          | _ -> ());
        process_queue ())
  in
  process_queue ()
;;

(* This is a "brute force" approach and is very slow for the large test case *)
let test_cheats () =
  let distances = find_min_path () in
  let base_line = Hashtbl.find_exn distances !goal in
  Spice.debugf "Base min distance = %d" base_line;
  let count = ref 0 in
  for r = 0 to !rows - 1 do
    for c = 0 to !cols - 1 do
      (*
         If the point is a '#' and NOT on the border, try turning it into a valid space
         and compare the new distance to the baseline
      *)
      if r <> 0
         && r <> !rows - 1
         && c <> 0
         && c <> !cols - 1
         && Char.equal !map.(r).(c) '#'
      then (
        !map.(r).(c) <- '.';
        let new_distances = find_min_path () in
        let new_distance = Hashtbl.find_exn new_distances !goal in
        !map.(r).(c) <- '#';
        if new_distance < base_line
        then
          Spice.debugf "Cheat at %d,%d saves %d picoseconds" r c (base_line - new_distance);
        if base_line - new_distance >= 100 then Int.incr count)
    done
  done;
  !count
;;

(*
   For part 1 we need to do a shortest path search, but we can phase through one wall.
   We want to find the number of "cheats" or phases we can perform that net a shorter
   path than the base line.
*)
let _part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  create_matrix data;
  Spice.debugf "Parsed %d x %d matrix" (Array.length !map) (Array.length !map.(0));
  let good_cheats = test_cheats () in
  Spice.infof "Good cheats = %d" good_cheats
;;

(* part1 () *)

(*
   For part 2, instead of just phasing through 1 wall, we can now go thru at MOST 20.
   Instead of the brute force approach from before, the searching algorithm will
   likely need to change.
*)

let get_manhattan_distance pa pb = abs (pa.(0) - pb.(0)) + abs (pa.(1) - pb.(1))

let find_min_path_with_cheats savings =
  let temp = !start in
  let from_start = find_min_path () in
  let base_line = Hashtbl.find_exn from_start !goal in
  start := !goal;
  goal := temp;
  let from_end = find_min_path () in
  goal := !start;
  start := temp;
  let count = ref 0 in
  Hashtbl.iteri from_start ~f:(fun ~key:point ~data:start_dist ->
    Hashtbl.iteri from_end ~f:(fun ~key:next ~data:end_dist ->
      let dist = get_manhattan_distance point next in
      if dist <= 20
      then if start_dist + dist + end_dist <= base_line - savings then Int.incr count));
  !count
;;

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let data = Utils.read_file input in
  create_matrix data;
  Spice.debugf "Parsed %d x %d matrix" (Array.length !map) (Array.length !map.(0));
  let cheats = find_min_path_with_cheats 100 in
  Spice.infof "%d valid cheats" cheats;
  ()
;;

part2 ()
