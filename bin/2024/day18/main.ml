open Core

let () = Spice.info "2024 Day 18"
(* let input = "bin/2024/day18/data/test.txt" *)
(* let rows = 7 *)
(* let cols = 7 *)

let input = "bin/2024/day18/data/puzzle.txt"
let rows = 71
let cols = 71
let map = ref [||]
let start = [| 0; 0 |]
let goal = [| rows - 1; cols - 1 |]

let set_map data iters =
  data
  |> List.iteri ~f:(fun i line ->
    if i > iters
    then ()
    else (
      (* Set the point on the map *)
      let items = line |> String.split ~on:',' |> Array.of_list in
      let y, x = items.(0) |> Int.of_string, items.(1) |> Int.of_string in
      Spice.debugf "x: %d y: %d" x y;
      !map.(x).(y) <- '#'));
  ()
;;

let points_equal item1 item2 = Array.equal Int.equal item1 item2

let is_valid_point coord =
  if coord.(0) >= 0
     && coord.(0) < rows
     && coord.(1) >= 0
     && coord.(1) < cols
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
  Utils.PrioQ.add queue start 0;
  let distances = Hashtbl.Poly.create () in
  Hashtbl.Poly.set ~key:start ~data:0 distances;
  let rec process_queue () =
    match Utils.PrioQ.pop queue with
    | None -> Int.max_value
    | Some (current, cost) ->
      if points_equal current goal
      then cost
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

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = List.rev (Utils.read_file input) in
  map := Array.make_matrix ~dimx:rows ~dimy:cols '.';
  set_map data 1024;
  !map.(start.(0)).(start.(1)) <- '@';
  !map.(goal.(0)).(goal.(1)) <- '$';
  Utils.print_map !map;
  let cost = find_min_path () in
  Spice.infof "Cost: %d" cost
;;

part1 ()

(* For part 2 we need to find the point at which a path is NOT obtainable *)
let build_and_search data iters =
  map := Array.make_matrix ~dimx:rows ~dimy:cols '.';
  set_map data iters;
  !map.(start.(0)).(start.(1)) <- '@';
  !map.(goal.(0)).(goal.(1)) <- '$';
  Utils.print_map !map;
  let cost = find_min_path () in
  cost
;;

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let data = List.rev (Utils.read_file input) in
  let rec bin_search start end_ =
    if start >= end_
    then start
    else (
      let mid = (start + end_) / 2 in
      Spice.debugf "start: %d end: %d mid: %d" start end_ mid;
      let cost = build_and_search data mid in
      (* if the cost is the max integer then it's not possible to traverse the path *)
      if Int.equal cost Int.max_value
      then bin_search start mid
      else bin_search (mid + 1) end_)
  in
  let value = bin_search 0 (List.length data - 1) in
  Spice.infof "Value: %d - %s" value (List.nth_exn data value)
;;

part2 ()
