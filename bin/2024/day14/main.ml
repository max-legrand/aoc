open Base

let () = Spice.info "2024 Day 14"
(* let input = "bin/2024/day14/data/test.txt" *)
(* let rows = 7 *)
(* let cols = 11 *)

let input = "bin/2024/day14/data/puzzle.txt"
let rows = 103
let cols = 101

let parse_line line =
  let items = Str.split (Str.regexp_string " ") line in
  let location_item = List.nth_exn items 0 in
  let velocity_item = List.nth_exn items 1 in
  let location_string =
    String.substr_replace_first ~pos:0 location_item ~pattern:"p=" ~with_:""
  in
  let location =
    String.split location_string ~on:',' |> List.map ~f:Int.of_string |> List.to_array
  in
  let velocity_string =
    String.substr_replace_first ~pos:0 velocity_item ~pattern:"v=" ~with_:""
  in
  let velocity =
    String.split velocity_string ~on:',' |> List.map ~f:Int.of_string |> List.to_array
  in
  location, velocity
;;

let simulate_points points seconds =
  let locations = Hashtbl.Poly.create () in
  points
  |> List.iter ~f:(fun (location, velocity) ->
    Spice.debug "=====================";
    Spice.debugf "p=%d,%d v=%d,%d" location.(0) location.(1) velocity.(0) velocity.(1);
    (* For each point, calculate the end location by multiplying the velocity by the seconds *)
    (* Then, use modulo to wrap the location around the grid *)
    let movement = velocity |> Array.map ~f:(fun x -> x * seconds) in
    Spice.debugf "movement -> %d,%d" movement.(0) movement.(1);
    let end_location =
      [ location.(0) + movement.(0); location.(1) + movement.(1) ] |> List.to_array
    in
    end_location.(0) <- end_location.(0) % cols;
    end_location.(1) <- end_location.(1) % rows;
    Spice.debugf "end_location -> p=%d,%d" end_location.(0) end_location.(1);
    if Hashtbl.Poly.mem locations end_location
    then
      Hashtbl.Poly.set
        locations
        ~key:end_location
        ~data:(Hashtbl.Poly.find_exn locations end_location + 1)
    else Hashtbl.Poly.add_exn locations ~key:end_location ~data:1);
  locations
;;

let get_quadrants () =
  let r = rows / 2 in
  let c = cols / 2 in
  let top_left = (0, 0), (c - 1, r - 1) in
  let top_right = (cols - c, 0), (cols - 1, r - 1) in
  let bottom_left = (0, rows - r), (c - 1, rows - 1) in
  let bottom_right = (cols - c, rows - r), (cols - 1, rows - 1) in
  [ top_left; top_right; bottom_left; bottom_right ]
;;

(*
   For part 1 we need to simulate movement of robots in a grid.
*)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let points = data |> List.map ~f:parse_line in
  let results = simulate_points points 100 in
  Spice.debug "-----------------------";
  Hashtbl.Poly.iteri results ~f:(fun ~key ~data ->
    let location = key in
    let count = data in
    Spice.debugf "p=%d,%d - %d" location.(0) location.(1) count);
  let quadrants = get_quadrants () in
  quadrants
  |> List.iter ~f:(fun (tl, br) ->
    Spice.debugf
      "quadrant spans from %d,%d to %d,%d"
      (tl |> fst)
      (tl |> snd)
      (br |> fst)
      (br |> snd));
  let counts =
    quadrants
    |> List.fold ~init:1 ~f:(fun safety_factor ((y1, x1), (y2, x2)) ->
      safety_factor
      * Hashtbl.Poly.fold ~init:0 results ~f:(fun ~key:location ~data:count acc ->
        if location.(0) >= y1
           && location.(0) <= y2
           && location.(1) >= x1
           && location.(1) <= x2
        then acc + count
        else acc))
  in
  Spice.infof "Result: %d" counts
;;

part1 ()

(*
   For part 2, we need to see how long it takes for the robots to arrange themselves
   in a christmas tree arrangement. There is a sneaky trick where when the christmas
   tree is arranged, there will be NO overlapping locations.
   We can use this to our advantage to find the minimum time it takes
   for the robots to arrange themselves.
*)

let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let points = data |> List.map ~f:parse_line in
  let total = List.length points in
  let rec helper iter =
    let results = simulate_points points iter in
    if Hashtbl.Poly.length results = total then iter else helper (iter + 1)
  in
  let min_time = helper 1 in
  Spice.infof "min_time: %d" min_time
;;

part2 ()
