open Core

let () = Spice.info "2024 Day 16"

(* let input = "bin/2024/day16/data/test.txt" *)
(* let input = "bin/2024/day16/data/test2.txt" *)
let input = "bin/2024/day16/data/puzzle.txt"

let create_matrix data =
  let lines = Str.split (Str.regexp_string "\n") data in
  let array = lines |> List.map ~f:(fun x -> x |> String.to_array) in
  Array.rev (array |> Array.of_list)
;;

let dirs = [| [| 0; 1 |]; [| -1; 0 |]; [| 0; -1 |]; [| 1; 0 |] |]

type coord =
  { pos : int array
  ; dir : int (* Index into dirs array (0-3) *)
  }

let map = ref [||]
let set_map new_map = map := new_map

let adjs (current : coord) =
  let next = ref [] in
  let position = current.pos in
  (* Turn left *)
  next := (1000, { pos = position; dir = (current.dir - 1 + 4) % 4 }) :: !next;
  (* Turn right *)
  next := (1000, { pos = position; dir = (current.dir + 1) % 4 }) :: !next;
  (* Move forward *)
  let forward_dir = dirs.(current.dir) in
  let nx = position.(0) + forward_dir.(0) in
  let ny = position.(1) + forward_dir.(1) in
  if not (Char.equal !map.(nx).(ny) '#')
  then next := (1, { pos = [| nx; ny |]; dir = current.dir }) :: !next;
  !next
;;

let find_start_and_end () =
  let cols = Array.length !map.(0) in
  let rec helper start goal row col =
    match start, goal with
    | None, None | Some (_, _), None | None, Some (_, _) ->
      let nx, ny = if col + 1 < cols then row, col + 1 else row + 1, 0 in
      if Char.equal !map.(row).(col) 'S'
      then helper (Some (row, col)) goal nx ny
      else if Char.equal !map.(row).(col) 'E'
      then helper start (Some (row, col)) nx ny
      else helper start goal nx ny
    | Some (sx, sy), Some (gx, gy) -> [| sx; sy |], [| gx; gy |]
  in
  helper None None 0 0
;;

let points_equal item1 item2 = Array.equal Int.equal item1 item2

let get_min_path_cost start goal =
  let start_state = { pos = start; dir = 0 } in
  let queue = Utils.PrioQ.create () in
  Utils.PrioQ.add queue start_state 0;
  let distances = Hashtbl.Poly.create () in
  let from = Hashtbl.Poly.create () in
  let min_cost = ref Int.max_value in
  let found = ref false in
  while not (Utils.PrioQ.is_empty queue) do
    let current, cost = Utils.PrioQ.pop queue |> Option.value_exn in
    if points_equal current.pos goal
    then (
      found := true;
      min_cost := cost);
    List.iter (adjs current) ~f:(fun (dist, next) ->
      let prev_min_cost =
        Hashtbl.find_or_add distances next ~default:(fun () -> Int.max_value)
      in
      if dist + cost < prev_min_cost
      then (
        Hashtbl.set distances ~key:next ~data:(dist + cost);
        Utils.PrioQ.add queue next (dist + cost);
        let new_set = Hash_set.Poly.create () in
        Hash_set.Poly.add new_set current;
        Hashtbl.set from ~key:next ~data:new_set)
      else if dist + cost <= prev_min_cost
      then (
        let existing =
          Hashtbl.find_or_add from next ~default:(fun () -> Hash_set.Poly.create ())
        in
        Hash_set.Poly.add existing current;
        Hashtbl.set from ~key:next ~data:existing));
    if !found then queue := []
  done;
  !min_cost
;;

(*
   Part 1 has us finding the shortest path from a start point to a goal
*)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let mat = create_matrix data in
  set_map mat;
  Utils.print_map !map;
  let start, goal = find_start_and_end () in
  Spice.debugf "Parsed a %d x %d matrix" (Array.length mat) (Array.length mat.(0));
  Spice.debugf "Start: %d, %d Goal: %d, %d" start.(0) start.(1) goal.(0) goal.(1);
  let cost = get_min_path_cost start goal in
  Spice.infof "Goal score: %d" cost;
  ()
;;

part1 ()

let find_min_paths start goal =
  let start_state = { pos = start; dir = 0 } in
  let queue = Utils.PrioQ.create () in
  Utils.PrioQ.add queue start_state 0;
  let distances = Hashtbl.Poly.create () in
  let from = Hashtbl.Poly.create () in
  while not (Utils.PrioQ.is_empty queue) do
    let current, cost = Utils.PrioQ.pop queue |> Option.value_exn in
    List.iter (adjs current) ~f:(fun (dist, next) ->
      let prev_min_cost =
        Hashtbl.find_or_add distances next ~default:(fun () -> Int.max_value)
      in
      if dist + cost < prev_min_cost
      then (
        Hashtbl.set distances ~key:next ~data:(dist + cost);
        Utils.PrioQ.add queue next (dist + cost);
        let new_set = Hash_set.Poly.create () in
        Hash_set.Poly.add new_set current;
        Hashtbl.set from ~key:next ~data:new_set)
      else if dist + cost = prev_min_cost
      then (
        let existing =
          Hashtbl.find_or_add from next ~default:(fun () -> Hash_set.Poly.create ())
        in
        Hash_set.Poly.add existing current;
        Hashtbl.set from ~key:next ~data:existing))
  done;
  let stack = Stack.create () in
  let end_state = { pos = goal; dir = 1 } in
  Stack.push stack end_state;
  let nodes = Hash_set.Poly.create () in
  Hash_set.Poly.add nodes end_state;
  while not (Stack.is_empty stack) do
    let current = Stack.pop_exn stack in
    let adj = Hashtbl.find_exn from current in
    Hash_set.iter adj ~f:(fun node ->
      if not (Hash_set.Poly.mem nodes node)
      then (
        Hash_set.Poly.add nodes node;
        Stack.push stack node))
  done;
  (* Deduplicate based on pos *)
  let positions = Hash_set.Poly.create () in
  nodes |> Hash_set.Poly.iter ~f:(fun node -> Hash_set.Poly.add positions node.pos);
  positions
;;

(* For part 2, we need to count how many tiles are in ALL the paths with the minimum count *)
let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let mat = create_matrix data in
  set_map mat;
  Utils.print_map !map;
  let start, goal = find_start_and_end () in
  Spice.debugf "Parsed a %d x %d matrix" (Array.length mat) (Array.length mat.(0));
  Spice.debugf "Start: %d, %d Goal: %d, %d" start.(0) start.(1) goal.(0) goal.(1);
  let all_min_tiles = find_min_paths start goal in
  Spice.infof "Tiles: %d" (Hash_set.Poly.length all_min_tiles);
  Hash_set.Poly.iter all_min_tiles ~f:(fun pos -> mat.(pos.(0)).(pos.(1)) <- 'O');
  Utils.print_map mat;
  ()
;;

part2 ()
