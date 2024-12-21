open Core

let () = Spice.info "2024 Day 21"
let input = "bin/2024/day21/data/test.txt"
(* let input = "bin/2024/day21/data/puzzle.txt" *)

let numberpad =
  [| [| '7'; '8'; '9' |]; [| '4'; '5'; '6' |]; [| '1'; '2'; '3' |]; [| '#'; '0'; 'A' |] |]
;;

let remotepad = [| [| '#'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let get_valid_moves grid pos =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let valid_moves = ref [] in
  let row, col = pos in
  if row > 0 && not (Char.equal grid.(row - 1).(col) '#')
  then valid_moves := (row - 1, col) :: !valid_moves;
  if row < height - 1 && not (Char.equal grid.(row + 1).(col) '#')
  then valid_moves := (row + 1, col) :: !valid_moves;
  if col > 0 && not (Char.equal grid.(row).(col - 1) '#')
  then valid_moves := (row, col - 1) :: !valid_moves;
  if col < width - 1 && not (Char.equal grid.(row).(col + 1) '#')
  then valid_moves := (row, col + 1) :: !valid_moves;
  !valid_moves
;;

let get_direction current next =
  let cr, cc = current in
  let nr, nc = next in
  if nr < cr then "^" else if nr > cr then "v" else if nc < cc then "<" else ">"
;;

let build_lookup grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let path_map = Hashtbl.Poly.create () in
  for start_row = 0 to height - 1 do
    for start_col = 0 to width - 1 do
      if not (Char.equal grid.(start_row).(start_col) '#')
      then (
        let start_char = grid.(start_row).(start_col) in
        let visited_at_length = Hashtbl.Poly.create () in
        (* Track visited positions at each path length *)
        let queue = Queue.create () in
        Queue.enqueue queue (start_row, start_col, "");
        Hashtbl.add_exn visited_at_length ~key:(start_row, start_col) ~data:0;
        while not (Queue.is_empty queue) do
          let current_row, current_column, path = Queue.dequeue_exn queue in
          let current_char = grid.(current_row).(current_column) in
          let current_length = String.length path in
          (* Add path to results if this is a destination *)
          if not (String.equal path "")
          then (
            let key = start_char, current_char in
            match Hashtbl.find path_map key with
            | None -> Hashtbl.add_exn path_map ~key ~data:[ path ]
            | Some path_list ->
              let prev_length = String.length (List.hd_exn path_list) in
              if current_length = prev_length
              then Hashtbl.set path_map ~key ~data:(path :: path_list)
              else if current_length < prev_length
              then Hashtbl.set path_map ~key ~data:[ path ]);
          (* Process neighbors *)
          List.iter
            (get_valid_moves grid (current_row, current_column))
            ~f:(fun next_pos ->
              let next_row, next_col = next_pos in
              match Hashtbl.find visited_at_length next_pos with
              | None ->
                (* Position never visited *)
                let new_length = current_length + 1 in
                Hashtbl.add_exn visited_at_length ~key:next_pos ~data:new_length;
                let new_path =
                  path ^ get_direction (current_row, current_column) (next_row, next_col)
                in
                Queue.enqueue queue (next_row, next_col, new_path)
              | Some prev_length when prev_length = current_length + 1 ->
                (* Position visited at same path length - add alternate path *)
                let new_path =
                  path ^ get_direction (current_row, current_column) (next_row, next_col)
                in
                Queue.enqueue queue (next_row, next_col, new_path)
              | Some prev_length when prev_length > current_length + 1 ->
                (* Found shorter path to this position *)
                let new_length = current_length + 1 in
                Hashtbl.set visited_at_length ~key:next_pos ~data:new_length;
                let new_path =
                  path ^ get_direction (current_row, current_column) (next_row, next_col)
                in
                Queue.enqueue queue (next_row, next_col, new_path)
              | _ ->
                (* Position visited with shorter path - ignore *)
                ())
        done)
    done
  done;
  path_map
;;

let parse_code numpad_lookup remote_lookup code_string =
  let code = code_string |> String.to_list in
  let rec path_helper lookup current acc_paths code =
    match code with
    | [] -> acc_paths
    | hd :: rest ->
      let key = current, hd in
      (match Hashtbl.find lookup key with
       | None ->
         (* If no path found, add 'A' to all accumulated paths *)
         let new_paths = List.map acc_paths ~f:(fun p -> p ^ "A") in
         path_helper lookup hd new_paths rest
       | Some paths ->
         (* For each accumulated path so far, append each possible new path *)
         let new_paths =
           List.concat_map acc_paths ~f:(fun acc_path ->
             List.map paths ~f:(fun new_path -> acc_path ^ new_path ^ "A"))
         in
         path_helper lookup hd new_paths rest)
  in
  let first_drone_paths = path_helper numpad_lookup 'A' [ "" ] code in
  let second_drone_paths =
    List.concat_map first_drone_paths ~f:(fun path ->
      path_helper remote_lookup 'A' [ "" ] (path |> String.to_list))
  in
  let user_paths =
    List.concat_map second_drone_paths ~f:(fun path ->
      path_helper remote_lookup 'A' [ "" ] (path |> String.to_list))
  in
  let path_lengths = List.map user_paths ~f:String.length in
  let min_length = List.min_elt path_lengths ~compare:Int.compare |> Option.value_exn in
  let code_number =
    String.sub code_string ~pos:0 ~len:(String.length code_string - 1) |> Int.of_string
  in
  code_number * min_length
;;

(*
   For part 1, we need to play a game of "telephone" where we get an input and determine the steps needed to recreate it
   in the shortest amount of movements possible. My thought process here was to build a mapping of the shortest path from each point to each goal
   and then generate the combinations until we reach the end of the chain.
*)
let _part1 () =
  Spice.set_log_level Spice.DEBUG;
  let data = Utils.read_file input in
  let lookup = build_lookup numberpad in
  let remote_lookup = build_lookup remotepad in
  let result =
    data
    |> List.fold ~init:0 ~f:(fun acc code -> acc + parse_code lookup remote_lookup code)
  in
  Spice.infof "Result = %d" result
;;

(*
   For part 2, we need to extend the chain to 25 passes. This will obviously explode our code so a new approach needs to be found
*)

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let _data = Utils.read_file input in
  let _lookup = build_lookup numberpad in
  let _remote_lookup = build_lookup remotepad in
  ()
;;

part2 ()
