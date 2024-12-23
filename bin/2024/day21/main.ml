open Core

let () = Spice.info "2024 Day 21"

(* let input = "bin/2024/day21/data/test.txt" *)
let input = "bin/2024/day21/data/puzzle.txt"

let numberpad =
  [| [| '7'; '8'; '9' |]; [| '4'; '5'; '6' |]; [| '1'; '2'; '3' |]; [| '#'; '0'; 'A' |] |]
;;

let remotepad = [| [| '#'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let point_is_valid point grid =
  let row, col = point in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  if row >= 0
  && row < rows
  && col >= 0
  && col < cols
  && not (Char.equal grid.(row).(col) '#')
  then true
  else false
;;

(* Create position mapping just like in Python *)
let build_positions keypad =
  let position = Hashtbl.Poly.create () in
  for r = 0 to Array.length keypad - 1 do
    for c = 0 to Array.length keypad.(0) - 1 do
      if not (Char.equal keypad.(r).(c) '#')
      then Hashtbl.add_exn position ~key:keypad.(r).(c) ~data:(r, c)
    done
  done;
  position
;;

(* Use BFS to create a mapping of character -> character where the value is the shortest paths available *)
let build_sequences position keypad =
  let sequences = Hashtbl.Poly.create () in
  Hashtbl.iteri position ~f:(fun ~key:from_char ~data:from_pos ->
    Hashtbl.iteri position ~f:(fun ~key:to_char ~data:_to_pos ->
      if Char.equal from_char to_char
      then Hashtbl.add_exn sequences ~key:(from_char, to_char) ~data:[ "A" ]
      else (
        let possibilities = ref [] in
        let queue = Queue.create () in
        Queue.enqueue queue (from_pos, "");
        let optimal = ref Int.max_value in
        let continue_search = ref true in
        while !continue_search && not (Queue.is_empty queue) do
          let pos, moves = Queue.dequeue_exn queue in
          let r, c = pos in
          let found_suboptimal = ref false in
          List.iter
            [ r - 1, c, "^"; r + 1, c, "v"; r, c - 1, "<"; r, c + 1, ">" ]
            ~f:(fun (nr, nc, nm) ->
              if not !found_suboptimal
              then
                if point_is_valid (nr, nc) keypad
                then
                  if Char.equal keypad.(nr).(nc) to_char
                  then
                    if !optimal < String.length moves + 1
                    then found_suboptimal := true
                    else (
                      optimal := String.length moves + 1;
                      possibilities := (moves ^ nm ^ "A") :: !possibilities)
                  else Queue.enqueue queue ((nr, nc), moves ^ nm));
          if !found_suboptimal then continue_search := false
        done;
        Hashtbl.add_exn sequences ~key:(from_char, to_char) ~data:!possibilities)));
  sequences
;;

let number_position = build_positions numberpad
let number_sequences = build_sequences number_position numberpad
let remote_position = build_positions remotepad
let remote_sequences = build_sequences remote_position remotepad

let list_product xs ys =
  List.concat_map xs ~f:(fun x -> List.map ys ~f:(fun y -> x @ [ y ]))
;;

let cartesian_product lists =
  match lists with
  | [] -> [ [] ]
  | first :: rest ->
    List.fold rest ~init:(List.map first ~f:(fun x -> [ x ])) ~f:list_product
;;

let solve code sequences =
  let code_list = String.to_list code in
  let code_list_minus_end =
    'A' :: code_list |> List.sub ~pos:0 ~len:(List.length code_list)
  in
  let zip_options = List.zip_exn code_list_minus_end code_list in
  let options =
    List.map zip_options ~f:(fun (a, b) -> Hashtbl.find_exn sequences (a, b))
  in
  cartesian_product options
;;

let calculate_score code length =
  let code_numer =
    String.sub code ~pos:0 ~len:(String.length code - 1) |> Int.of_string
  in
  code_numer * length
;;

(*
   For part 1, we need to play a game of "telephone" where we get an input and determine the steps needed to recreate it
   in the shortest amount of movements possible. My thought process here was to build a mapping of the shortest path from each point to each goal
   and then generate the combinations until we reach the end of the chain.
*)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let result =
    data
    |> List.fold ~init:0 ~f:(fun acc line ->
      let robot1 = List.map (solve line number_sequences) ~f:(String.concat ~sep:"") in
      Spice.debugf "Robot 1 options:";
      List.iter robot1 ~f:(fun option -> Spice.debugf "%s" option);
      let current = ref robot1 in
      let i = ref 0 in
      while !i < 2 do
        (* Generate all possible paths for this robot *)
        let next_paths =
          List.concat_map !current ~f:(fun path ->
            let results = solve path remote_sequences in
            List.map results ~f:(String.concat ~sep:""))
        in
        (* Find and filter for minimum length paths *)
        let min_len =
          List.min_elt next_paths ~compare:(fun a b ->
            Int.compare (String.length a) (String.length b))
          |> Option.value_exn
          |> String.length
        in
        let next_filtered =
          List.filter next_paths ~f:(fun x -> String.length x = min_len)
        in
        (* Debug output to track progress *)
        Spice.debugf "Robot %d options (%d):" (!i + 2) (List.length next_filtered);
        List.iter next_filtered ~f:(fun option -> Spice.debugf "%s" option);
        (* Update for next iteration *)
        current := next_filtered;
        Int.incr i
      done;
      acc + calculate_score line (String.length (List.nth_exn !current 0)))
  in
  Spice.infof "Result: %d" result
;;

part1 ()

let compute_length_cache = Hashtbl.Poly.create ()

let compute_length from_char to_char depth =
  match Hashtbl.find compute_length_cache (from_char, to_char, depth) with
  | Some result -> result
  | None -> if depth = 1 then Hashtbl.find_exn remote_sequences (from_char, to_char)
;;
(*
   For part 2, we need to extend the chain to 25 passes. This will obviously explode our code so a new approach needs to be found.
   A memoization approach will probably be necessary coupled with using DFS instead of BFS.
*)
