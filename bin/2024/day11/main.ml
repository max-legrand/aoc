open Base

let () = Spice.info "2024 Day 11"

(* let input = "bin/2024/day11/data/test.txt" *)
let input = "bin/2024/day11/data/puzzle.txt"

let print_rocks ~(rocks : int list) =
  rocks |> List.map ~f:Int.to_string |> String.concat ~sep:" "
;;

let print_rocks2 ~(rocks : (int * int) list) =
  rocks |> List.map ~f:(fun x -> fst x |> Int.to_string) |> String.concat ~sep:" "
;;

let parse_rocks ~(data : string) ~(stop : int) =
  let r = String.split data ~on:' ' |> List.map ~f:Int.of_string in
  let rec blink ~rocks ~iter stop =
    Spice.debugf "rocks @ blink %d: %s" iter (print_rocks ~rocks);
    match iter with
    | x when x = stop -> rocks
    | _ ->
      let new_rocks = ref [] in
      List.iter rocks ~f:(fun rock ->
        if rock = 0
        then new_rocks := 1 :: !new_rocks
        else if String.length (Int.to_string rock) % 2 = 0
        then (
          let rock_string = Int.to_string rock in
          let rock_string_len = String.length rock_string in
          let sub1 = String.sub rock_string ~pos:0 ~len:(rock_string_len / 2) in
          let sub2 =
            String.sub
              (Int.to_string rock)
              ~pos:(rock_string_len / 2)
              ~len:(rock_string_len / 2)
          in
          new_rocks := Int.of_string sub2 :: Int.of_string sub1 :: !new_rocks)
        else new_rocks := (rock * 2024) :: !new_rocks);
      blink ~rocks:(List.rev !new_rocks) ~iter:(iter + 1) stop
  in
  blink ~rocks:r ~iter:0 stop
;;

(* Part 1 has us performing iterations on a list of rocks *)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let value = parse_rocks ~data ~stop:25 |> List.length in
  Spice.infof "value: %d" value
;;

part1 ()

(*
   Part 2 is just running the code for more iterations
   -- We will need to optimize the approach that we took in part
   1 otherwise this will take too long to evaluate.
*)
let count_digits n =
  let rec loop count n = if n < 10 then count + 1 else loop (count + 1) (n / 10) in
  loop 0 (Int.abs n)
;;

let split_number n =
  let rec pow10 n = if n = 0 then 1 else 10 * pow10 (n - 1) in
  let digits = count_digits n in
  let half = digits / 2 in
  let divisor = pow10 half in
  n / divisor, n % divisor
;;

let process_stone stone =
  let digit_count = count_digits stone in
  if stone = 0
  then [ 1 ]
  else if digit_count % 2 = 0
  then (
    let left, right = split_number stone in
    [ left; right ])
  else [ stone * 2024 ]
;;

let simulate ~data ~iterations =
  let initial = String.split ~on:' ' data |> List.map ~f:Int.of_string in
  (* Group initial numbers and count their occurrences *)
  let init =
    List.fold initial ~init:[] ~f:(fun acc n ->
      match List.find acc ~f:(fun (x, _) -> x = n) with
      | Some (x, _c) ->
        List.map acc ~f:(fun (v, count) -> if v = x then v, count + 1 else v, count)
      | None -> (n, 1) :: acc)
  in
  let rec blink ~rocks ~iter stop =
    Spice.debugf "rocks @ blink %d: %s" iter (print_rocks2 ~rocks);
    match iter with
    | x when x = stop -> rocks
    | _ ->
      (* Process each rock and its count *)
      let new_rocks =
        List.fold rocks ~init:[] ~f:(fun acc (rock, count) ->
          let next_rocks = process_stone rock in
          (* For each resulting rock from the transformation *)
          List.fold next_rocks ~init:acc ~f:(fun inner_acc next_rock ->
            match List.find inner_acc ~f:(fun (x, _) -> x = next_rock) with
            | Some (x, _c) ->
              List.map inner_acc ~f:(fun (v, existing_count) ->
                if v = x then v, existing_count + count else v, existing_count)
            | None -> (next_rock, count) :: inner_acc))
      in
      blink ~rocks:new_rocks ~iter:(iter + 1) stop
  in
  (* Sum up all counts from the final state *)
  let final_rocks = blink ~rocks:init ~iter:0 iterations in
  List.fold final_rocks ~init:0 ~f:(fun acc (_, count) -> acc + count)
;;

let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let value = simulate ~data ~iterations:75 in
  Spice.infof "value: %d" value
;;

part2 ()
