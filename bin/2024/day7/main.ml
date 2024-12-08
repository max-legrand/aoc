open Base

let () = Spice.info "2024 Day 7"

(* let input = "bin/2024/day7/data/test.txt" *)
let input = "bin/2024/day7/data/puzzle.txt"

(*
   For part 1 we need to determine if it is possible to achieve the sum value by applying
   either addition or multiplication between elements.
*)
let dec_to_bin (dec : int) =
  let rec d2b y lst =
    match y with
    | 0 -> lst
    | _ -> d2b (y / 2) ((y % 2) :: lst)
  in
  d2b dec []
;;

let dec_to_tri (dec : int) =
  let rec d2t y lst =
    match y with
    | 0 -> lst
    | _ -> d2t (y / 3) ((y % 3) :: lst)
  in
  d2t dec []
;;

let pad_to_length ~(target_length : int) ~bin =
  let rec helper ~(acc : int list) =
    if List.length acc = target_length then acc else helper ~acc:(0 :: acc)
  in
  helper ~acc:bin
;;

let generate_combinations max_number ~is_tri =
  let total =
    match is_tri with
    | false -> 2 ** max_number
    | true -> 3 ** max_number
  in
  let rec helper ~(acc : int list list) ~(current : int) =
    if current >= total
    then acc
    else (
      let values =
        match is_tri with
        | false -> pad_to_length ~target_length:max_number ~bin:(dec_to_bin current)
        | true -> pad_to_length ~target_length:max_number ~bin:(dec_to_tri current)
      in
      helper ~acc:(values :: acc) ~current:(current + 1))
  in
  helper ~acc:[] ~current:0
;;

let process_line (line : string) ~is_tri =
  let idx =
    match Base.String.substr_index ~pos:0 line ~pattern:": " with
    | Some x -> x
    | None -> failwith "Invalid line"
  in
  let items =
    [ String.sub line ~pos:0 ~len:idx
    ; String.sub line ~pos:(idx + 2) ~len:(String.length line - idx - 2)
    ]
  in
  let total = Int.of_string (List.nth_exn items 0) in
  let numbers =
    List.nth_exn items 1
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
    |> Array.of_list
  in
  Spice.debugf
    "total=%d - %s"
    total
    (String.concat ~sep:"," (List.of_array numbers |> List.map ~f:Int.to_string));
  let combinations = generate_combinations (Array.length numbers - 1) ~is_tri in
  (* combinations *)
  (* |> List.iter ~f:(fun combination -> *)
  (*   Stdio.print_string "["; *)
  (*   combination *)
  (*   |> List.iter ~f:(fun item -> *)
  (*     Stdio.printf "%d" item; *)
  (*     Stdio.print_string ","); *)
  (*   Stdio.print_string "]\n"); *)
  match
    List.fold combinations ~init:false ~f:(fun acc item ->
      let result =
        List.foldi item ~init:numbers.(0) ~f:(fun idx acc2 item ->
          match item with
          | 0 -> acc2 * numbers.(idx + 1)
          | 1 -> acc2 + numbers.(idx + 1)
          | 2 ->
            let current_string = Int.to_string acc2 in
            let new_string = Int.to_string numbers.(idx + 1) in
            let new_val = String.concat ~sep:"" [ current_string; new_string ] in
            Int.of_string new_val
          | _ -> failwith "Invalid value")
      in
      if result = total then true else acc)
  with
  | true -> total
  | false -> 0
;;

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let value =
    List.fold ~init:0 ~f:(fun acc line -> acc + process_line line ~is_tri:false) data
  in
  Spice.infof "Result=%d" value;
  ()
;;

part1 ()

(*
   Part 2 has added a new operation available, which means our approach of
   using binary to represent the choices no longer works
*)

let part2 () =
  let data = Utils.read_file input in
  let value =
    List.fold ~init:0 ~f:(fun acc line -> acc + process_line line ~is_tri:true) data
  in
  Spice.infof "Result=%d" value
;;

part2 ()
