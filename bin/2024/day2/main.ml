Log.info "2024 Day 2"

(* let input = "bin/2024/day2/data/test.txt" *)
let input = "bin/2024/day2/data/puzzle.txt"

(*
   Part 1 has us parsing reports with different levels.

   Reports are newline differentiated while levels are
   space delimited on the same line.

   We need to find reports that have the levels all gradually increasing
   or gradually decreasing.
   - The levels are either all increasing or all decreasing.
   - Any two adjacent levels differ by AT LEAST one and AT MOST three.
*)

type direction =
  | Increasing
  | Decreasing

let is_safe (levels : int list) (dir : direction) : bool =
  let rec check (levels : int list) (last : int option) (dir : direction) =
    match levels with
    | [] -> true
    | hd :: tl ->
      (match last with
       | None -> check tl (Some hd) dir
       | Some last ->
         (match dir with
          | Increasing ->
            if hd - last >= 1 && hd - last < 4
            then check tl (Some hd) Increasing
            else false
          | Decreasing ->
            if last - hd >= 1 && last - hd < 4
            then check tl (Some hd) Decreasing
            else false))
  in
  check levels None dir
;;

let parse_report (line : string) : bool =
  let levels = String.split_on_char ' ' line |> List.map int_of_string in
  let is_report_safe = is_safe levels Increasing || is_safe levels Decreasing in
  if is_report_safe then Log.debug (Printf.sprintf "Report is safe: %s" line);
  is_report_safe
;;

let part1 () =
  let lines = Utils.read_file input in
  let res =
    lines |> List.fold_left (fun acc x -> if parse_report x then acc + 1 else acc) 0
  in
  Log.info (Printf.sprintf "Result: %d" res)
;;

part1 ()

(*
   Part 2 allows 1 non-conforming level to exist within a report.
*)

let parse_report_fault (line : string) : bool =
  let levels = String.split_on_char ' ' line |> List.map int_of_string in
  if is_safe levels Increasing || is_safe levels Decreasing
  then true
  else (
    (* Try removing each level to see if the report can be safe *)
    let rec try_removal i =
      if i >= List.length levels
      then false
      else (
        let new_list = List.filteri (fun j _ -> j <> i) levels in
        if is_safe new_list Increasing || is_safe new_list Decreasing
        then true
        else try_removal (i + 1))
    in
    try_removal 0)
;;

let part2 () =
  (* Log.set_log_level Log.DEBUG; *)
  let lines = Utils.read_file input in
  let res =
    lines |> List.fold_left (fun acc x -> if parse_report_fault x then acc + 1 else acc) 0
  in
  Log.info (Printf.sprintf "Result: %d" res)
;;

part2 ()
