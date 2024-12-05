Spice.info "2024 Day 3"

(* let input = "bin/2024/day3/data/test.txt" *)
let input = "bin/2024/day3/data/puzzle.txt"
let lines = Utils.read_file input

(*
   Part 1 has us parsing a string and extracting "valid" sequences from it.
*)

let parse_sequence (line : string) : int =
  let reg = Re.Pcre.regexp {|mul\((\d+),(\d+)\)|} in
  let matches = Re.all reg line in
  List.fold_left
    (fun acc m ->
      let a, b = Re.Group.get m 1, Re.Group.get m 2 in
      acc + (int_of_string a * int_of_string b))
    0
    matches
;;

let part1 () =
  let res = lines |> List.fold_left (fun acc line -> acc + parse_sequence line) 0 in
  Spice.info (Printf.sprintf "Result: %d" res)
;;

part1 ()

(* Part 2 has us enabling and disabling the use of the multiplication *)
let parse_with_on_off line state =
  let reg = Re.Pcre.regexp {|(?:mul\(([0-9]+),([0-9]+)\))|(?:do\(\))|(?:don't\(\))|} in
  let matches = Re.all reg line in
  let rec parse_sequence acc matches state =
    match matches with
    | [] -> acc, state
    | x :: xs ->
      let match_str = Re.Group.get x 0 in
      (match match_str with
       | "do()" -> parse_sequence acc xs true
       | "don't()" -> parse_sequence acc xs false
       | _ ->
         if state
         then (
           let num1 = int_of_string (Re.Group.get x 1) in
           let num2 = int_of_string (Re.Group.get x 2) in
           parse_sequence (acc + (num1 * num2)) xs state)
         else parse_sequence acc xs state)
  in
  let res, state = parse_sequence 0 matches state in
  res, state
;;

let part2 () =
  let rec parse_sequence acc lines state =
    match lines with
    | [] -> acc, state
    | line :: xs ->
      let res, state = parse_with_on_off line state in
      parse_sequence (acc + res) xs state
  in
  let res, _ = parse_sequence 0 lines true in
  Spice.info (Printf.sprintf "Result: %d" res)
;;

part2 ()
