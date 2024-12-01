(*
   Playing RPS, but with a strategy guide.
   A = Rock, B = Paper, C = Scissors
   X = Rock, Y = Paper, Z = Scissors
   For Part 1, column 1 is what our opponent will play and column 2 is what we will play.
   Scores are determined based on what you threw + whether you won
   1 point for rock, 2 points for paper, 3 points for scissors.
   0 points for a loss, 3 for a tie, 6 for a win.
*)

(* let input = "bin/2022/day2/data/test.txt" *)
let input = "bin/2022/day2/data/puzzle.txt"

type rps =
  | Rock
  | Paper
  | Scissors

let score_for_hand = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
;;

let _rps_to_string = function
  | Rock -> "Rock"
  | Paper -> "Paper"
  | Scissors -> "Scissors"
;;

let rps_of_string = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "not implemented"
;;

(* Part 2 the second column is how the game needs to end - X is a loss, Y is a tie, Z is a win *)
type end_result =
  | Loss
  | Tie
  | Win

let score_for_end_result = function
  | Loss -> 0
  | Tie -> 3
  | Win -> 6
;;

let end_result_of_string = function
  | "X" -> Loss
  | "Y" -> Tie
  | "Z" -> Win
  | _ -> failwith "not implemented"
;;

(** Determine the result of a hand where the opponent's hand is first and your hand is second *)
let determine_result = function
  | Rock, Rock | Scissors, Scissors | Paper, Paper -> Tie
  | Rock, Scissors | Scissors, Paper | Paper, Rock -> Loss
  | Rock, Paper | Scissors, Rock | Paper, Scissors -> Win
;;

(** Determine what hand I need to play to get a desired result *)
let determine_my_hand = function
  | Rock, Loss -> Scissors
  | Rock, Tie -> Rock
  | Rock, Win -> Paper
  | Paper, Loss -> Rock
  | Paper, Tie -> Paper
  | Paper, Win -> Scissors
  | Scissors, Loss -> Paper
  | Scissors, Tie -> Scissors
  | Scissors, Win -> Rock
;;

let parse_line_pt1 (line : string) : int =
  match String.split_on_char ' ' line with
  | [ opp; yours ] ->
    let opp, yours = rps_of_string opp, rps_of_string yours in
    score_for_hand yours + score_for_end_result (determine_result (opp, yours))
  | _ -> failwith "Invalid input"
;;

let part1 () =
  let lines = Utils.read_file input in
  let total =
    lines |> List.map parse_line_pt1 |> List.fold_left (fun acc x -> acc + x) 0
  in
  Log.info (Printf.sprintf "Total: %d" total)
;;

part1 ()

let parse_line_pt2 (line : string) : int =
  match String.split_on_char ' ' line with
  | [ opp; outcome ] ->
    let opp, outcome = rps_of_string opp, end_result_of_string outcome in
    score_for_end_result outcome + score_for_hand (determine_my_hand (opp, outcome))
  | _ -> failwith "Invalid input"
;;

let part2 () =
  let lines = Utils.read_file input in
  let total = lines |> List.map parse_line_pt2 |> List.fold_left ( + ) 0 in
  Log.info (Printf.sprintf "Total: %d" total)
;;

part2 ()
