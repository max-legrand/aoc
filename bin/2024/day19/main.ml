open Core

let () = Spice.info "2024 Day 19"

(* let input = "bin/2024/day19/data/test.txt" *)
let input = "bin/2024/day19/data/puzzle.txt"

(*
   Part 1 we need to test whether a combination able to be constructed
   based on some inputs we are given.
*)

let split_list list split_idx =
  let rec helper acc idx =
    if idx = List.length list
    then List.rev acc
    else if idx = split_idx
    then List.rev acc
    else helper (List.nth_exn list idx :: acc) (idx + 1)
  in
  helper [] 0, helper [] (split_idx + 1)
;;

let build_lookup towels =
  let lookup = Hash_set.create (module String) in
  towels
  |> Str.split_delim (Str.regexp_string ", ")
  |> List.iter ~f:(fun x -> Hash_set.add lookup x);
  lookup
;;

let test_pattern pattern lookup =
  let pattern_array = String.to_array pattern in
  let rec helper current idx =
    if idx > String.length pattern
    then false
    else (
      let substring = String.sub ~pos:0 ~len:idx pattern in
      (* Test the current matches the pattern thus far *)
      if String.equal current pattern
      then true
      else if not (String.equal current substring)
      then false
      else (
        let current_character = pattern_array.(idx) in
        (* Find all possible matches *)
        let options =
          Hash_set.filter lookup ~f:(fun x ->
            if Char.equal (String.get x 0) current_character then true else false)
          |> Hash_set.to_array
        in
        let valid = ref false in
        let i = ref 0 in
        while !i < Array.length options do
          let pat = options.(!i) in
          if helper (current ^ pat) (idx + String.length pat)
          then (
            valid := true;
            i := Array.length options)
          else Int.incr i
        done;
        !valid))
  in
  helper "" 0
;;

let test_patterns patterns lookup =
  patterns
  |> List.fold ~init:0 ~f:(fun acc pat ->
    match test_pattern pat lookup with
    | true -> acc + 1
    | false -> acc)
;;

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file input in
  let towels, patterns = split_list data 1 in
  let towels = List.nth_exn towels 0 in
  let lookup = build_lookup towels in
  let number_valid = test_patterns patterns lookup in
  Spice.infof "Valid = %d" number_valid
;;

part1 ()

(*
   For part 2, we need to count how many possible combinations can produce valid
   patterns.
*)

let test_pattern_count pattern lookup =
  (* Try a DP approach instead of backtracking since we need to keep track of counts *)
  let len = String.length pattern in
  let dp = Array.create ~len:(len + 1) 0 in
  dp.(0) <- 1;
  for i = 0 to len - 1 do
    if dp.(i) > 0
    then
      Hash_set.iter lookup ~f:(fun pat ->
        let pat_len = String.length pat in
        if i + pat_len <= len
        then (
          let substring = String.sub ~pos:i ~len:pat_len pattern in
          if String.equal pat substring then dp.(i + pat_len) <- dp.(i + pat_len) + dp.(i)))
  done;
  dp.(len)
;;

let test_patterns_count patterns lookup =
  patterns
  |> List.fold ~init:0 ~f:(fun acc pat ->
    let number = test_pattern_count pat lookup in
    Spice.debugf "%s - %d" pat number;
    acc + number)
;;

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let data = Utils.read_file input in
  let towels, patterns = split_list data 1 in
  let towels = List.nth_exn towels 0 in
  let lookup = build_lookup towels in
  let number_valid = test_patterns_count patterns lookup in
  Spice.infof "Valid = %d" number_valid;
  ()
;;

part2 ()
