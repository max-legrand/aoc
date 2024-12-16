open Base

let () = Spice.info "2024 Day 13"

(* let input = "bin/2024/day13/data/test.txt" *)
let input = "bin/2024/day13/data/puzzle.txt"

type equation =
  { a : float
  ; b : float
  ; c : float
  }

let button_regex = Re.Pcre.regexp {|Button [A|B]: X\+(\d+), Y\+(\d+)|}
let prize_regex = Re.Pcre.regexp {|Prize: X=(\d+), Y=(\d+)|}

(* Use Cramer's rule to solve a system of equations *)
let solve_system eq1 eq2 =
  let determineate = (eq1.a *. eq2.b) -. (eq1.b *. eq2.a) in
  let dx = (eq1.c *. eq2.b) -. (eq2.c *. eq1.b) in
  let dy = (eq1.a *. eq2.c) -. (eq2.a *. eq1.c) in
  dx /. determineate, dy /. determineate
;;

let is_whole_number (x : float) =
  let y = Float.round ~dir:`Up x in
  Float.equal x y
;;

let parse_chunk chunk ~prize_addition =
  let items = Str.split (Str.regexp_string "\n") chunk |> List.to_array in
  (* Item 0 is the button A, item 1 is the button B, item 2 is the prize *)
  let eq1 = ref { a = 0.0; b = 0.0; c = 0.0 } in
  let eq2 = ref { a = 0.0; b = 0.0; c = 0.0 } in
  items
  |> Array.iteri ~f:(fun idx item ->
    match idx with
    | 0 ->
      let matches = Re.all button_regex item in
      let match_value = List.nth_exn matches 0 in
      let x = Re.Group.get match_value 1 in
      let y = Re.Group.get match_value 2 in
      eq1 := { !eq1 with a = Float.of_string x };
      eq2 := { !eq2 with a = Float.of_string y }
    | 1 ->
      let matches = Re.all button_regex item in
      let match_value = List.nth_exn matches 0 in
      let x = Re.Group.get match_value 1 in
      let y = Re.Group.get match_value 2 in
      eq1 := { !eq1 with b = Float.of_string x };
      eq2 := { !eq2 with b = Float.of_string y }
    | 2 ->
      let matches = Re.all prize_regex item in
      let match_value = List.nth_exn matches 0 in
      let x = Re.Group.get match_value 1 in
      let y = Re.Group.get match_value 2 in
      eq1 := { !eq1 with c = Float.of_string x +. prize_addition };
      eq2 := { !eq2 with c = Float.of_string y +. prize_addition }
    | _ -> ());
  let a, b = solve_system !eq1 !eq2 in
  (* let a_cost = 3.0 in *)
  (* let b_cost = 1.0 in *)
  (* Int.of_float (((solution |> fst) *. a_cost) +. ((solution |> snd) *. b_cost)) *)
  Spice.debugf "Solution: %f, %f" a b;
  (* Check if a and b are both postivie whole numbers *)
  if is_whole_number a && is_whole_number b
  then
    if Float.compare prize_addition 0.0 = 0
       && (Float.compare a 100.0 > 0 || Float.compare b 100.0 > 0)
    then 0
    else Int.of_float ((a *. 3.0) +. (b *. 1.0))
  else 0
;;

(* For part 1, we need to solve a system of equations *)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let chunks = Str.split (Str.regexp_string "\n\n") data in
  let result =
    chunks
    |> List.fold ~init:0 ~f:(fun acc chunk -> acc + parse_chunk chunk ~prize_addition:0.0)
  in
  Spice.infof "Result: %d" result
;;

part1 ()

(* For part 2, we need to add 10000000000000 to the prize locations *)

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let data = Utils.read_file_single input in
  let chunks = Str.split (Str.regexp_string "\n\n") data in
  let result =
    chunks
    |> List.fold ~init:0 ~f:(fun acc chunk ->
      acc + parse_chunk chunk ~prize_addition:10000000000000.0)
  in
  Spice.infof "Result: %d" result
;;

part2 ()
