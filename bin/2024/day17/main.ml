open Core

let () = Spice.info "2024 Day 17"

(* let input = "bin/2024/day17/data/smalltest.txt" *)
(* let input = "bin/2024/day17/data/test.txt" *)
(* let input = "bin/2024/day17/data/p2test.txt" *)

let input = "bin/2024/day17/data/puzzle.txt"

type opcode =
  | Adv
  | Bxl
  | Bst
  | Jnz
  | Bxc
  | Out
  | Bdv
  | Cdv
[@@deriving enum]

let opcode_to_string = function
  | Adv -> "Adv"
  | Bxl -> "Bxl"
  | Bst -> "Bst"
  | Jnz -> "Jnz"
  | Bxc -> "Bxc"
  | Out -> "Out"
  | Bdv -> "Bdv"
  | Cdv -> "Cdv"
;;

let get_combo_value operand regA regB regC =
  match operand with
  | x when x >= 0 && x < 4 -> x
  | 4 -> regA
  | 5 -> regB
  | 6 -> regC
  | 7 -> failwith "Invalid operand"
  | _ -> failwith "Invalid operand"
;;

let perform opcode operand regA regB regC pc =
  match opcode with
  | Adv ->
    (* Division *)
    let num = regA in
    let den = Int.pow 2 (get_combo_value operand regA regB regC) in
    num / den, regB, regC, pc, None
  | Bxl ->
    (* Bitwise Xor *)
    let xor_result = regB lxor operand in
    regA, xor_result, regC, pc, None
  | Bst ->
    (* Mod 8 reg b *)
    regA, get_combo_value operand regA regB regC % 8, regC, pc, None
  | Jnz ->
    (* Jump *)
    if regA = 0 then regA, regB, regC, pc, None else regA, regB, regC, operand, None
  | Bxc ->
    (* Bitwise Or *)
    let result = regB lxor regC in
    regA, result, regC, pc, None
  | Out ->
    let combo = get_combo_value operand regA regB regC % 8 in
    regA, regB, regC, pc, Some combo
  | Bdv ->
    let num = regA in
    let den = Int.pow 2 (get_combo_value operand regA regB regC) in
    regA, num / den, regC, pc, None
  | Cdv ->
    let num = regA in
    let den = Int.pow 2 (get_combo_value operand regA regB regC) in
    regA, regB, num / den, pc, None
;;

let run regA regB regC program =
  let items =
    String.split_on_chars ~on:[ ',' ] program
    |> Array.of_list
    |> Array.map ~f:Int.of_string
  in
  items |> Array.iter ~f:(fun x -> Spice.debugf "%d" x);
  let rec helper pc regA regB regC output =
    if pc >= Array.length items
    then output
    else (
      let opcode = opcode_of_enum items.(pc) |> Option.value_exn in
      let operand = items.(pc + 1) in
      Spice.debugf "pc: %d opcode: %s operand: %d" pc (opcode_to_string opcode) operand;
      let a, b, c, newpc, print = perform opcode operand regA regB regC pc in
      let new_ouptut =
        match print with
        | None -> output
        | Some x -> x :: output
      in
      Spice.debugf "a: %d b: %d c: %d" a b c;
      if newpc <> pc
      then helper newpc a b c new_ouptut
      else helper (pc + 2) a b c new_ouptut)
  in
  List.rev (helper 0 regA regB regC [])
;;

let parse_register line =
  Spice.debugf "line: %s" line;
  let items = Str.split_delim (Str.regexp_string ": ") line in
  List.nth_exn items 1 |> Int.of_string
;;

(* Part 1 requires us to run through some operations on a 3-bit computer *)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = List.rev (Utils.read_file input) in
  let regA = parse_register (List.nth_exn data 0) in
  let regB = parse_register (List.nth_exn data 1) in
  let regC = parse_register (List.nth_exn data 2) in
  let program_items = Str.split_delim (Str.regexp_string ": ") (List.nth_exn data 4) in
  let program = List.nth_exn program_items 1 in
  let output = run regA regB regC program |> List.map ~f:Int.to_string in
  let output_string = String.concat ~sep:"," output in
  Spice.infof "Output: %s" output_string
;;

part1 ()

(* Part 2 we need to find the value for Register A which causes the program to print a copy of itself *)

(* Test a value, match it to the end of the output, and then shift over by 3 bits if it matches *)
let solve a program =
  let original_program = program in
  let program = String.substr_replace_all ~pattern:"," ~with_:"" program in
  let rec helper a i =
    let output =
      run a 0 0 original_program |> List.map ~f:Int.to_string |> String.concat ~sep:""
    in
    if String.equal output program
    then a
    else if i = 0
            || String.equal
                 output
                 (String.sub ~pos:(String.length program - i) ~len:i program)
    then (
      let new_a = ref 0 in
      for new_i = 0 to 7 do
        let na = helper ((8 * a) + new_i) (i + 1) in
        if na > 0 && !new_a = 0 then new_a := na
      done;
      !new_a)
    else 0
  in
  helper a 0
;;

let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = List.rev (Utils.read_file input) in
  let program_items = Str.split_delim (Str.regexp_string ": ") (List.nth_exn data 4) in
  let program = List.nth_exn program_items 1 in
  let regA = solve 0 program in
  Spice.infof "regA: %d" regA
;;

part2 ()
