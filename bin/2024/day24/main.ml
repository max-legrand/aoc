open Core
open Utils

let () = Spice.info "2024 Day 24"

(* let input = "bin/2024/day24/data/test2.txt" *)
let input = "bin/2024/day24/data/puzzle.txt"

let init_state data =
  let sections = Str.split (Str.regexp_string "\n\n") data in
  let state_lines = List.nth_exn sections 0 in
  let instructions = List.nth_exn sections 1 in
  let state = Hashtbl.Poly.create () in
  state_lines
  |> String.split_lines
  |> List.iter ~f:(fun line ->
    let items = line |> Str.split (Str.regexp_string ": ") in
    let register = List.nth_exn items 0 in
    let value = List.nth_exn items 1 |> Int.of_string in
    Hashtbl.Poly.set state ~key:register ~data:value);
  state, instructions
;;

let get_result op a b =
  match op with
  | "AND" -> a land b
  | "OR" -> a lor b
  | "XOR" -> a lxor b
  | _ -> failwith "Invalid operation"
;;

let process_instruction ~instruction ~state =
  let reg = Re.Pcre.regexp {|(.*) (AND|OR|XOR) (.*) -> (.*)|} in
  let matches = Re.all reg instruction in
  assert (List.length matches = 1);
  let m = List.hd_exn matches in
  let reg1 = Re.Group.get m 1 in
  let op = Re.Group.get m 2 in
  let reg2 = Re.Group.get m 3 in
  let target = Re.Group.get m 4 in
  (* Spice.debugf "reg1: %s - op: %s - reg2: %s - output: %s" reg1 op reg2 target; *)
  let reg1_value = Hashtbl.Poly.find_exn state reg1 in
  let reg2_value = Hashtbl.Poly.find_exn state reg2 in
  let result = get_result op reg1_value reg2_value in
  Hashtbl.Poly.set state ~key:target ~data:result
;;

let process_instructions ~state ~instructions =
  let rec process_instructions instructions =
    match instructions with
    | [] -> ()
    | hd :: tl ->
      (try
         process_instruction ~instruction:hd ~state;
         process_instructions tl
       with
       | _ -> process_instructions (List.append tl [ hd ]))
  in
  process_instructions (String.split_lines instructions)
;;

let __generate_number ~state ~filter =
  (* First, get only the keys that start with 'z' *)
  let filtered =
    state
    |> Hashtbl.filter_keys ~f:(fun key -> Char.equal (key |> String.to_array).(0) filter)
  in
  let filtered_keys_sorted = Hashtbl.keys filtered |> List.sort ~compare:String.compare in
  let rec helper acc pow (nums : string list) =
    match nums with
    | [] -> acc
    | hd :: tl ->
      let value = Hashtbl.find_exn filtered hd in
      (match value with
       | 0 -> helper acc (pow + 1) tl
       | 1 -> helper (acc + Int.pow 2 pow) (pow + 1) tl
       | _ -> failwith "Invalid value")
  in
  helper 0 0 filtered_keys_sorted
;;

let generate_number ~state = __generate_number ~state ~filter:'z'

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = read_file_single input in
  let state, instructions = init_state data in
  Hashtbl.iteri state ~f:(fun ~key ~data -> Spice.debugf "Key: %s Value: %d" key data);
  process_instructions ~state ~instructions;
  let number = generate_number ~state in
  Spice.infof "Number: %d" number
;;

part1 ()

type operation =
  { operand1 : string
  ; operation : string
  ; operand2 : string
  ; result : string
  }

let parse_line line =
  let reg = Re.Pcre.regexp {|(.*) (AND|OR|XOR) (.*) -> (.*)|} in
  let matches = Re.all reg line in
  assert (List.length matches = 1);
  let m = List.hd_exn matches in
  let reg1 = Re.Group.get m 1 in
  let op = Re.Group.get m 2 in
  let reg2 = Re.Group.get m 3 in
  let target = Re.Group.get m 4 in
  { operand1 = reg1; operation = op; operand2 = reg2; result = target }
;;

let parse_operations input = input |> String.split_lines |> List.map ~f:parse_line

let find_highest_z operations =
  List.fold operations ~init:"z00" ~f:(fun acc op ->
    if not (Char.equal (op.result |> String.to_array).(0) 'z')
    then acc
    else (
      let acc_z_num =
        acc |> String.sub ~pos:1 ~len:(String.length acc - 1) |> Int.of_string
      in
      let op_z_num =
        op.result |> String.sub ~pos:1 ~len:(String.length op.result - 1) |> Int.of_string
      in
      if op_z_num > acc_z_num then op.result else acc))
;;

let not_xyz reg =
  let first_character = (String.to_array reg).(0) in
  (not (Char.equal first_character 'x'))
  && (not (Char.equal first_character 'y'))
  && not (Char.equal first_character 'z')
;;

let has_x00 reg1 reg2 = String.equal reg1 "x00" || String.equal reg2 "x00"

let part2 () =
  let data = read_file_single input in
  let _wires, instructions_string = init_state data in
  let operations = parse_operations instructions_string in
  let highest_z = find_highest_z operations in
  let wrong = Hash_set.Poly.create () in
  List.iter operations ~f:(fun op ->
    if Char.equal (op.result |> String.to_array).(0) 'z'
    && (not (String.equal op.operation "XOR"))
    && not (String.equal op.result highest_z)
    then Hash_set.add wrong op.result;
    if String.equal op.operation "XOR"
    && not_xyz op.operand1
    && not_xyz op.operand2
    && not_xyz op.result
    then Hash_set.add wrong op.result;
    if String.equal op.operation "AND" && not (has_x00 op.operand1 op.operand2)
    (* Changed this condition *)
    then
      List.iter operations ~f:(fun op2 ->
        if (String.equal op.result op2.operand1 || String.equal op.result op2.operand2)
        && not (String.equal op2.operation "OR")
           (* Changed this condition *)
        then Hash_set.add wrong op.result);
    if String.equal op.operation "XOR"
    then
      List.iter operations ~f:(fun op2 ->
        if (String.equal op.result op2.operand1 || String.equal op.result op2.operand2)
        && String.equal op2.operation "OR"
        then Hash_set.add wrong op.result));
  let result =
    String.concat ~sep:"," (Hash_set.to_list wrong |> List.sort ~compare:String.compare)
  in
  Spice.infof "Result: %s" result
;;

part2 ()
