open Core
open Utils

let () = Spice.info "2024 Day 24"
let input = "bin/2024/day24/data/test2.txt"
(* let input = "bin/2024/day24/data/puzzle.txt" *)

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
  Spice.debugf "reg1: %s - op: %s - reg2: %s - output: %s" reg1 op reg2 target;
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

let generate_x ~state = __generate_number ~state ~filter:'x'
let generate_y ~state = __generate_number ~state ~filter:'y'

let num_to_bin num =
  let rec helper acc num =
    match num with
    | 0 -> acc
    | x when x > 0 -> helper ((x % 2) :: acc) (x / 2)
    | _ -> failwith "Invalid number"
  in
  helper [] num
;;

let pad_list length list =
  let rec helper acc = if List.length acc = length then acc else helper (0 :: acc) in
  helper list
;;

let diff_idx l1 l2 =
  let zip = List.zip_exn l1 l2 in
  zip
  |> List.foldi ~init:[] ~f:(fun idx acc item ->
    let i1, i2 = item in
    if i1 <> i2 then idx :: acc else acc)
;;

let find_candidates diffs instructions_string =
  let instructions = String.split_lines instructions_string in
  let items = Hash_set.Poly.create () in
  diffs |> List.iter ~f:(fun idx -> Hash_set.Poly.add items (Printf.sprintf "z%02d" idx));
  let added = ref false in
  let seen = Hash_set.Poly.create () in
  let first = ref true in
  while !first || !added do
    first := false;
    added := false;
    instructions
    |> List.iter ~f:(fun line ->
      if Hash_set.Poly.mem seen line
      then ()
      else (
        let to_add_items = ref [] in
        items
        |> Hash_set.Poly.iter ~f:(fun item ->
          if String.is_substring line ~substring:item
          then (
            let reg = Re.Pcre.regexp {|(.*) (AND|OR|XOR) (.*) -> (.*)|} in
            let matches = Re.all reg line in
            assert (List.length matches = 1);
            let m = List.hd_exn matches in
            let reg1 = Re.Group.get m 1 in
            let reg2 = Re.Group.get m 3 in
            let target = Re.Group.get m 4 in
            let contains_target = Hash_set.mem items target in
            let target_start_char = Array.get (target |> String.to_array) 0 in
            Spice.debugf "target: %s" target;
            if (Char.equal target_start_char 'z' && contains_target)
            || not (Char.equal target_start_char 'z')
            then (
              to_add_items := List.append !to_add_items [ reg1; reg2; target ];
              added := true)));
        if List.length !to_add_items > 0
        then (
          Hash_set.Poly.add seen line;
          List.iter !to_add_items ~f:(fun item -> Hash_set.Poly.add items item))))
  done;
  seen
;;

type output_reg =
  { operation : string
  ; reg1 : string
  ; reg2 : string
  }

let build_adj instructions =
  let adj = Hashtbl.Poly.create () in
  List.iter (String.split_lines instructions) ~f:(fun line ->
    let reg = Re.Pcre.regexp {|(.*) (AND|OR|XOR) (.*) -> (.*)|} in
    let matches = Re.all reg line in
    assert (List.length matches = 1);
    let m = List.hd_exn matches in
    let reg1 = Re.Group.get m 1 in
    let reg2 = Re.Group.get m 3 in
    let op = Re.Group.get m 2 in
    let target = Re.Group.get m 4 in
    Hashtbl.Poly.add_exn adj ~key:target ~data:{ operation = op; reg1; reg2 });
  adj
;;

let part2 () =
  let data = read_file_single input in
  let state, instructions = init_state data in
  let x = generate_x ~state in
  let y = generate_y ~state in
  process_instructions ~state ~instructions;
  let z = generate_number ~state in
  Spice.infof "Expected; %d + %d = %d; got %d + %d = %d" x y (x + y) x y z;
  Spice.set_log_level Spice.DEBUG;
  let z_binary = num_to_bin z in
  let expected_z = num_to_bin (x + y) |> pad_list (List.length z_binary) in
  print_list (z_binary |> List.map ~f:Int.to_string);
  print_list (expected_z |> List.map ~f:Int.to_string);
  let diffs = diff_idx z_binary expected_z in
  print_list (diffs |> List.map ~f:Int.to_string);
  let candidates = find_candidates diffs instructions in
  Hash_set.Poly.iter candidates ~f:(fun candidate ->
    Spice.debugf "Candidate: %s" candidate)
  (* Printing out the candidates, we can see there are 3 instances of a Z bit not being set using XOR *)
;;

part2 ()
