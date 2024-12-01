Log.info "2022 Day 3"

(* let input = "bin/2022/day3/data/test.txt" *)
let input = "bin/2022/day3/data/puzzle.txt"

(* For part 1, we need to to find the items that appear in BOTH compartments of a rucksack.
   Rucksacks have two compartments which have equal amount of items.
   After identifying the contents that are the same, we must convert them to a numeric value and sum all the
   similar items together.
*)

let get_value_from_char (x : char) : int =
  if int_of_char x >= int_of_char 'A' && int_of_char x <= int_of_char 'Z'
  then int_of_char x - int_of_char 'A' + 27
  else int_of_char x - int_of_char 'a' + 1
;;

let parse_rucksack (line : string) : int =
  let length = String.length line in
  let compartment1 = String.sub line 0 (length / 2) in
  let compartment2 = String.sub line (length / 2) (length / 2) in
  (* Sort the compartments *)
  let chars1 = compartment1 |> String.to_seq |> List.of_seq |> List.sort compare in
  let chars2 = compartment2 |> String.to_seq |> List.of_seq |> List.sort compare in
  let rec compare_lists (acc : int) (l1 : char list) (l2 : char list) : int =
    match l1, l2 with
    | [], [] -> acc
    | x :: xs, y :: ys ->
      let res = compare x y in
      (match res with
       | 0 -> get_value_from_char x
       | z when z < 0 -> compare_lists acc xs l2
       | z when z > 0 -> compare_lists acc l1 ys
       | _ -> failwith "Not possible")
    | _ -> failwith "No similar items found"
  in
  compare_lists 0 chars1 chars2
;;

let part1 () =
  let lines = Utils.read_file input in
  let result = lines |> List.fold_left (fun acc x -> acc + parse_rucksack x) 0 in
  Log.info (Printf.sprintf "Result=%d" result)
;;

part1 ()

let parse_group_of_three (group : string list) : int =
  let compare_lists (l1 : char list) (l2 : char list) (l3 : char list) : int =
    let max_length = max (max (List.length l1) (List.length l2)) (List.length l3) in
    let freqs = Hashtbl.create max_length in
    l1 |> List.iter (fun x -> Hashtbl.replace freqs x 1);
    l2 |> List.iter (fun x -> if Hashtbl.mem freqs x then Hashtbl.replace freqs x 2);
    let result =
      l3
      |> List.fold_left
           (fun acc x ->
             if Hashtbl.mem freqs x
             then (
               let freq = Hashtbl.find freqs x in
               if freq = 2
               then (
                 Log.debug (Printf.sprintf "Similar_char=%c" x);
                 acc + get_value_from_char x)
               else acc)
             else acc)
           0
    in
    result
  in
  (* Get the sorted unique characters *)
  let chars1 =
    List.nth group 0 |> String.to_seq |> List.of_seq |> List.sort_uniq compare
  in
  let chars2 =
    List.nth group 1 |> String.to_seq |> List.of_seq |> List.sort_uniq compare
  in
  let chars3 =
    List.nth group 2 |> String.to_seq |> List.of_seq |> List.sort_uniq compare
  in
  let str1 = String.of_seq (List.to_seq chars1) in
  let str2 = String.of_seq (List.to_seq chars2) in
  let str3 = String.of_seq (List.to_seq chars3) in
  Log.debug (Printf.sprintf "Comparing %s, %s, %s" str1 str2 str3);
  compare_lists chars1 chars2 chars3
;;

(* For part 2, the rucksacks are put into groups of 3.
   We need to find the common element between all 3 packs.
*)
let part2 () =
  let lines = Utils.read_file input in
  (* Split this into groups *)
  let rec split_into_groups (acc : string list list) (lines : string list)
    : string list list
    =
    match lines with
    | [] -> acc
    | x :: y :: z :: xs -> split_into_groups (acc @ [ [ x; y; z ] ]) xs
    | _ -> failwith "Invalid input"
  in
  let groups = split_into_groups [] lines in
  let result = List.fold_left (fun acc x -> acc + parse_group_of_three x) 0 groups in
  Log.info (Printf.sprintf "Result=%d" result)
;;

part2 ()
