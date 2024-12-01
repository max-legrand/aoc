Log.info "2024 Day 1"

(* let input = "bin/2024/day1/data/test.txt" *)
let input = "bin/2024/day1/data/puzzle.txt"

let create_list (lines : string list) (col : int) : int list =
  lines
  |> List.map (fun x ->
    let items =
      String.split_on_char ' ' x
      |> List.filter (fun x ->
        match x with
        | "" -> false
        | _ -> true)
    in
    List.nth items col |> int_of_string)
;;

(* To solve part 1, we need to sort the two lists then get the diff of each element *)
let part1 () =
  let lines = Utils.read_file input in
  let list1 = create_list lines 0 in
  let list2 = create_list lines 1 in
  let sl1 = list1 |> List.sort compare in
  let sl2 = list2 |> List.sort compare in
  let distances =
    List.map2
      (fun x y ->
        let res = x - y in
        if res < 0 then res * -1 else res)
      sl1
      sl2
  in
  let result = List.fold_left ( + ) 0 distances in
  Log.info (Printf.sprintf "Result=%d" result)
;;

part1 ()

(* For part 2, we need to figure out how many times an item in the left appears in the right list *)
(* Sum each item in the (left * number of occurrences in the right) *)
let part2 () =
  let lines = Utils.read_file input in
  let list1 = create_list lines 0 in
  let list2 = create_list lines 1 in
  let freqs = Hashtbl.create (List.length list1) in
  list2
  |> List.iter (fun x ->
    if Hashtbl.mem freqs x
    then Hashtbl.replace freqs x (Hashtbl.find freqs x + 1)
    else Hashtbl.add freqs x 1);
  let result =
    list1
    |> List.fold_left
         (fun acc x ->
           let freq = Hashtbl.find_opt freqs x |> Option.value ~default:0 in
           acc + (freq * x))
         0
  in
  Log.info (Printf.sprintf "Result=%d" result)
;;

part2 ()
