open Core
open Utils

let () = Spice.info "2024 Day 23"

(* let input = "bin/2024/day23/data/smalltest.txt" *)
(* let input = "bin/2024/day23/data/test.txt" *)
let input = "bin/2024/day23/data/puzzle.txt"

let parse_adj_list text =
  let lines = String.split_lines text in
  let adj = Hashtbl.Poly.create () in
  lines
  |> List.iter ~f:(fun line ->
    let items = line |> String.split ~on:'-' in
    let from, to_ = List.nth_exn items 0, List.nth_exn items 1 in
    if not (Hashtbl.Poly.mem adj from) then Hashtbl.Poly.add_exn adj ~key:from ~data:[];
    if not (Hashtbl.Poly.mem adj to_) then Hashtbl.Poly.add_exn adj ~key:to_ ~data:[];
    let from_value = Hashtbl.Poly.find_exn adj from in
    let to_value = Hashtbl.Poly.find_exn adj to_ in
    Hashtbl.Poly.set adj ~key:from ~data:(to_ :: from_value);
    Hashtbl.Poly.set adj ~key:to_ ~data:(from :: to_value));
  adj
;;

let contains item list =
  let rec helper list =
    match list with
    | [] -> false
    | hd :: tl -> if String.equal hd item then true else helper tl
  in
  helper list
;;

let find_triplets (adj : (string, string list) Base.Hashtbl.t) =
  let triplets = Hash_set.Poly.create () in
  let seen = Hash_set.Poly.create () in
  Hashtbl.Poly.iteri adj ~f:(fun ~key ~data ->
    if List.length data >= 2
    then (
      Spice.debugf "Key: %s" key;
      data
      |> List.iter ~f:(fun child ->
        if not (Hash_set.mem seen child)
        then (
          Spice.debugf "Child: %s" child;
          Hashtbl.find_exn adj child
          |> List.iter ~f:(fun grandchild ->
            Spice.debugf "Grandchild: %s" grandchild;
            if Hashtbl.find_exn adj grandchild |> contains key
            then (
              (* Triplet found! *)
              let triplet_sorted =
                [ key; child; grandchild ] |> List.sort ~compare:String.compare
              in
              Hash_set.add triplets triplet_sorted)))));
    Hash_set.add seen key);
  triplets
;;

(*
   For part 1 we need to find groups of 3 connected nodes based on an adjacency list.
*)
let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = read_file_single input in
  let adj = parse_adj_list data in
  Hashtbl.Poly.iteri adj ~f:(fun ~key ~data ->
    Spice.debugf "Key: %s Values: %s" key (data |> print_list_string));
  let triplets = find_triplets adj in
  Hash_set.iter triplets ~f:(fun triplet ->
    print_list_string triplet |> Spice.debugf "%s");
  let filtered =
    Hash_set.filter triplets ~f:(fun triplet ->
      List.fold ~init:false triplet ~f:(fun acc item ->
        if Array.get (item |> String.to_array) 0 |> Char.equal 't' then true else acc))
  in
  Spice.infof "Triplets w/ computer starting with t = %d" (Hash_set.length filtered)
;;

part1 ()

(*
   For part 2 instead of just finding groups of 3, we need to find a set of items that are ALL grouped together.
*)

(* Return the neighbors of a vertex *)
let neighbors adj v =
  match Hashtbl.find adj v with
  | None -> Hash_set.Poly.create ()
  | Some vs -> Hash_set.Poly.of_list vs
;;

let rec bron_kerbosch ~adj ~r ~p ~x ~all =
  if Hash_set.is_empty p && Hash_set.is_empty x
  then (
    let clique = Hash_set.to_list r |> List.sort ~compare:String.compare in
    all := clique :: !all)
  else (
    let p_list = Hash_set.to_list p in
    p_list
    |> List.iter ~f:(fun v ->
      let next_v = neighbors adj v in
      let p' = Hash_set.copy p in
      Hash_set.filter_inplace p' ~f:(fun u -> Hash_set.mem next_v u);
      let x' = Hash_set.copy x in
      Hash_set.filter_inplace x' ~f:(fun u -> Hash_set.mem next_v u);
      Hash_set.add r v;
      bron_kerbosch ~adj ~r ~p:p' ~x:x' ~all;
      Hash_set.remove r v;
      Hash_set.remove p v;
      Hash_set.remove x v))
;;

let part2 () =
  Spice.set_log_level Spice.DEBUG;
  let data = read_file_single input in
  let adj = parse_adj_list data in
  let all_vertices = Hashtbl.keys adj |> Hash_set.Poly.of_list in
  let r = Hash_set.Poly.create () in
  let p = Hash_set.copy all_vertices in
  let x = Hash_set.Poly.create () in
  let all = ref [] in
  bron_kerbosch ~adj ~r ~p ~x ~all;
  let largest =
    List.fold !all ~init:[] ~f:(fun acc clique ->
      if List.length clique > List.length acc then clique else acc)
  in
  Spice.infof "Largest clique: %s" (String.concat ~sep:"," largest)
;;

part2 ()
