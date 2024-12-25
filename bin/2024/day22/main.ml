open Core
open Utils

let () = Spice.info "2024 Day 22"

(* let input = "bin/2024/day22/data/test.txt" *)
(* let input = "bin/2024/day22/data/smalltest.txt" *)
(* let input = "bin/2024/day22/data/test2.txt" *)
let input = "bin/2024/day22/data/puzzle.txt"
let mix a b = a lxor b
let prune a = a % 16777216

let calculate_secret secret =
  let step1 = secret * 64 |> mix secret |> prune in
  let step2 = step1 / 32 |> mix step1 |> prune in
  let step3 = step2 * 2048 |> mix step2 |> prune in
  step3
;;

(* For part 1 we need to apply a series of transformations to a set of numbers *)
let part1 () =
  let data = read_file input in
  let domains =
    data
    |> List.map ~f:(fun x ->
      Domain.spawn (fun () ->
        let secret = ref (Int.of_string x) in
        for _i = 1 to 2000 do
          secret := calculate_secret !secret
        done;
        !secret))
  in
  let results = List.map domains ~f:(fun domain -> Domain.join domain) in
  results |> List.iter ~f:(fun x -> Spice.debugf "Secret: %d" x);
  let sum = List.fold results ~init:0 ~f:(fun acc x -> acc + x) in
  Spice.infof "Result: %d" sum
;;

part1 ()

let _print_array array =
  String.concat ~sep:" | " (Array.to_list array |> List.map ~f:Int.to_string)
  |> Spice.debugf "%s"
;;

(* For part 2, instead of summing the final transformations, we need to determine a global stopping point based on differences in sequences. *)
let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = read_file input in
  let iters = 2000 in
  let ranges = Hashtbl.Poly.create () in
  data
  |> List.iter ~f:(fun x ->
    let secret = ref (Int.of_string x) in
    let visited = Hash_set.Poly.create () in
    let changes = ref [] in
    for _i = 1 to iters do
      let next = calculate_secret !secret in
      let diff = (next % 10) - (!secret % 10) in
      changes := diff :: !changes;
      if !changes |> List.length >= 4
      then (
        let key =
          List.take !changes 4
          |> List.rev
          |> List.map ~f:Int.to_string
          |> String.concat ~sep:","
        in
        if not (Hash_set.mem visited key)
        then (
          if not (Hashtbl.Poly.mem ranges key) then Hashtbl.add_exn ranges ~key ~data:[];
          let value = Hashtbl.find_exn ranges key in
          Hashtbl.set ranges ~key ~data:((next % 10) :: value);
          Hash_set.add visited key);
        changes := List.sub !changes ~pos:0 ~len:3);
      secret := next
    done);
  let max_value =
    Hashtbl.fold ranges ~init:0 ~f:(fun ~key ~data acc ->
      Spice.debugf
        "Key: %s Values: %s"
        key
        (List.map data ~f:Int.to_string |> String.concat ~sep:",");
      let sum = List.sum (module Int) data ~f:Fn.id in
      Int.max acc sum)
  in
  Spice.infof "Max value: %d" max_value
;;

part2 ()
