open Core
open Utils

let () = Spice.info "2024 Day 22"

(* let input = "bin/2024/day22/data/test.txt" *)
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

(* For part 2, instead of summing the final transformations, we need to determine a global stopping point based on differences in sequences. *)
let part2 () = ();;

part2 ()
