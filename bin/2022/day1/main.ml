(* Part 1 wants us to find the elf with the most calories. *)
(* Evles are split on the double new line. *)

let max_calories (input : string list) : int =
  let rec max_cal_aux (acc : int) (input : string list) (max : int) : int =
    match input with
    | [] -> if acc > max then acc else max
    | hd :: tl ->
      (match hd with
       | "" -> if acc > max then max_cal_aux 0 tl acc else max_cal_aux 0 tl max
       | _ -> max_cal_aux (acc + int_of_string hd) tl max)
  in
  max_cal_aux 0 input 0
;;

let update_maxes ~(acc : int) ~(max1 : int) ~(max2 : int) ~(max3 : int) =
  if acc >= max1
  then acc, max1, max2
  else if acc >= max2
  then max1, acc, max2
  else if acc >= max3
  then max1, max2, acc
  else max1, max2, max3
;;

let top_three (input : string list) : int =
  let rec max_cal_aux (acc : int) (input : string list) (maxes : int * int * int) : int =
    let update_current () =
      let max1, max2, max3 =
        update_maxes
          ~acc
          ~max1:(Core.fst3 maxes)
          ~max2:(Core.snd3 maxes)
          ~max3:(Core.trd3 maxes)
      in
      max1, max2, max3
    in
    match input with
    | [] ->
      let max1, max2, max3 = update_current () in
      max1 + max2 + max3
    | hd :: tl ->
      (match hd with
       | "" -> max_cal_aux 0 tl (update_current ())
       | _ -> max_cal_aux (acc + int_of_string hd) tl maxes)
  in
  max_cal_aux 0 input (0, 0, 0)
;;

let main () =
  let input = "bin/2022/day1/data/puzzle.txt" in
  let lines = Utils.read_file input in
  Spice.info (Printf.sprintf "max_calories=%d" (max_calories lines));
  Spice.info (Printf.sprintf "top_three=%d" (top_three lines))
;;

main ()
