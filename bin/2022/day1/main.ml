(* Part 1 wants us to find the elf with the most calories. *)
(* Evles are split on the double new line. *)

let main () =
  let input = "bin/2022/day1/data/puzzle.txt" in
  let lines = Utils.read_file input in
  let max_calories (input : string list) : int =
    let rec max_cal_aux (acc : int) (input : string list) (max : int) : int =
      match input with
      | [] -> if acc > max then acc else max
      | hd :: tl ->
        if String.compare hd "" = 0
        then if acc > max then max_cal_aux 0 tl acc else max_cal_aux 0 tl max
        else max_cal_aux (acc + int_of_string hd) tl max
    in
    max_cal_aux 0 input 0
  in
  Log.info (Printf.sprintf "max_calories=%d" (max_calories lines));
  let top_three (input : string list) : int =
    let rec max_cal_aux
      (acc : int)
      (input : string list)
      (max1 : int)
      (max2 : int)
      (max3 : int)
      : int
      =
      match input with
      | [] ->
        if acc >= max1
        then acc + max1 + max2
        else if acc >= max2
        then max1 + acc + max2
        else if acc >= max3
        then max1 + max2 + acc
        else max1 + max2 + max3
      | hd :: tl ->
        if String.compare hd "" = 0
        then
          if acc >= max1
          then max_cal_aux 0 tl acc max1 max2
          else if acc >= max2
          then max_cal_aux 0 tl max1 acc max2
          else if acc >= max3
          then max_cal_aux 0 tl max1 max2 acc
          else max_cal_aux 0 tl max1 max2 max3
        else max_cal_aux (acc + int_of_string hd) tl max1 max2 max3
    in
    max_cal_aux 0 input 0 0 0
  in
  Log.info (Printf.sprintf "top_three=%d" (top_three lines))
;;

main ()
