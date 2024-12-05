open Base

let () = Spice.info "2024 Day 5"

(* let input = "bin/2024/day5/data/test.txt" *)
let input = "bin/2024/day5/data/puzzle.txt"

(*
   Part 1 has us parsing a list of pages that need to be printed in a certain order.
   We must determine which orders are already correctly "sorted".
*)
exception Invalid_string

let split_on_double_newline str =
  let idx =
    match Base.String.substr_index ~pos:0 str ~pattern:"\n\n" with
    | Some i -> i
    | None -> raise Invalid_string
  in
  [ String.sub str ~pos:0 ~len:idx
  ; String.sub str ~pos:(idx + 2) ~len:(String.length str - idx - 2)
  ]
;;

let build_reverse_associations (orderings : string) =
  let map =
    Hashtbl.create
      ~growth_allowed:true
      ~size:(String.length orderings * 2)
      (module String)
  in
  orderings
  |> String.split_lines
  |> List.iter ~f:(fun pair ->
    let items = String.split pair ~on:'|' in
    let first = List.nth_exn items 0 in
    let second = List.nth_exn items 1 in
    match Hashtbl.find map second with
    | Some item ->
      let new_item = first :: item in
      Hashtbl.set map ~key:second ~data:new_item
    | None -> Hashtbl.set map ~key:second ~data:[ first ]);
  map
;;

let is_good_page ~page ~map =
  let seen = Hash_set.create (module String) in
  (* First convert the page into a hashet for easy lookup *)
  let page_items = String.split ~on:',' page in
  let all_items = Hash_set.of_list (module String) page_items in
  let valid =
    List.for_all page_items ~f:(fun item ->
      (* Check if the item is in the map *)
      Hash_set.add seen item;
      match Hashtbl.find map item with
      | None -> true
      | Some items ->
        (* Check if the item is in the list but NOT seen *)
        items
        |> List.for_all ~f:(fun mapitem ->
          match Hash_set.mem all_items mapitem, Hash_set.mem seen mapitem with
          | true, false -> false
          | _ -> true))
  in
  valid
;;

let count_good_pages ~map ~(pages : string list) =
  List.fold pages ~init:0 ~f:(fun acc page ->
    let valid = is_good_page ~page ~map in
    Spice.debugf "Page=%s - %b" page valid;
    let page_items = String.split ~on:',' page in
    let middle_idx = List.length page_items / 2 in
    let middle_element = Int.of_string (List.nth_exn page_items middle_idx) in
    match valid with
    | true -> acc + middle_element
    | false -> acc)
;;

let part1 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let items = data |> split_on_double_newline in
  let ordering = List.nth_exn items 0 in
  let map = build_reverse_associations ordering in
  Hashtbl.iteri map ~f:(fun ~key ~data ->
    let val_str = String.concat ~sep:"," data in
    Spice.debugf "k=%s v=%s" key val_str);
  let pages = List.nth_exn items 1 |> String.split_lines in
  let res = count_good_pages ~map ~pages in
  Spice.infof "Result=%d" res
;;

part1 ()

(*
   Part 2 we only care about the incorrectly ordered sets of pages.
   We need to sort them properly, then get the middle element.
*)

let sort_page ~(page : string) ~map =
  let page_items = String.split ~on:',' page in
  let all_items = Hash_set.of_list (module String) page_items in
  let current_map = Hashtbl.create ~size:(List.length page_items) (module String) in
  List.iter page_items ~f:(fun item ->
    match Hashtbl.find map item with
    | None -> Hashtbl.set current_map ~key:item ~data:[]
    | Some items ->
      let new_items = List.filter items ~f:(Hash_set.mem all_items) in
      Hashtbl.set current_map ~key:item ~data:new_items);
  page_items
  |> List.sort ~compare:(fun a b ->
    let a_len = Hashtbl.find_exn current_map a |> List.length in
    let b_len = Hashtbl.find_exn current_map b |> List.length in
    Int.compare a_len b_len)
;;

let count_bad_pages ~map ~(pages : string list) =
  List.fold pages ~init:0 ~f:(fun acc page ->
    let valid = is_good_page ~page ~map in
    match valid with
    | true -> acc
    | false ->
      Spice.debugf "Page=%s is invalid. Sorting." page;
      let sorted = sort_page ~page ~map in
      Spice.debugf "Sorted=%s" String.(concat ~sep:"," sorted);
      let middle_idx = List.length sorted / 2 in
      let middle_element = Int.of_string (List.nth_exn sorted middle_idx) in
      acc + middle_element)
;;

let part2 () =
  (* Spice.set_log_level Spice.DEBUG; *)
  let data = Utils.read_file_single input in
  let items = data |> split_on_double_newline in
  let ordering = List.nth_exn items 0 in
  let map = build_reverse_associations ordering in
  Hashtbl.iteri map ~f:(fun ~key ~data ->
    let val_str = String.concat ~sep:"," data in
    Spice.debugf "k=%s v=%s" key val_str);
  let pages = List.nth_exn items 1 |> String.split_lines in
  let res = count_bad_pages ~map ~pages in
  Spice.infof "Result=%d" res
;;

part2 ()
