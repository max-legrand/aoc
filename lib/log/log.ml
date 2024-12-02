open Base
open Unix

(** ANSI color codes *)
let cyan = "\027[0;36m"

let purple = "\027[0;35m"
let red = "\027[0;31m"
let yellow = "\027[0;33m"
let reset = "\027[0m"

type level =
  | ERROR
  | WARN
  | INFO
  | DEBUG

(** Global log level - Default is INFO *)
let log_level = ref INFO

let set_log_level level = log_level := level

let should_log msg_level =
  match !log_level, msg_level with
  | ERROR, ERROR -> true
  | WARN, ERROR | WARN, WARN -> true
  | INFO, ERROR | INFO, WARN | INFO, INFO -> true
  | DEBUG, _ -> true
  | _ -> false
;;

let level_to_string level =
  match level with
  | ERROR -> "ERROR"
  | WARN -> "WARN"
  | INFO -> "INFO"
  | DEBUG -> "DEBUG"
;;

(** Get timestamp*)
let get_timestamp () = Unix.localtime (Unix.time ())

(** Convert timestamp to string of the form YYYY-MM-DD HH:MM:SS *)
let timestamp_to_string (timestamp : Unix.tm) =
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (timestamp.tm_year + 1900)
    (timestamp.tm_mon + 1)
    timestamp.tm_mday
    timestamp.tm_hour
    timestamp.tm_min
    timestamp.tm_sec
;;

let _log_inner level msg =
  let color =
    match level with
    | ERROR -> red
    | WARN -> yellow
    | INFO -> cyan
    | DEBUG -> purple
  in
  let timestamp = timestamp_to_string (get_timestamp ()) in
  let formatted_msg =
    Printf.sprintf "%s%s [%s]:%s %s" color timestamp (level_to_string level) reset msg
  in
  match level with
  | ERROR -> Stdio.prerr_endline formatted_msg
  | _ -> Stdio.print_endline formatted_msg
;;

let info msg = if should_log INFO then _log_inner INFO msg
let debug msg = if should_log DEBUG then _log_inner DEBUG msg
let error msg = if should_log ERROR then _log_inner ERROR msg
let warn msg = if should_log WARN then _log_inner WARN msg
