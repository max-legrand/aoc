type level =
  | ERROR
  | WARN
  | INFO
  | DEBUG

val set_log_level : level -> unit
val info : string -> unit
val debug : string -> unit
val error : string -> unit
val warn : string -> unit
