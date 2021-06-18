open Core

val validate_value : string -> unit Or_error.t
val parse_declaration_list : string -> (string * string) list Or_error.t
