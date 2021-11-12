open! Core
open Import
open Dom_html

type t = keyboardEvent Js.t

module Keyboard_code = Keyboard_code

val key : t -> Keyboard_code.t
val ctrl : t -> bool
val alt : t -> bool
val shift : t -> bool
val meta : t -> bool

(** [match_modifiers] evaluates a [t]'s modifiers vs the function's
    arguments. If an argument is not specified then that modifier is not evaluated. *)
val match_modifiers : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> ?meta:bool -> t -> bool

val no_modifiers : t -> bool

val map
  :  t
  -> f:
       ([ `Ctrl of bool ]
        * [ `Alt of bool ]
        * [ `Shift of bool ]
        * [ `Meta of bool ]
        * Keyboard_code.t
        -> 'a)
  -> 'a
