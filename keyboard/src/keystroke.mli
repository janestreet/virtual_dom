open Core
open Js_of_ocaml

module Keyboard_code : sig
  type t = Dom_html.Keyboard_code.t
  [@@deriving sexp, compare, bin_io, hash, enumerate, equal]

  val of_event : Dom_html.keyboardEvent Js.t -> t

  (** [to_location] returns the location of the key on the keyboard.
      https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/location *)
  val to_location : t -> int

  (** [to_code_string] returns the value of [evt##.code] that this [Keyboard_code.t]
      represents; i.e. which physical key was pressed. Note that this may not account for
      character mappings.
      https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code *)
  val to_code_string : t -> string

  (** [to_key_code] returns the value of [evt##.keyCode] that this [Keyboard_code.t]
      represents. [keyCode] is deprecated in web standards.
      https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode *)
  val to_key_code : t -> int
end

type t [@@deriving sexp, compare, bin_io, hash, sexp_grammar]

include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t

val create : Keyboard_code.t -> ctrl:bool -> alt:bool -> shift:bool -> meta:bool -> t
val create' : ?ctrl:unit -> ?alt:unit -> ?shift:unit -> ?meta:unit -> Keyboard_code.t -> t
val key : t -> Keyboard_code.t
val ctrl : t -> bool
val alt : t -> bool
val shift : t -> bool
val meta : t -> bool
val of_event : Keyboard_event.t -> t
val to_string_hum : t -> string

(** The first string is either "Shift+" or "", depending on whether "Shift" needs to be
    displayed to indicate the keystrokes. The second is the keyboard code itself. For
    instance:

    - For '(', returns ["" , "("] instead of ["Shift+", "9"]
    - For 'A', returns ["Shift+" , "a"] *)
val shift_string_and_keyboard_code_string : t -> string * string

(** Same as [shift_string_and_keyboard_code_string], but indicates whether shift needs to
    be displayed in a variant rather than via "Shift+" vs "". *)
val shift_and_keyboard_code_string
  :  t
  -> [ `Display_shift | `Dont_display_shift ] * string
