(* Printing floats compatible with what javascript does *)

open Js_of_ocaml

(** Calls the [toString] method on the float *)
val to_js_string : float -> Js.js_string Js.t

(** Calls the [toFixed] method on the float
    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed *)
val to_js_string_fixed : int -> float -> Js.js_string Js.t

(** Calls the [toPrecision] method on the float
    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision *)
val to_js_string_precision : int -> float -> Js.js_string Js.t

(** Calls the [toExponential] method on the float
    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential *)
val to_js_string_exponential : float -> Js.js_string Js.t

val to_string : float -> string
val to_string_fixed : int -> float -> string
val to_string_precision : int -> float -> string
val to_string_exponential : float -> string
