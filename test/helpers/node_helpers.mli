open! Core_kernel
open! Js_of_ocaml

(** Roughly analogous to {!Vdom.Node.t}, but more easily inspectable and represented as a
    pure OCaml type. *)
type t =
  | Text of string
  | Element of
      { tag_name : string
      ; attributes : (string * string) list
      ; handlers : (string * Handler.t) list
      ; key : string option
      ; children : t list
      }
  | Widget of string
[@@deriving sexp_of]

val select : t -> selector:string -> t list
val select_one : t -> selector:string -> t option

(* This function currently Stack Overflows when compiled with Js_of_ocaml
   JavaScript because of limitations in the tail-call optimizer.
   INFO: https://github.com/aantron/markup.ml/issues/26
   INFO: https://ocsigen.org/js_of_ocaml/3.1.0/manual/tailcall *)
val to_string_html : t -> string
val unsafe_convert_exn : Virtual_dom.Vdom.Node.t -> t
