open! Core_kernel
open! Js_of_ocaml

(** Roughly analogous to {!Vdom.Node.t}, but more easily inspectable and represented as a
    pure OCaml type. *)
type t =
  | Text of string
  | Element of
      { tag_name : string
      ; attributes : (string * string) list
      ; string_properties : (string * string) list
      ; handlers : (string * Handler.t) list
      ; key : string option
      ; children : t list
      }
  | Widget of Sexp.t
[@@deriving sexp_of]

val map : t -> f:(t -> [ `Continue | `Replace_with of t ]) -> t
val is_tag : tag:string -> t -> bool
val has_class : cls:string -> t -> bool
val select : t -> selector:string -> t list
val select_first : t -> selector:string -> t option
val select_first_exn : t -> selector:string -> t

(* This function currently Stack Overflows when compiled with Js_of_ocaml
   JavaScript because of limitations in the tail-call optimizer.
   INFO: https://github.com/aantron/markup.ml/issues/26
   INFO: https://ocsigen.org/js_of_ocaml/3.1.0/manual/tailcall *)
val to_string_html : t -> string
val unsafe_convert_exn : Virtual_dom.Vdom.Node.t -> t

val trigger
  :  ?extra_fields:(string * Js.Unsafe.any) list
  -> t
  -> event_name:string
  -> unit

module User_actions : sig
  (** Convenience functions for {!trigger}, closely modeling user interactions. *)

  val click_on : t -> unit
  val input_text : t -> text:string -> unit
end
