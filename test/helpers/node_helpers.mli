open! Core_kernel
open! Js_of_ocaml

type element =
  { tag_name : string
  ; attributes : (string * string) list
  ; string_properties : (string * string) list
  ; styles : (string * string) list
  ; handlers : (string * Handler.t) list
  ; hooks : (string * Virtual_dom.Vdom.Attr.Expert.Extra.t) list
  ; key : string option
  ; children : t list
  }
[@@deriving sexp_of]

(** Roughly analogous to {!Vdom.Node.t}, but more easily inspectable and represented as a
    pure OCaml type. *)
and t =
  | Text of string
  | Element of element
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

(** When a hook-based attribute build from an event-returning function, this function will
    find the hook, extract the value, call that function with [arg], and schedule the
    resulting function. *)
val trigger_hook
  :  t
  -> type_id:('a -> Virtual_dom.Vdom.Event.t) Type_equal.Id.t
  -> name:string
  -> arg:'a
  -> unit

(** Given an element, this function attempts to retrieve a hook with the name [name], and
    the type-id from the hooks [For_testing] module. *)
val get_hook_value : t -> type_id:'a Type_equal.Id.t -> name:string -> 'a

module User_actions : sig
  (** Convenience functions for {!trigger}, closely modeling user interactions. *)

  val click_on : t -> unit
  val input_text : t -> text:string -> unit
end
