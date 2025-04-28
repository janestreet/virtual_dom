open! Core
open! Js_of_ocaml

type element =
  { tag_name : string
  ; attributes : (string * string) list
  ; string_properties : (string * string) list
  ; bool_properties : (string * bool) list
  ; styles : (string * string) list
  ; handlers : (string * Handler.t) list
  ; hooks : (string * Virtual_dom.Vdom.Attr.Hooks.For_testing.Extra.t) list
  ; key : string option
  ; children : t list
  }
[@@deriving sexp_of]

(** Roughly analogous to {!Vdom.Node.t}, but more easily inspectable and represented as a
    pure OCaml type. *)
and t =
  | Text of string
  | Element of element
  | Widget
[@@deriving sexp_of]

val map : t -> f:(t -> [ `Continue | `Replace_with of t ]) -> t
val is_tag : tag:string -> t -> bool
val has_class : cls:string -> t -> bool
val select : t -> selector:string -> t list
val select_first : t -> selector:string -> t option
val select_first_exn : t -> selector:string -> t

val to_string_html
  :  ?filter_printed_attributes:(key:string -> data:string -> bool)
  -> ?censor_paths:bool
  -> ?censor_hash:bool
  -> ?path_censoring_message:string
  -> ?hash_censoring_message:string
  -> t
  -> string

val inner_text : t -> string
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
  -> type_id:'t Type_equal.Id.t
  -> name:string
  -> f:('t -> 'a -> unit Virtual_dom.Vdom.Effect.t)
  -> arg:'a
  -> unit

(** Given an element, this function attempts to retrieve a hook with the name [name], and
    the type-id from the hooks [For_testing] module.

    Raises if called on a Widget or Text node, or if a hook matching [name] was found, but
    the type id does not match. *)
val get_hook_value_opt : t -> type_id:'a Type_equal.Id.t -> name:string -> 'a option

(** Like [get_hook_value_opt], but also raises if the hook was not found. *)
val get_hook_value : t -> type_id:'a Type_equal.Id.t -> name:string -> 'a

module User_actions : sig
  (** Convenience functions for {!trigger}, closely modeling user interactions. *)

  val click_on
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?ctrl_key_down:bool
    -> ?alt_key_down:bool
    -> ?meta_key_down:bool
    -> t
    -> unit

  val submit_form : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val focus : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit

  val blur
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> ?related_target:t
    -> t
    -> unit

  val input_text
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> t
    -> text:string
    -> unit

  val input_files
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> t
    -> files:File.file Js.t list
    -> unit

  val keydown
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?ctrl_key_down:bool
    -> ?alt_key_down:bool
    -> ?meta_key_down:bool
    -> t
    -> key:Dom_html.Keyboard_code.t
    -> unit

  val set_checkbox
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?ctrl_key_down:bool
    -> ?alt_key_down:bool
    -> ?meta_key_down:bool
    -> t
    -> checked:bool
    -> unit

  val change
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> t
    -> value:string
    -> unit

  val drag : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val enter : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val leave : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val over : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val drop : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val end_ : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val mousemove : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit
  val mouseenter : ?extra_event_fields:(string * Js.Unsafe.any) list -> t -> unit

  val wheel
    :  ?extra_event_fields:(string * Js.Unsafe.any) list
    -> t
    -> delta_y:float
    -> unit
end

module Linter : sig
  module Rule : sig
    type t =
      | Undetectable_clickable_element
      | Invalid_tabindex
      | Event_handler_html_attribute
      | Duplicate_ids
      | Whitespace_in_id
      | Siblings_have_same_vdom_key
      | Unsafe_target_blank
      | Clickable_role_but_no_tabindex
      | Button_without_valid_type
  end

  module Severity : sig
    (** Some errors, if not addressed, can unexpectedly crash your app at runtime. For
        instance, if two sibling vdom nodes have the same key, vdom will throw an
        unrecoverable exception. *)
    type t =
      | Only_report_app_crashing_errors
      | Report_all_errors
  end

  val run
    :  ?expected_failures:Rule.t list
    -> ?min_severity:Severity.t
    -> t
    -> string option

  val print_report
    :  ?expected_failures:Rule.t list
    -> ?min_severity:Severity.t
    -> ?on_ok:(unit -> unit)
    -> t
    -> unit
end
