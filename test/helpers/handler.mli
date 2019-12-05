open! Core_kernel
open! Js_of_ocaml

(** The type representing an event handler e.g. onclick, onkeydown, etc...
    These event handlers can be triggered, but take care, accessing any of
    the fields on the event object in the callback will cause an exception
    to be raised. *)
type t [@@deriving sexp_of]

(** Converts a [js_any] into a handler.  Throws an exception if the value
    isn't a javascript function. *)
val of_any_exn : Js.Unsafe.any -> name:string -> t

(** Triggers the provided event handler.

    Use the [extra_fields] parameter in order to put extra fields on the event.  This
    will let you simulate event handlers that read the event object's properties. *)
val trigger : ?extra_fields:(string * Js.Unsafe.any) list -> t -> unit
