open Js_of_ocaml

include module type of struct
  include Ui_effect
end

module type Visibility_handler = sig
  val handle : unit -> unit
end

module type S = sig
  type action
  type 'a t

  val inject : action -> unit t
end

type 'a t +=
  | Viewport_changed
  (** [Viewport_changed] events are delivered to all visibility handlers  *)
  | Stop_propagation
  (** [Stop_propagation] prevents the underlying DOM event from propagating up to the
      parent elements *)
  | Stop_immediate_propagation
  (** [Stop_immediate_propagation] causes [sequence_as_sibling] to ignore next
      sequenced event. *)
  | Prevent_default
  (** [Prevent_default] prevents the default browser action from occurring as a result
      of this event *)

(** Sequences two events, but only if the first is neither
    [Stop_immediate_propagation] nor a [Many] which contains
    [Stop_immediate_propagation]. Use this instead of [Many] if combining events
    that are associated with the same source; for example, the motivation for
    this function is for merging Inputs of hooks.

    The second argument is a function that takes unit not because it is
    expected to be impure, but because often it is computed via some arbitrary
    handler function. Using hooks as an example, often the input to a hook has
    type ['a -> Effect.t]. To merge inputs [f] and [g], you might write

    {[
      fun x -> sequence_as_sibling (f x) (g x)
    ]}

    but this might unnecessarily call [g] if [f] returns something with
    [Stop_immediate_propagation]. Instead, using this API, you must write

    {[
      fun x -> sequence_as_sibling (f x) (fun () -> (g x))
    ]} *)
val sequence_as_sibling : unit t -> unless_stopped:(unit -> unit t) -> unit t

(** For registering a handler for Viewport_changed events. Note that if this functor is
    called multiple times, each handler will see all of the events. *)
module Define_visibility (VH : Visibility_handler) : sig end

module Expert : sig
  (** [handle t] looks up the [Handler.handle] function in the table of [Define]d
      functions, unwraps the [Effect.t] back into its underlying [Action.t], and applies
      the two.  This is only intended for internal use by this library, specifically by
      the attribute code. *)
  val handle : #Dom_html.event Js.t -> unit t -> unit

  (** [handle_non_dom_event_exn] is the same as [handle] except that it raises in any
      case that would have required the [#Dom_html.event Js.t]. In particular, this
      can be to feed Actions back to the system that are not triggered by events from
      the DOM and do not have a corresponding [#Dom_html.event Js.t]. *)
  val handle_non_dom_event_exn : unit t -> unit
end
