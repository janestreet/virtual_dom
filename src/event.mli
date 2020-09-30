open Js_of_ocaml

module type Handler = sig
  module Action : sig
    type t
  end

  val handle : Action.t -> unit
end

module type Visibility_handler = sig
  val handle : unit -> unit
end

module type S = sig
  type action
  type t = private ..
  type t += C : action -> t

  val inject : action -> t
end

type t = Ui_event.t = ..

type t +=
  | Ignore (** [Ignore] events are dropped, so no handler is called *)
  | Viewport_changed
  (** [Viewport_changed] events are delivered to all visibility handlers  *)
  | Stop_propagation
  (** [Stop_propagation] prevents the underlying DOM event from propagating up to the
      parent elements *)
  | Prevent_default
  (** [Prevent_default] prevents the default browser action from occurring as a result
      of this event *)
  | Many of t list
  (** Allows one to represent a list of handlers, which will be individually dispatched
      to their respective handlers. This is so callbacks can return multiple events of
      whatever kind. *)

(** For registering a new handler and a corresponding new constructor of the Event.t
    type *)
module Define (Handler : Handler) : S with type action := Handler.Action.t and type t := t

(** For registering a handler for Viewport_changed events. Note that if this functor is
    called multiple times, each handler will see all of the events. *)
module Define_visibility (VH : Visibility_handler) : sig end

module Expert : sig
  (** [handle t] looks up the [Handler.handle] function in the table of [Define]d
      functions, unwraps the [Event.t] back into its underlying [Action.t], and applies
      the two.  This is only intended for internal use by this library, specifically by
      the attribute code. *)
  val handle : #Dom_html.event Js.t -> t -> unit

  (** [handle_non_dom_event_exn] is the same as [handle] except that it raises in any
      case that would have required the [#Dom_html.event Js.t]. In particular, this
      can be to feed Actions back to the system that are not triggered by events from
      the DOM and do not have a corresponding [#Dom_html.event Js.t]. *)
  val handle_non_dom_event_exn : t -> unit
end
