open! Core_kernel

module type Handler = sig
  module Action : sig
    type t
  end

  val handle : Action.t -> unit
end

module type S = sig
  type action
  type t = private ..
  type t += C : action -> t

  val inject : action -> t
end

module type Event = sig
  module type Handler = Handler
  module type S = S

  type t = ..
  type t += Ignore | Many of t list

  module Define (Handler : Handler) :
    S with type action := Handler.Action.t and type t := t

  module Expert : sig
    (** [handle t] looks up the [Handler.handle] function in the table of [Define]d
        functions, unwraps the [Event.t] back into its underlying [Action.t], and applies
        the two.  This is only intended for internal use by this library, specifically by
        the attribute code. *)
    val handle : t -> unit

    val handlers : (t -> unit) Int.Table.t
  end
end
