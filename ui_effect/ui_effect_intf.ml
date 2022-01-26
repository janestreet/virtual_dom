open! Core

module type Handler = sig
  module Action : sig
    type t
  end

  val handle : Action.t -> unit
end

module type Handler1 = sig
  module Action : sig
    type 'a t
  end

  val handle : 'a Action.t -> on_response:('a -> unit) -> unit
end

module type S = sig
  type action
  type _ t

  val inject : action -> unit t
end

module type S1 = sig
  type 'a action
  type _ t

  val inject : 'a action -> 'a t
end

module type Effect = sig
  module type Handler = Handler
  module type S = S

  (** ['a Effect.t] represents some computation of type ['a] that can be performed
      outside of the typical computational/incremental structure of a Bonsai program
      .
      Examples of this computation might be:

      - Calling an RPC and getting the result back
      - Running expensive computation on a web-worker thread
      - Requesting some information from the imperative "Start.Handle"-holding code

      If you have a value of type ['a Effect.t], you can schedule it to be run
      by calling [inject] and providing a function that will be called when
      the callback completes. *)
  type 'a t = ..

  type 'a t += Ignore : unit t | Many : unit t list -> unit t

  include Monad.S with type 'a t := 'a t

  (** An effect that never completes *)
  val never : 'a t

  (** If creating an effect could be expensive, you can
      wrap its construction in a lazy and pass it to this function so that
      its construction will be deferred until it's about to be evaluated. *)
  val lazy_ : 'a t Lazy.t -> 'a t

  (** Prints the sexp when scheduled. *)
  val print_s : Sexp.t -> unit t

  (** [of_sync_fun] is similar to [of_deferred_fun] but with a synchronous function
      instead of a deferred one.  This can be used for functions that are synchronous
      but side-effecting, or as a mock-function in tests that replace the usages of
      [of_deferred_fun] in the actual app.

      Note that, unlike [of_deferred_fun], the function must return immediately, so it's not
      possible to test the behaviour of tour app between calling the function and the effect
      becoming 'determined'. If you need to do this, see [of_svar] and
      [of_query_response_tracker] below.
  *)
  val of_sync_fun : ('query -> 'result) -> 'query -> 'result t

  module Define (Handler : Handler) :
    S with type action := Handler.Action.t and type 'a t := 'a t

  module Define1 (Handler : Handler1) :
    S1 with type 'a action := 'a Handler.Action.t and type 'a t := 'a t

  module Expert : sig
    (** [handle t] looks up the [Handler.handle] function in the table of [Define]d
        functions, unwraps the [Event.t] back into its underlying [Action.t], and applies
        the two.  This is only intended for internal use by this library, specifically by
        the attribute code. *)
    val handle : unit t -> unit

    (* We use this table for dispatching to the appropriate handler in an efficient way.  *)
    type hide = T : ('a t * ('a -> unit)) -> hide

    val handlers : (hide -> unit) Hashtbl.M(Int).t
    val of_fun : f:(callback:('a -> unit) -> unit) -> 'a t
  end

  module Private : sig
    module Callback : sig
      type 'a effect := 'a t
      type ('a, 'b) t

      val make : request:'a -> on_response:('b -> unit effect) -> ('a, 'b) t
      val request : ('a, 'b) t -> 'a
      val respond_to : ('a, 'b) t -> 'b -> unit effect
    end

    val make : request:'a -> evaluator:(('a, 'b) Callback.t -> unit t) -> 'b t
  end

  module For_testing : sig
    module Svar : sig
      (** You can think of an [Svar.t] as like an [Ivar.t] whose purpose is to allow us to
          implement [of_svar] below.

          (The difference between [Svar] and [Ivar] is that the former is synchronous. That
          is, when [fill_if_empty] is called, it will directly call all of the handlers rather
          than scheduling that they be called later. This semantics can be confusing to work
          with in large-scale programs, as it means the control flow of your application hops
          around a lot more. However, it does mean that you don't need a scheduler, so it's
          easier to implement.) *)

      type 'a t

      val create : unit -> 'a t
      val upon : 'a t -> ('a -> unit) -> unit
      val fill_if_empty : 'a t -> 'a -> unit
      val peek : 'a t -> 'a option
    end

    (** Create an effect from a function that returns an [Svar.t]. This is mostly useful in
        testing, to emulate a ['query -> 'result Deferred.t] function that does not return
        immediately. You may find [Query_response_tracker] a more convenient interface than
        using [of_svar] directly.
    *)
    val of_svar_fun : ('query -> 'result Svar.t) -> 'query -> 'result t

    module Query_response_tracker : sig
      (** [Query_response_tracker] is an interface designed to make [of_svar] more
          convenient to use. When the function returned by [of_query_response_tracker t] is
          called (typically by your bonsai app), the query passed is stored within [t]. Your
          test code can then call [maybe_respond] to cause those effects to 'become
          determined'. *)
      type ('q, 'r) t

      val create : unit -> _ t

      type 'r maybe_respond =
        | No_response_yet
        | Respond of 'r

      val maybe_respond : ('q, 'r) t -> f:('q -> 'r maybe_respond) -> unit
      val queries_pending_response : ('q, _) t -> 'q list
    end

    val of_query_response_tracker
      :  ('query, 'result) Query_response_tracker.t
      -> 'query
      -> 'result t
  end
end
