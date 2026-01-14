open! Base

module type Handler = sig
  module Action : sig
    type t
  end

  val handle : Action.t -> on_exn:(Exn.t -> unit) -> unit
end

module type Handler1 = sig
  module Action : sig
    type 'a t
  end

  val handle : 'a Action.t -> on_response:('a -> unit) -> on_exn:(Exn.t -> unit) -> unit
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

  (** ['a Effect.t] represents some computation of type ['a] that can be performed outside
      of the typical computational/incremental structure of a Bonsai program . Examples of
      this computation might be:

      - Calling an RPC and getting the result back
      - Running expensive computation on a web-worker thread
      - Requesting some information from the imperative "Start.Handle"-holding code

      If you have a value of type ['a Effect.t], you can schedule it to be run by calling
      [inject] and providing a function that will be called when the callback completes. *)
  type 'a t = ..

  type 'a t += Ignore : unit t | Many : unit t list -> unit t

  include Monad.S with type 'a t := 'a t

  (** The [Par] module contains a Let_syntax whose [both] function executes the effects in
      parallel.

      This means that if you have a [let%map] or [let%bind], then bindings separated by
      `and` will run at the same time instead of being sequenced. *)
  module Par : Monad.S with type 'a t := 'a t

  (** An effect that never completes *)
  val never : 'a t

  (** Produces an effect that raises an exception when the effect is performed.

      Note: As long as this is handled by either [try_with]/[try_with_or_error] or
      [Expert.handle]/[Expert.eval] (with a non-raising [on_exn]), no exceptions will ever
      actually be raised. This should be safe to use in environments where raising
      exceptions is costly (or at the very least, better than raising directly). *)
  val raise : exn -> 'a t

  val raise_s : Sexp.t -> 'a t
  val raise_error : Error.t -> 'a t

  (** Like [Result.ok_exn], except with the performance-friendly properties of [raise]. *)
  val lower_result : ('a, Exn.t) Result.t t -> 'a t

  (** Like [Or_error.ok_exn], except with the performance-friendly properties of
      [raise_error]. *)
  val lower_or_error : 'a Or_error.t t -> 'a t

  (** If evaluating the given effect produces an exception before producing a value,
      [try_with] will return the first exn in the error side of the [Result.t].

      If [rest] is [`Call f], [try_with] will use [f] to handle any further exns produced.
      Otherwise, they will be raised further (i.e. visible to [iter_errors], [try_with],
      and eventual [Expert.handle]/[Expert.eval] calls), although they cannot cause the
      effect itself to raise (i.e. further [try_with]s will result in Ok and handle them
      via its [rest] parameter). *)
  val try_with
    :  ?rest:[ `Raise | `Call of Exn.t -> unit ] (** default is [`Raise] *)
    -> 'a t
    -> ('a, Exn.t) Result.t t

  (** Like [try_with], but returns the exn as an [Error.t]. *)
  val try_with_or_error
    :  ?rest:[ `Raise | `Call of Exn.t -> unit ] (** default is [`Raise] *)
    -> 'a t
    -> 'a Or_error.t t

  (** If exceptions are produced in the course of evaluating the input effect, the
      function passed to [iter_errors] will be invoked with those errors. Multiple errors
      can be produced from functions such as [both_parallel], [all_parallel], and
      [protect]. This doesn't _handle_ the errors, but you can record or print the error.
      Throwing an exception inside of [f] will not recursively invoke [f] on the new error
      that is produced.

      Note that [iter_errors] only calls [f] on exns as they are produced; it does not
      wait for more exns before letting an exn continue to raise. For example,
      [iter_errors (both_parallel (raise a) (raise b)) ~f] will evaluate [f a], raise [a]
      to its next handler, evaluate [f b], and then raise [b] to its next handler. *)
  val iter_errors : 'a t -> f:(Exn.t -> unit t) -> 'a t

  (** [protect] runs an effect, and after the effect completes (including if it fails with
      an exception), it also runs the [finally] effect.

      If the given effect raises an exception, [protect] will output an effect that also
      raises that exception after executing [finally] (even if [finally] also raises). If
      [finally] raises (but not the given effect), the output of [protect] will raise with
      that exception. *)
  val protect : 'a t -> finally:unit t -> 'a t

  (** evaluates both effects in parallel and returns their product when both complete *)
  val both_parallel : 'a t -> 'b t -> ('a * 'b) t

  (** evaluates all effects in the list in parallel and returns the list of results when
      all of them complete. The output list is always the same length as the input list. *)
  val all_parallel : 'a t list -> 'a list t

  (** like [all_parallel], but for [unit Effect.t]s. *)
  val all_parallel_unit : unit t list -> unit t

  (** If creating an effect could be expensive, you can wrap its construction in a lazy
      and pass it to this function so that its construction will be deferred until it's
      about to be evaluated. *)
  val lazy_ : 'a t Lazy.t -> 'a t

  (** Prints the sexp when scheduled. *)
  val print_s : Sexp.t -> unit t

  (** [of_sync_fun] is similar to [of_deferred_fun] but with a synchronous function
      instead of a deferred one. This can be used for functions that are synchronous but
      side-effecting, or as a mock-function in tests that replace the usages of
      [of_deferred_fun] in the actual app.

      Note that, unlike [of_deferred_fun], the function must return immediately, so it's
      not possible to test the behaviour of tour app between calling the function and the
      effect becoming 'determined'. If you need to do this, see [of_svar] and
      [of_query_response_tracker] below. *)
  val of_sync_fun : ('query -> 'result) -> 'query -> 'result t

  (** Like [of_sync_fun] but with a pre-applied unit query. Side-effects in the function
      will be run every time that the resulting effect is scheduled *)
  val of_thunk : (unit -> 'result) -> 'result t

  (** Like [of_sync_fun] but with an extra [on_exn] parameter that can be passed into
      calls to [Expert.handle]. *)
  val of_sync_fun' : ('query -> on_exn:(Exn.t -> unit) -> 'result) -> 'query -> 'result t

  (** Like [of_thunk] but with an extra [on_exn] parameter that can be passed into calls
      to [Expert.handle]. *)
  val of_thunk' : (unit -> on_exn:(Exn.t -> unit) -> 'result) -> 'result t

  module Define (Handler : Handler) :
    S with type action := Handler.Action.t and type 'a t := 'a t

  module Define1 (Handler : Handler1) :
    S1 with type 'a action := 'a Handler.Action.t and type 'a t := 'a t

  module Expert : sig
    (** [eval t ~f] runs the given effect, and calls [f] when the effect completes, or
        [on_exn] if the effect does not return a result due to an exception. This function
        should not be called while constructing effects; it's intended for use by
        libraries that actually schedule effects, such as Bonsai, or Virtual_dom.

        [on_further_exns] is called on exceptions that occur after the effect has gotten a
        result/exn. For example, if every branch of an [all_parallel] call raises an
        exception, [on_exn] would be called on the first exn, while [on_further_exns]
        would be called on the rest. There is no guarantee that the callbacks will be
        called in any particular order. *)
    val eval
      :  on_exn:(Exn.t -> unit)
      -> ?on_further_exns:(Exn.t -> unit) (** defaults to [on_exn] *)
      -> 'a t
      -> f:('a -> unit)
      -> unit

    (** [handle ~on_exn ?on_further_exns t] is the same as
        [eval ~on_exn ?on_further_exns t ~f:ignore]. *)
    val handle
      :  on_exn:(Exn.t -> unit)
      -> ?on_further_exns:(Exn.t -> unit) (** defaults to [on_exn] *)
      -> unit t
      -> unit

    (* We use this table for dispatching to the appropriate handler in an efficient way. *)
    type hide =
      | T :
          { value : 'a t
          ; callback : 'a -> unit
          ; on_exn : Exn.t -> unit
          }
          -> hide

    val handlers : (hide -> unit) Hashtbl.M(Int).t
    val of_fun : f:(callback:('a -> unit) -> on_exn:(Exn.t -> unit) -> unit) -> 'a t
  end

  module Private : sig
    module Callback : sig
      type 'a effect := 'a t
      type ('a, 'b) t

      val make
        :  request:'a
        -> on_response:('b -> unit effect)
        -> on_exn:(Exn.t -> unit)
        -> ('a, 'b) t

      val request : ('a, 'b) t -> 'a
      val respond_to : ('a, 'b) t -> 'b -> unit effect
      val on_exn : _ t -> Exn.t -> unit
    end

    val make : request:'a -> evaluator:(('a, 'b) Callback.t -> unit) -> 'b t
  end

  (** [Result] is extremely similar to [Deferred.Result] *)
  module Result : sig
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    include Monad.S2 with type ('a, 'b) t := ('a, 'b) t

    val fail : 'err -> (_, 'err) t

    val combine
      :  ('ok1, 'err) t
      -> ('ok2, 'err) t
      -> ok:('ok1 -> 'ok2 -> 'ok3)
      -> err:('err -> 'err -> 'err)
      -> ('ok3, 'err) t
  end

  (** Like [Result] above, this is extremely similar to [Deferred.Or_error]. A lot of the
      additional functions are missing, but can be added as needed *)
  module Or_error : sig
    type nonrec 'a t = 'a Or_error.t t

    include Applicative.S with type 'a t := 'a t
    include Monad.S with type 'a t := 'a t

    val fail : Error.t -> _ t
    val error : string -> 'a -> ('a -> Sexp.t) -> _ t
    val error_s : Sexp.t -> _ t
    val error_string : string -> _ t
  end

  module For_testing : sig
    module Svar : sig
      (** You can think of an [Svar.t] as like an [Ivar.t] whose purpose is to allow us to
          implement [of_svar] below.

          (The difference between [Svar] and [Ivar] is that the former is synchronous.
          That is, when [fill_if_empty] is called, it will directly call all of the
          handlers rather than scheduling that they be called later. This semantics can be
          confusing to work with in large-scale programs, as it means the control flow of
          your application hops around a lot more. However, it does mean that you don't
          need a scheduler, so it's easier to implement.) *)

      type 'a t

      val create : unit -> 'a t
      val upon : 'a t -> ('a -> unit) -> unit
      val fill_if_empty : 'a t -> 'a -> unit
      val peek : 'a t -> 'a option
    end

    (** Create an effect from a function that returns an [Svar.t]. This is mostly useful
        in testing, to emulate a ['query -> 'result Deferred.t] function that does not
        return immediately. You may find [Query_response_tracker] a more convenient
        interface than using [of_svar] directly. *)
    val of_svar_fun : ('query -> 'result Svar.t) -> 'query -> 'result t

    module Query_response_tracker : sig
      (** [Query_response_tracker] is an interface designed to make [of_svar] more
          convenient to use. When the function returned by [of_query_response_tracker t]
          is called (typically by your bonsai app), the query passed is stored within [t].
          Your test code can then call [maybe_respond] to cause those effects to 'become
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
