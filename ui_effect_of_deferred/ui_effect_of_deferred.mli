open! Base
open! Async_kernel

(** [of_deferred_fun] is a way to convert from a deferred-returning function to an
    effect-returning function. This function is commonly used to wrap RPC calls. *)
val of_deferred_fun : ('query -> 'response Deferred.t) -> 'query -> 'response Ui_effect.t

(** Like [of_deferred_fun] but with a pre-applied unit query. Side-effects in the function
    will be run every time that the resulting effect is scheduled *)
val of_deferred_thunk : (unit -> 'response Deferred.t) -> 'response Ui_effect.t

(** Like [of_deferred_fun] but with an extra [on_exn] parameter that can be passed into
    calls to [Expert.handle]. *)
val of_deferred_fun'
  :  ('query -> on_exn:(Exn.t -> unit) -> 'response Deferred.t)
  -> 'query
  -> 'response Ui_effect.t

(** Like [of_deferred_thunk] but with an extra [on_exn] parameter that can be passed into
    calls to [Expert.handle]. *)
val of_deferred_thunk'
  :  (unit -> on_exn:(Exn.t -> unit) -> 'response Deferred.t)
  -> 'response Ui_effect.t

(** [expert_handle_as_deferred] will return a function that schedules the effect, and
    returns the result as a [Deferred.t]. *)
val expert_handle_as_deferred : 'a Ui_effect.t -> 'a Deferred.t
