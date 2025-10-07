open! Base
open! Async_kernel

module Deferred_fun_arg = struct
  module Action = struct
    type 'r t = T : 'a * ('a -> on_exn:(Exn.t -> unit) -> 'r Deferred.t) -> 'r t
  end

  let handle (Action.T (a, f)) ~on_response ~on_exn =
    let on_exn' exn = on_exn (Monitor.extract_exn exn) in
    don't_wait_for
      (match%map.Deferred
         Monitor.try_with ~extract_exn:false ~rest:(`Call on_exn') (fun () -> f a ~on_exn)
       with
       | Ok v -> on_response v
       | Error e -> on_exn' e)
  ;;
end

module Deferred_fun = Ui_effect.Define1 (Deferred_fun_arg)

let of_deferred_fun' f a = Deferred_fun.inject (T (a, f))
let of_deferred_thunk' f = of_deferred_fun' f ()
let of_deferred_fun f a = Deferred_fun.inject (T (a, fun a ~on_exn:_ -> f a))
let of_deferred_thunk f = of_deferred_fun f ()

let expert_handle_as_deferred e =
  let result_ivar = Ivar.create () in
  Ui_effect.Expert.eval e ~on_exn:raise ~f:(Ivar.fill_if_empty result_ivar);
  Ivar.read result_ivar
;;
