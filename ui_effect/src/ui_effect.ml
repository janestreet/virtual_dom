open! Base
include Ui_effect_intf

type 'a t = ..
type 'a t += Ignore : unit t | Many : unit t list -> unit t

(* We use this table for dispatching to the appropriate handler in an efficient way.  *)
type hidden =
  | T :
      { value : 'a t
      ; callback : 'a -> unit
      ; on_exn : Exn.t -> unit
      }
      -> hidden

let handlers : (hidden -> unit) Hashtbl.M(Int).t = Hashtbl.create (module Int) ~size:8

(* Runs [f] while ensuring that the number of calls to [callback] and [on_exn] is at
   most 1. If [on_exn] is called multiple times or after a call to [callback], these
   exns are diverted to [on_further_exns]. *)
let ensure_single_callback_is_called f ~callback ~on_exn ~on_further_exns =
  let primary_cb_filled = ref false in
  let callback v =
    if !primary_cb_filled
    then ignore v
    else (
      primary_cb_filled := true;
      callback v)
  in
  let on_exn exn =
    if !primary_cb_filled
    then on_further_exns exn
    else (
      primary_cb_filled := true;
      on_exn exn)
  in
  f ~callback ~on_exn ~on_further_exns
;;

(* Runs [f] while ensuring that (a) raised exceptions are caught, (b) exception
   handlers are only called once for each exn, and (c) ensures of
   [ensure_single_callback_is_called]. *)
let run_custom_function f ~callback ~on_exn ~on_further_exns =
  let f =
    ensure_single_callback_is_called (fun ~callback ~on_exn ~on_further_exns:_ ->
      f ~callback ~on_exn)
  in
  (* True if any of {callback,on_exn,on_further_exns} have raised yet.
     If one of them raises and is synchronous, it can cause [f ...] to raise
     as well. In this case, we've already called [on_exn], so we shouldn't call it
     again. *)
  let raised_from_callback = ref false in
  let wrap_callback f v =
    try f v with
    | exn when not !raised_from_callback ->
      raised_from_callback := true;
      Base.raise exn
  in
  let callback = wrap_callback callback in
  let on_exn = wrap_callback on_exn in
  let on_further_exns = wrap_callback on_further_exns in
  try f ~callback ~on_exn ~on_further_exns with
  | exn when not !raised_from_callback -> on_exn exn
;;

module Define (Handler : Handler) :
  S with type action := Handler.Action.t and type 'a t := 'a t = struct
  type _ t += C : Handler.Action.t -> unit t

  let key = Stdlib.Obj.Extension_constructor.id [%extension_constructor C]

  let () =
    Hashtbl.add_exn handlers ~key ~data:(fun inp ->
      match inp with
      | T { value = C value; callback; on_exn } ->
        let f ~callback ~on_exn =
          Handler.handle value ~on_exn;
          callback ()
        in
        run_custom_function f ~callback ~on_exn ~on_further_exns:on_exn
      | _ -> raise_s [%message "Unrecognized variant"])
  ;;

  let inject v = C v
end

module Define1 (Handler : Handler1) :
  S1 with type 'a action := 'a Handler.Action.t and type 'a t := 'a t = struct
  type _ t += C : 'a Handler.Action.t -> 'a t

  let key = Stdlib.Obj.Extension_constructor.id [%extension_constructor C]

  let () =
    Hashtbl.add_exn handlers ~key ~data:(fun inp ->
      match inp with
      | T { value = C value; callback; on_exn } ->
        let f ~callback ~on_exn = Handler.handle value ~on_response:callback ~on_exn in
        run_custom_function f ~callback ~on_exn ~on_further_exns:on_exn
      | _ -> raise_s [%message "Unrecognized variant"])
  ;;

  let inject v = C v
end

let get_key t = Stdlib.Obj.Extension_constructor.(id (of_val t))

let handle_registered_event (T { value; _ } as evt) =
  Hashtbl.find_exn handlers (get_key value) evt
;;

module Print_s = Define (struct
    module Action = Sexp

    let handle s ~on_exn:_ = Stdio.print_s s
  end)

let print_s = Print_s.inject

(* Effectful things *)
type 'a t +=
  | Return : 'a -> 'a t
  | Lazy : 'a t Lazy.t -> 'a t
  | Bind :
      { t : 'a t
      ; f : 'a -> 'b t
      }
      -> 'b t
  | Map :
      { t : 'a t
      ; f : 'a -> 'b
      }
      -> 'b t
  | Never : 'b t
  | Raise : Exn.t -> _ t
  | Try_with : 'a t * [ `Raise | `Call of Exn.t -> unit ] -> ('a, Exn.t) Result.t t
  | Iter_errors : ('a t * (Exn.t -> unit t)) -> 'a t
  | Fun : (callback:('a -> unit) -> on_exn:(Exn.t -> unit) -> unit) -> 'a t

let return a = Return a
let bind (type a) (t : a t) ~f = Bind { t; f }
let map (type a b) (t : a t) ~f : b t = Map { t; f }
let never = Never
let lazy_ a = Lazy a

(** errors *)

let raise exn = Raise exn
let raise_s s = Raise (Exn.create_s s)
let raise_error error = Raise (Error.to_exn error)

let lower_result t =
  bind t ~f:(function
    | Ok v -> return v
    | Error exn -> raise exn)
;;

let lower_or_error t =
  bind t ~f:(function
    | Ok v -> return v
    | Error error -> raise_error error)
;;

let try_with ?(rest = `Raise) t = Try_with (t, rest)

let try_with_or_error ?(rest = `Raise) t =
  map (try_with t ~rest) ~f:(Result.map_error ~f:(fun exn -> Error.of_exn exn))
;;

let iter_errors t ~f = Iter_errors (t, f)
let of_fun ~f = Fun f

let protect t ~finally =
  (* We first evaluate both effects in order. *)
  bind (try_with t ~rest:`Raise) ~f:(fun result ->
    bind (try_with finally ~rest:`Raise) ~f:(fun finally ->
      (* Then, we ensure that if both [t] and [finally] raise, the effect passes [t]'s
         error to [on_exn] and [finally]'s error to [on_further_exns]. *)
      of_fun ~f:(fun ~callback ~on_exn ->
        match result, finally with
        | Ok result, Ok () -> callback result
        | Error exn, Ok () -> on_exn exn
        | Ok _, Error exn -> on_exn exn
        | Error exn1, Error exn2 ->
          on_exn exn1;
          on_exn exn2)))
;;

module As_monad = Base.Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

include As_monad

module Result = struct
  type nonrec ('a, 'b) t = ('a, 'b) Result.t t

  let fail err = As_monad.return (Error err)

  let combine t1 t2 ~ok ~err =
    let open As_monad.Let_syntax in
    let%map t1 and t2 in
    Result.combine t1 t2 ~ok ~err
  ;;

  include Base.Monad.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return a = As_monad.return (Result.return a)

      let bind t ~f =
        As_monad.bind t ~f:(function
          | Ok a -> f a
          | Error _ as error -> As_monad.return error)
      ;;

      let map t ~f = As_monad.map t ~f:(fun r -> Result.map r ~f)
      let map = `Custom map
    end)
end

module Or_error = struct
  include (Result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.t)

  type nonrec 'a t = 'a Or_error.t t

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return = return

      let apply f x =
        Result.combine
          f
          x
          ~ok:(fun f x -> f x)
          ~err:(fun e1 e2 -> Error.of_list [ e1; e2 ])
      ;;

      let map = `Custom map
    end)

  let fail err = As_monad.return (Base.Result.fail err)
  let error msg v sexp_of = As_monad.return (Or_error.error msg v sexp_of)
  let error_s sexp = As_monad.return (Or_error.error_s sexp)
  let error_string msg = As_monad.return (Or_error.error_string msg)
end

let rec eval
  : type a.
    a t
    -> callback:(a -> unit)
    -> on_exn:(Exn.t -> unit)
    -> on_further_exns:(Exn.t -> unit)
    -> unit
  =
  let eval t = ensure_single_callback_is_called (eval t) in
  let try_eval f t ~callback ~on_exn ~on_further_exns =
    match f t with
    | v -> eval v ~callback ~on_exn ~on_further_exns
    | exception exn -> on_exn exn
  in
  fun t ~callback ~on_exn ~on_further_exns ->
    match t with
    | Fun f -> run_custom_function f ~callback ~on_exn ~on_further_exns
    | Ignore -> callback ()
    | Return a -> callback a
    | Lazy t -> try_eval Lazy.force t ~callback ~on_exn ~on_further_exns
    | Many l ->
      List.iter l ~f:(eval ~callback:ignore ~on_exn ~on_further_exns);
      callback ()
    | Bind { t; f } ->
      eval
        t
        ~callback:(fun a -> try_eval f a ~callback ~on_exn ~on_further_exns)
        ~on_exn
        ~on_further_exns
    | Map { t; f } ->
      eval
        t
        ~callback:(fun a ->
          match f a with
          | v -> callback v
          | exception exn -> on_exn exn)
        ~on_exn
        ~on_further_exns
    | Never -> ()
    | Raise exn -> on_exn exn
    | Try_with (t, rest) ->
      let on_exn exn = callback (Error exn) in
      let callback a = callback (Ok a) in
      let on_further_exns =
        match rest with
        | `Raise -> on_further_exns
        | `Call f -> f
      in
      eval t ~callback ~on_exn ~on_further_exns
    | Iter_errors (t, f) ->
      (* We want the exn from [t] to be handled before the one from [f]. *)
      let sequence on_exn exn1 exn2 =
        on_exn exn1;
        on_exn exn2
      in
      let on_exn exn =
        try_eval
          f
          exn
          ~callback:(fun () -> on_exn exn)
          ~on_exn:(sequence on_exn exn)
          ~on_further_exns:(sequence on_exn exn)
      in
      let on_further_exns exn =
        try_eval
          f
          exn
          ~callback:(fun () -> on_further_exns exn)
          ~on_exn:(sequence on_further_exns exn)
          ~on_further_exns:(sequence on_further_exns exn)
      in
      eval t ~callback ~on_exn ~on_further_exns
    | value -> handle_registered_event (T { value; callback; on_exn })
;;

let eval t = ensure_single_callback_is_called (eval t)

module Expert = struct
  let handle ~on_exn ?(on_further_exns = on_exn) t =
    eval t ~callback:ignore ~on_exn ~on_further_exns
  ;;

  let eval ~on_exn ?(on_further_exns = on_exn) t ~f =
    let callback v =
      try f v with
      | exn -> on_exn exn
    in
    eval t ~callback ~on_exn ~on_further_exns
  ;;

  type hide = hidden =
    | T :
        { value : 'a t
        ; callback : 'a -> unit
        ; on_exn : Exn.t -> unit
        }
        -> hide

  let handlers = handlers
  let of_fun = of_fun
end

let both_parallel a b =
  Expert.of_fun ~f:(fun ~callback ~on_exn ->
    let a_res = ref None
    and b_res = ref None
    and finished = ref false in
    let maybe_finalize () =
      match !finished, !a_res, !b_res with
      | false, Some a, Some b ->
        finished := true;
        callback (a, b)
      | _ -> ()
    in
    let dispatch effect ref =
      Expert.eval
        effect
        ~f:(fun x ->
          ref := Some x;
          maybe_finalize ())
        ~on_exn
    in
    dispatch a a_res;
    dispatch b b_res)
;;

let all_parallel xs =
  if List.is_empty xs
  then return []
  else
    Expert.of_fun ~f:(fun ~callback ~on_exn ->
      let number_of_effects = List.length xs in
      let remaining = ref number_of_effects in
      let results = Array.create ~len:number_of_effects None in
      let complete_one i v =
        match Array.get results i with
        | Some _ ->
          (* this branch can only be hit if an effect completes multiple times *)
          ()
        | None ->
          Array.set results i (Some v);
          Int.decr remaining;
          if !remaining = 0
          then (
            let result =
              Array.filter_map results ~f:(function
                | None ->
                  Stdio.eprint_s
                    [%message
                      "BUG in ui_effect.ml: length of [all_parallel x] is not equal to \
                       length of [x]. The result of an effect is missing."
                        [%here]];
                  None
                | Some _ as e -> e)
            in
            callback (Array.to_list result))
      in
      List.iteri xs ~f:(fun i e -> Expert.eval e ~f:(complete_one i) ~on_exn))
;;

let all_parallel_unit xs = map (all_parallel xs) ~f:(ignore : unit list -> unit)

module Par = struct
  include As_monad

  module Let_syntax = struct
    include Let_syntax

    module Let_syntax = struct
      include Let_syntax

      let both = both_parallel
    end
  end
end

module Advanced = struct
  let of_sync_fun' f a = of_fun ~f:(fun ~callback ~on_exn -> callback (f a ~on_exn))
  let of_thunk' f = of_sync_fun' f ()
  let of_sync_fun f = of_sync_fun' (fun a ~on_exn:_ -> f a)
  let of_thunk f = of_sync_fun f ()

  module Private = struct
    module Callback = struct
      type nonrec ('a, 'b) t =
        { request : 'a
        ; on_response : 'b -> unit t
        ; on_exn : Exn.t -> unit
        }

      let make ~request ~on_response ~on_exn = { request; on_response; on_exn }
      let request { request; _ } = request
      let respond_to { on_response; _ } response = on_response response
      let on_exn { on_exn; _ } = on_exn
    end

    let make : request:'a -> evaluator:(('a, 'b) Callback.t -> unit) -> 'b t =
      fun ~request ~evaluator ->
      Expert.of_fun ~f:(fun ~callback ~on_exn ->
        let callback =
          Callback.make
            ~request
            ~on_response:(fun response ->
              callback response;
              Ignore)
            ~on_exn
        in
        evaluator callback)
    ;;
  end

  module For_testing = struct
    module Svar = struct
      type 'a state =
        | Empty of { mutable handlers : ('a -> unit) list }
        | Full of 'a

      type 'a t = 'a state ref

      let create () = ref (Empty { handlers = [] })

      let upon t handler =
        match !t with
        | Empty t -> t.handlers <- handler :: t.handlers
        | Full x -> handler x
      ;;

      let fill_if_empty t x =
        match !t with
        | Full _ -> ()
        | Empty { handlers } ->
          List.iter handlers ~f:(fun handler -> handler x);
          t := Full x
      ;;

      let peek t =
        match !t with
        | Empty _ -> None
        | Full x -> Some x
      ;;
    end

    module Svar_fun_arg = struct
      module Action = struct
        type 'r t = T : 'a * ('a -> 'r Svar.t) -> 'r t
      end

      let handle (Action.T (a, f)) ~on_response ~on_exn:_ = Svar.upon (f a) on_response
    end

    module Svar_fun = Define1 (Svar_fun_arg)

    let of_svar_fun f a = Svar_fun.inject (T (a, f))

    module Query_response_tracker = struct
      type ('q, 'r) rpc =
        { query : 'q
        ; response : 'r Svar.t
        }

      type ('q, 'r) t = ('q, 'r) rpc list ref

      let create () = ref []

      let add_query t query =
        let response = Svar.create () in
        t := { query; response } :: !t;
        response
      ;;

      let queries_pending_response t =
        List.map !t ~f:(fun { query; response = _ } -> query)
      ;;

      type 'r maybe_respond =
        | No_response_yet
        | Respond of 'r

      let maybe_respond t ~f =
        t
        := List.filter !t ~f:(fun { query; response } ->
             match f query with
             | No_response_yet -> true
             | Respond resp ->
               Svar.fill_if_empty response resp;
               false)
      ;;
    end

    let of_query_response_tracker qrt = of_svar_fun (Query_response_tracker.add_query qrt)
  end
end

include Advanced
