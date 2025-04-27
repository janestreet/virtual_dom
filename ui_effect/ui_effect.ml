open! Base
include Ui_effect_intf

type 'a t = ..
type 'a t += Ignore : unit t | Many : unit t list -> unit t

(* We use this table for dispatching to the appropriate handler in an efficient way.  *)
type hidden = T : ('a t * ('a -> unit)) -> hidden

let handlers : (hidden -> unit) Hashtbl.M(Int).t = Hashtbl.create (module Int) ~size:8

module Define (Handler : Handler) :
  S with type action := Handler.Action.t and type 'a t := 'a t = struct
  type _ t += C : Handler.Action.t -> unit t

  let key = Stdlib.Obj.Extension_constructor.id [%extension_constructor C]

  let () =
    Hashtbl.add_exn handlers ~key ~data:(fun inp ->
      match inp with
      | T (C value, callback) ->
        Handler.handle value;
        callback ()
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
      | T (C value, callback) ->
        let called = ref false in
        let callback a =
          if !called
          then failwith "on_response called multiple times!"
          else called := true;
          callback a
        in
        Handler.handle value ~on_response:callback
      | _ -> raise_s [%message "Unrecognized variant"])
  ;;

  let inject v = C v
end

let get_key t = Stdlib.Obj.Extension_constructor.(id (of_val t))

let handle_registered_event (T (t, cb)) =
  Hashtbl.find_exn handlers (get_key t) (T (t, cb))
;;

module Print_s = Define (struct
    module Action = Sexp

    let handle s = Stdio.print_s s
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
  | Fun : (callback:('a -> unit) -> unit) -> 'a t

let return a = Return a
let bind (type a) (t : a t) ~f = Bind { t; f }
let map (type a b) (t : a t) ~f : b t = Map { t; f }
let never = Never
let of_fun ~f = Fun f
let lazy_ a = Lazy a

module As_monad = Base.Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

include As_monad

let rec eval : type a. a t -> callback:(a -> unit) -> unit =
  fun t ~callback ->
  match t with
  | Fun f -> f ~callback
  | Ignore -> callback ()
  | Return a -> callback a
  | Lazy (lazy t) -> eval t ~callback
  | Many l ->
    List.iter l ~f:(eval ~callback:ignore);
    callback ()
  | Bind { t; f } -> eval t ~callback:(fun a -> eval (f a) ~callback)
  | Map { t; f } -> eval t ~callback:(fun a -> callback (f a))
  | Never -> ()
  | t -> handle_registered_event (T (t, callback))
;;

module Expert = struct
  let handle = eval ~callback:ignore
  let eval t ~f = eval t ~callback:f

  type hide = hidden = T : ('a t * ('a -> unit)) -> hide

  let handlers = handlers
  let of_fun = of_fun
end

let both_parallel a b =
  Expert.of_fun ~f:(fun ~callback ->
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
      Expert.eval effect ~f:(fun x ->
        ref := Some x;
        maybe_finalize ())
    in
    dispatch a a_res;
    dispatch b b_res)
;;

let all_parallel xs =
  if List.is_empty xs
  then return []
  else
    Expert.of_fun ~f:(fun ~callback ->
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
      List.iteri xs ~f:(fun i e -> Expert.eval e ~f:(complete_one i)))
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
  module Sync_fun_arg = struct
    module Action = struct
      type 'r t = T : 'a * ('a -> 'r) -> 'r t
    end

    let handle (Action.T (a, f)) ~on_response = on_response (f a)
  end

  module Sync_fun = Define1 (Sync_fun_arg)

  let of_sync_fun f a = Sync_fun.inject (T (a, f))
  let of_thunk f = of_sync_fun f ()

  module Private = struct
    module Callback = struct
      type nonrec ('a, 'b) t =
        { request : 'a
        ; on_response : 'b -> unit t
        }

      let make ~request ~on_response = { request; on_response }
      let request { request; _ } = request
      let respond_to { on_response; _ } response = on_response response
    end

    let make : request:'a -> evaluator:(('a, 'b) Callback.t -> unit) -> 'b t =
      fun ~request ~evaluator ->
      Expert.of_fun ~f:(fun ~callback ->
        let callback =
          Callback.make ~request ~on_response:(fun response ->
            callback response;
            Ignore)
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

      let handle (Action.T (a, f)) ~on_response = Svar.upon (f a) on_response
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
