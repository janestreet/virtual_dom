open! Core
include Ui_effect_intf

type 'a t = ..
type 'a t += Ignore : unit t | Many : unit t list -> unit t

(* We use this table for dispatching to the appropriate handler in an efficient way.  *)
type hidden = T : ('a t * ('a -> unit)) -> hidden

let handlers : (hidden -> unit) Hashtbl.M(Int).t = Hashtbl.create (module Int) ~size:8

module Obj = struct
  module Extension_constructor = struct
    [@@@ocaml.warning "-3"]

    let id = Caml.Obj.extension_id
    let of_val = Caml.Obj.extension_constructor
  end
end

module Define (Handler : Handler) :
  S with type action := Handler.Action.t and type 'a t := 'a t = struct
  type _ t += C : Handler.Action.t -> unit t

  let key = Obj.Extension_constructor.id [%extension_constructor C]

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

  let key = Obj.Extension_constructor.id [%extension_constructor C]

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

let get_key t = Obj.Extension_constructor.id (Obj.Extension_constructor.of_val t)

let handle_registered_event (T (t, cb)) =
  Hashtbl.find_exn handlers (get_key t) (T (t, cb))
;;

module Print_s = Define (struct
    module Action = Sexp

    let handle s = print_s s
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

include Core.Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

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

  type hide = hidden = T : ('a t * ('a -> unit)) -> hide

  let handlers = handlers
  let of_fun = of_fun
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

    let make : request:'a -> evaluator:(('a, 'b) Callback.t -> unit t) -> 'b t =
      fun ~request ~evaluator ->
      Expert.of_fun ~f:(fun ~callback ->
        let callback =
          Callback.make ~request ~on_response:(fun response ->
            callback response;
            Ignore)
        in
        Expert.handle (evaluator callback))
    ;;
  end

  module For_testing = struct
    module Svar = struct
      type 'a state =
        | Empty of { handlers : ('a -> unit) Bag.t }
        | Full of 'a

      type 'a t = 'a state ref

      let create () = ref (Empty { handlers = Bag.create () })

      let upon t handler =
        match !t with
        | Empty { handlers } -> ignore (Bag.add handlers handler : _ Bag.Elt.t)
        | Full x -> handler x
      ;;

      let fill_if_empty t x =
        match !t with
        | Full _ -> ()
        | Empty { handlers } ->
          Bag.iter handlers ~f:(fun handler -> handler x);
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

      type ('q, 'r) t = ('q, 'r) rpc Bag.t

      let create () = Bag.create ()

      let add_query t query =
        let response = Svar.create () in
        ignore (Bag.add t { query; response } : _ Bag.Elt.t);
        response
      ;;

      let queries_pending_response t =
        Bag.to_list t |> List.map ~f:(fun { query; response = _ } -> query)
      ;;

      type 'r maybe_respond =
        | No_response_yet
        | Respond of 'r

      let maybe_respond t ~f =
        Bag.filter_inplace t ~f:(fun { query; response } ->
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
