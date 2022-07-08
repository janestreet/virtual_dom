open! Core
open! Js_of_ocaml

type t = Raw.Node.t

let vdom_thunk = lazy Js.Unsafe.(get global (Js.string "VdomThunk"))

let create ~key arg ~f =
  let key =
    match key with
    | Some key -> Js.Optdef.return (Js.string key)
    | None -> Js.Optdef.empty
  in
  let key = Js.Unsafe.inject key in
  let f = Js.wrap_callback (fun a -> f a) |> Js.Unsafe.inject in
  let arg = Js.Unsafe.inject arg in
  (Obj.magic : _ -> Raw.Node.t)
    (Js.Unsafe.fun_call (Lazy.force vdom_thunk) [| f; arg; key |])
;;
