open! Core_kernel
open! Js_of_ocaml

module type S = Hooks_intf.S

let cancel_animation_frame id = Dom_html.window##cancelAnimationFrame id

let request_animation_frame f =
  Dom_html.window##requestAnimationFrame (Js.wrap_callback f)
;;

module Low_level_hook = struct
  type t = Js.Unsafe.any

  module Extra = struct
    type t =
      | T :
          { type_id : 'a Type_equal.Id.t
          ; value : 'a
          }
          -> t

    let sexp_of_t (T { type_id; value }) = Type_equal.Id.to_sexp type_id value
    let unit = T { type_id = Type_equal.Id.create ~name:"unit" sexp_of_unit; value = () }
  end

  let generic_hook = lazy Js.Unsafe.(get global (Js.string "GenericHook"))

  let create ~init ?(extra = Extra.unit) ?update ?destroy ~id =
    let wrap a = a |> Js.wrap_callback |> Js.Unsafe.inject in
    let init = wrap init in
    let update =
      update |> Option.map ~f:Js.wrap_callback |> Js.Opt.option |> Js.Unsafe.inject
    in
    let destroy = destroy |> Option.value ~default:(fun _ _ -> ()) |> wrap in
    let generic_hook = Lazy.force generic_hook in
    Js.Unsafe.fun_call
      generic_hook
      [| init; update; destroy; id |> Js.Unsafe.inject; extra |> Js.Unsafe.inject |]
  ;;
end

type t = Low_level_hook.t

let pack = Fn.id

let make_hook ?extra ~init ~update ~destroy ~id =
  let extra =
    Option.map extra ~f:(fun (value, type_id) ->
      Low_level_hook.Extra.T { value; type_id })
  in
  Low_level_hook.create ?extra ~init ~update ~destroy ~id
;;

module Make (S : S) = struct
  let input_and_state_id =
    Type_equal.Id.create ~name:"" (fun (input, _animation_id, state) ->
      [%sexp_of: S.Input.t * opaque] (input, state))
  ;;

  let input_id = Type_equal.Id.create ~name:"" S.Input.sexp_of_t

  let create input =
    let init element =
      let state = S.init input element in
      let animation_id =
        request_animation_frame (fun _ -> S.on_mount input state element)
      in
      input, animation_id, state
    in
    let update (old_input, animation_id, state) element =
      S.update ~old_input ~new_input:input state element;
      input, animation_id, state
    in
    let destroy (old_input, animation_id, state) element =
      cancel_animation_frame animation_id;
      S.destroy old_input state element
    in
    make_hook ~extra:(input, input_id) ~id:input_and_state_id ~init ~update ~destroy
  ;;

  module For_testing = struct
    let type_id = input_id
  end
end

module For_testing = struct
  module Extra = Low_level_hook.Extra
end
