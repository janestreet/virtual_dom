open! Core
open! Js_of_ocaml

module type S = Hooks_intf.S

let cancel_animation_frame id = Dom_html.window##cancelAnimationFrame id

let request_animation_frame f =
  Dom_html.window##requestAnimationFrame (Js.wrap_callback f)
;;

module Extra = struct
  type t =
    | T :
        { type_id : 'a Type_equal.Id.t
        ; value : 'a
        }
        -> t

  let sexp_of_t (T { type_id; value }) = Type_equal.Id.to_sexp type_id value
end

type t =
  | T :
      { input : 'input
      ; input_id : 'input Type_equal.Id.t
      ; combine_inputs : 'input -> 'input -> 'input
      ; init :
          'input
          -> Dom_html.element Js.t
          -> 'input * Dom_html.animation_frame_request_id * 'state
      ; update :
          'input
          -> 'input * Dom_html.animation_frame_request_id * 'state
          -> Dom_html.element Js.t
          -> 'input * Dom_html.animation_frame_request_id * 'state
      ; destroy :
          'input * Dom_html.animation_frame_request_id * 'state
          -> Dom_html.element Js.t
          -> unit
      ; id : ('input * Dom_html.animation_frame_request_id * 'state) Core.Type_equal.Id.t
      }
      -> t

let generic_hook = lazy Js.Unsafe.(get global (Js.string "GenericHook"))

let make_hook ~combine_inputs ~init ~extra:(input, input_id) ~update ~destroy ~id =
  T { init; combine_inputs; input; input_id; update; destroy; id }
;;

let pack (T { init; input; input_id; update; destroy; id; _ }) =
  let wrap a = a |> Js.wrap_callback |> Js.Unsafe.inject in
  let init = wrap (init input) in
  let update = wrap (update input) in
  let destroy = wrap destroy in
  let generic_hook = Lazy.force generic_hook in
  let extra = Extra.T { type_id = input_id; value = input } in
  Js.Unsafe.fun_call
    generic_hook
    [| init; update; destroy; id |> Js.Unsafe.inject; extra |> Js.Unsafe.inject |]
;;

let combine (T left) (T right) =
  match Type_equal.Id.same_witness left.input_id right.input_id with
  | None ->
    eprint_s
      [%message
        "hooks do not have the same type, so they cannot be combined; taking the second \
         of the two"];
    T right
  | Some T -> T { right with input = right.combine_inputs left.input right.input }
;;

module Make (S : S) = struct
  let input_and_state_id =
    Type_equal.Id.create ~name:"" (fun (input, _animation_id, state) ->
      [%sexp_of: S.Input.t * opaque] (input, state))
  ;;

  let input_id = Type_equal.Id.create ~name:"" S.Input.sexp_of_t

  let init input element =
    let state = S.init input element in
    let animation_id =
      request_animation_frame (fun _ -> S.on_mount input state element)
    in
    input, animation_id, state
  ;;

  let update input (old_input, animation_id, state) element =
    S.update ~old_input ~new_input:input state element;
    input, animation_id, state
  ;;

  let destroy (old_input, animation_id, state) element =
    cancel_animation_frame animation_id;
    S.destroy old_input state element
  ;;

  let create input =
    make_hook
      ~extra:(input, input_id)
      ~combine_inputs:S.Input.combine
      ~id:input_and_state_id
      ~init
      ~update
      ~destroy
  ;;

  module For_testing = struct
    let type_id = input_id
  end
end

module For_testing = struct
  module Extra = Extra
end
