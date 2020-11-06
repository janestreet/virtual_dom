open! Core_kernel
open! Js_of_ocaml

module type S = Hooks_intf.S

let cancel_animation_frame id = Dom_html.window##cancelAnimationFrame id

let request_animation_frame f =
  Dom_html.window##requestAnimationFrame (Js.wrap_callback f)
;;

let make_hook = (Attr.Expert.create_persistent_hook [@ocaml.warning "-3"] (* Beta API *))

module Make (S : S) = struct
  let input_and_state_id =
    Type_equal.Id.create ~name:"" (fun (input, _animation_id, state) ->
      [%sexp_of: S.Input.t * opaque] (input, state))
  ;;

  let input_id = Type_equal.Id.create ~name:"" S.Input.sexp_of_t

  let create ~name input =
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
    make_hook ~extra:(input, input_id) name ~id:input_and_state_id ~init ~update ~destroy
  ;;

  module For_testing = struct
    let type_id = input_id
  end
end
