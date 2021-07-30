open Js_of_ocaml

module Make (X : sig
    val event_kind : Dom_html.mouseEvent Js.t Dom.Event.typ
  end) =
struct
  include Hooks.Make (struct
      module Input = struct
        type t = Dom_html.mouseEvent Js.t -> unit Ui_effect.t [@@deriving sexp_of]

        let combine f g event = Ui_effect.Many [ f event; g event ]
      end

      module State = struct
        type t = { mutable listener : (Dom_html.event_listener_id[@sexp.opaque]) }
        [@@deriving sexp_of]
      end

      let set f =
        let handler =
          Dom.handler (fun ev ->
            Effect.Expert.handle_non_dom_event_exn (f ev);
            Js._true)
        in
        Dom_html.addEventListener Dom_html.window X.event_kind handler Js._true
      ;;

      let init f _element = { State.listener = set f }
      let destroy _input state _element = Dom_html.removeEventListener state.State.listener

      let update ~old_input ~new_input:f state element =
        (* if the callback function changes, cancel the old one and re-install *)
        destroy old_input state element;
        let new_state = init f element in
        state.State.listener <- new_state.listener
      ;;

      let on_mount _input _state _element = ()
    end)
end

module Mouse_up = Make (struct
    let event_kind = Dom_html.Event.mouseup
  end)

module Mouse_move = Make (struct
    let event_kind = Dom_html.Event.mousemove
  end)

let mouseup f = Mouse_up.create f |> Attr.create_hook "global-mouseup-listener"
let mousemove f = Mouse_move.create f |> Attr.create_hook "global-mousemove-listener"

module For_testing = struct
  let mouse_up_type_id = Mouse_up.For_testing.type_id
  let mouse_move_type_id = Mouse_move.For_testing.type_id
end
