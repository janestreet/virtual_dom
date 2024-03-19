open Js_of_ocaml

module Make (X : sig
  type event = private #Dom_html.event

  val event_kind : event Js.t Dom.Event.typ
end) =
struct
  include Hooks.Make (struct
    module Input = struct
      type t = X.event Js.t -> unit Ui_effect.t [@@deriving sexp_of]

      let combine f g event = Ui_effect.Many [ f event; g event ]
    end

    module State = struct
      type t = { mutable listener : (Dom_html.event_listener_id[@sexp.opaque]) }
      [@@deriving sexp_of]
    end

    let set f =
      let handler =
        Dom.handler (fun ev ->
          Effect.Expert.handle ev (f ev);
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

    let on_mount = `Do_nothing
  end)
end

module Mouseup = Make (struct
  type event = Dom_html.mouseEvent

  let event_kind = Dom_html.Event.mouseup
end)

module Mousemove = Make (struct
  type event = Dom_html.mouseEvent

  let event_kind = Dom_html.Event.mousemove
end)

module Click = Make (struct
  type event = Dom_html.mouseEvent

  let event_kind = Dom_html.Event.click
end)

module Contextmenu = Make (struct
  type event = Dom_html.mouseEvent

  let event_kind = Dom_html.Event.make "contextmenu"
end)

module Keydown = Make (struct
  type event = Dom_html.keyboardEvent

  let event_kind = Dom_html.Event.keydown
end)

module Visibilitychange = Make (struct
  type event = Dom_html.event

  let event_kind = Dom_html.Event.make "visibilitychange"
end)

module Beforeunload = Make (struct
  type event = Dom_html.event

  let event_kind = Dom_html.Event.make "beforeunload"
end)

let mouseup f = Mouseup.create f |> Attr.create_hook "global-mouseup-listener"
let mousemove f = Mousemove.create f |> Attr.create_hook "global-mousemove-listener"
let click f = Click.create f |> Attr.create_hook "global-click-listener"
let contextmenu f = Contextmenu.create f |> Attr.create_hook "global-contextmenu-listener"
let keydown f = Keydown.create f |> Attr.create_hook "global-keydown-listener"

class type event_with_string_return_value = object
  (* Events with [returnValue] are impossible to properly type, so we make one that
     is specialized for string, and cast our before_unload type to it. *)
  method returnValue : Js.js_string Js.t Js.writeonly_prop
end

let beforeunload f =
  let f event =
    match%bind.Effect f event with
    | `Show_warning ->
      let event : event_with_string_return_value Js.t = Obj.magic event in
      event##.returnValue := Js.string "this string can be anything";
      Effect.Ignore
    | `Do_nothing -> Effect.Ignore
    | `Custom_best_effort effect -> effect
  in
  Beforeunload.create f |> Attr.create_hook "global-beforeunload-listener"
;;

let visibilitychange f =
  Visibilitychange.create f |> Attr.create_hook "global-visibilitychange-listener"
;;

module For_testing = struct
  let mouseup_type_id = Mouseup.For_testing.type_id
  let mousemove_type_id = Mousemove.For_testing.type_id
  let keydown_type_id = Keydown.For_testing.type_id
  let click_type_id = Click.For_testing.type_id
  let contextmenu_type_id = Contextmenu.For_testing.type_id
  let visibilitychange_type_id = Visibilitychange.For_testing.type_id
  let beforeunload_type_id = Beforeunload.For_testing.type_id
end
