open Core
open Js_of_ocaml

module Phase = struct
  type t =
    | Capture
    | Bubbling
end

module Capture_and_bubbling = struct
  type 'a t =
    { capture : 'a option [@sexp.option]
    ; bubbling : 'a option [@sexp.option]
    }
  [@@deriving sexp_of]
end

module Make (X : sig
    type event = private #Dom_html.event

    val event_kind : event Js.t Dom.Event.typ
  end) =
struct
  module Impl = struct
    module Input = struct
      module Listener = struct
        type t = X.event Js.t -> unit Ui_effect.t [@@deriving sexp_of]

        let combine f g =
          match f, g with
          | None, None -> None
          | Some f, None -> Some f
          | None, Some g -> Some g
          | Some f, Some g -> Some (fun event -> Ui_effect.Many [ f event; g event ])
        ;;
      end

      type t = Listener.t Capture_and_bubbling.t [@@deriving sexp_of]

      let combine
        { Capture_and_bubbling.capture = capture1; bubbling = bubbling1 }
        { Capture_and_bubbling.capture = capture2; bubbling = bubbling2 }
        =
        { Capture_and_bubbling.capture = Listener.combine capture1 capture2
        ; bubbling = Listener.combine bubbling1 bubbling2
        }
      ;;
    end

    module State = struct
      type t = { mutable listeners : (Dom_html.event_listener_id list[@sexp.opaque]) }
      [@@deriving sexp_of]
    end

    let set ~use_capture f =
      let use_capture = if use_capture then Js._true else Js._false in
      let handler =
        Dom.handler (fun ev ->
          Effect.Expert.handle ev (f ev);
          Js._true)
      in
      Dom_html.addEventListener Dom_html.window X.event_kind handler use_capture
    ;;

    let init { Capture_and_bubbling.capture; bubbling } _element =
      { State.listeners =
          [ Option.map capture ~f:(set ~use_capture:true)
          ; Option.map bubbling ~f:(set ~use_capture:false)
          ]
          |> List.filter_opt
      }
    ;;

    let destroy _input state _element =
      List.iter state.State.listeners ~f:Dom_html.removeEventListener
    ;;

    let update ~old_input ~new_input:f state element =
      (* if the callback function changes, cancel the old one and re-install *)
      destroy old_input state element;
      let new_state = init f element in
      state.State.listeners <- new_state.listeners
    ;;

    let on_mount = `Do_nothing
  end

  include Hooks.Make (Impl)

  let create phase ~f =
    let input =
      match phase with
      | Phase.Capture -> { Capture_and_bubbling.capture = Some f; bubbling = None }
      | Bubbling -> { capture = None; bubbling = Some f }
    in
    create input
  ;;
end

module Mousedown = Make (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.mousedown
  end)

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

module Blur = Make (struct
    type event = Dom_html.focusEvent

    let event_kind = Dom_html.Event.blur
  end)

module Contextmenu = Make (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.make "contextmenu"
  end)

module Keydown = Make (struct
    type event = Dom_html.keyboardEvent

    let event_kind = Dom_html.Event.keydown
  end)

module Keyup = Make (struct
    type event = Dom_html.keyboardEvent

    let event_kind = Dom_html.Event.keyup
  end)

module Visibilitychange = Make (struct
    type event = Dom_html.event

    let event_kind = Dom_html.Event.make "visibilitychange"
  end)

module Beforeunload = Make (struct
    type event = Dom_html.event

    let event_kind = Dom_html.Event.make "beforeunload"
  end)

let mousedown ~phase ~f =
  Mousedown.create phase ~f |> Attr.create_hook "global-mousedown-listener"
;;

let mouseup ~phase ~f =
  Mouseup.create phase ~f |> Attr.create_hook "global-mouseup-listener"
;;

let mousemove ~phase ~f =
  Mousemove.create phase ~f |> Attr.create_hook "global-mousemove-listener"
;;

let click ~phase ~f = Click.create phase ~f |> Attr.create_hook "global-click-listener"
let blur ~phase ~f = Blur.create phase ~f |> Attr.create_hook "global-blur-listener"

let contextmenu ~phase ~f =
  Contextmenu.create phase ~f |> Attr.create_hook "global-contextmenu-listener"
;;

let keydown ~phase ~f =
  Keydown.create phase ~f |> Attr.create_hook "global-keydown-listener"
;;

let keyup ~phase ~f = Keyup.create phase ~f |> Attr.create_hook "global-keyup-listener"

class type event_with_string_return_value = object
  (* Events with [returnValue] are impossible to properly type, so we make one that
     is specialized for string, and cast our before_unload type to it. *)
  method returnValue : Js.js_string Js.t Js.writeonly_prop
end

let beforeunload ~phase ~f =
  let f event =
    match%bind.Effect f event with
    | `Show_warning ->
      let event : event_with_string_return_value Js.t = Obj.magic event in
      event##.returnValue := Js.string "this string can be anything";
      Effect.Ignore
    | `Do_nothing -> Effect.Ignore
    | `Custom_best_effort effect -> effect
  in
  Beforeunload.create phase ~f |> Attr.create_hook "global-beforeunload-listener"
;;

let visibilitychange ~phase ~f =
  Visibilitychange.create phase ~f |> Attr.create_hook "global-visibilitychange-listener"
;;

module For_testing = struct
  type 'a t = 'a Capture_and_bubbling.t =
    { capture : 'a option
    ; bubbling : 'a option
    }

  let combine_capture_and_bubbling
    : ('a -> unit Ui_effect.t) t -> ('a -> unit Ui_effect.t)
    =
    fun { capture; bubbling } event ->
    match capture, bubbling with
    | None, None -> Effect.Ignore
    | Some f, None -> f event
    | None, Some g -> g event
    | Some f, Some g -> Ui_effect.Many [ f event; g event ]
  ;;

  let mousedown_type_id = Mousedown.For_testing.type_id
  let mouseup_type_id = Mouseup.For_testing.type_id
  let mousemove_type_id = Mousemove.For_testing.type_id
  let keydown_type_id = Keydown.For_testing.type_id
  let click_type_id = Click.For_testing.type_id
  let contextmenu_type_id = Contextmenu.For_testing.type_id
  let visibilitychange_type_id = Visibilitychange.For_testing.type_id
  let beforeunload_type_id = Beforeunload.For_testing.type_id
end
