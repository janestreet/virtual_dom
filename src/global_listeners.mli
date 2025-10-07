open! Core
open! Js_of_ocaml

(** Hooks to set events listeners on [window]. This is needed as if we only set them on
    individual elements we will miss ones that happen outside of the viewport

    https://coderwall.com/p/79hkbw/js-mouse-events-that-work-even-when-mouse-is-moved-outside-the-window *)

module Phase : sig
  (** Should the handler run on the [capture] or [bubbling] phase of event processing?
      https://javascript.info/bubbling-and-capturing#capturing

      If ran on capture, it will run before any element-specific listeners.

      If ran on bubbling, it will run after all element-specific listeners, and it won't
      run if [stopPropagation] was called during the handlers of the event target or any
      of its ancestors.

      When in doubt, default to [Bubbling]. *)
  type t =
    | Capture
    | Bubbling
end

(** Mouse-event handlers *)

val mousedown
  :  phase:Phase.t
  -> f:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t)
  -> Attr.t

val mouseup : phase:Phase.t -> f:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t

val mousemove
  :  phase:Phase.t
  -> f:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t)
  -> Attr.t

val click : phase:Phase.t -> f:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t

(* Focus event handlers *)
val blur : phase:Phase.t -> f:(Dom_html.focusEvent Js.t -> unit Ui_effect.t) -> Attr.t
val focusin : phase:Phase.t -> f:(Dom_html.focusEvent Js.t -> unit Ui_effect.t) -> Attr.t
val focusout : phase:Phase.t -> f:(Dom_html.focusEvent Js.t -> unit Ui_effect.t) -> Attr.t

val contextmenu
  :  phase:Phase.t
  -> f:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t)
  -> Attr.t

(** Keyboard-event handlers *)

val keydown
  :  phase:Phase.t
  -> f:(Dom_html.keyboardEvent Js.t -> unit Ui_effect.t)
  -> Attr.t

val keyup : phase:Phase.t -> f:(Dom_html.keyboardEvent Js.t -> unit Ui_effect.t) -> Attr.t

(** Other event handlers *)
val visibilitychange
  :  phase:Phase.t
  -> f:(Dom_html.event Js.t -> unit Ui_effect.t)
  -> Attr.t

(* Chrome may not allow Javascript to run after a user requests a tab close, so an
   effect passed into `Custom_best_effort may or may not execute to completion *)
val beforeunload
  :  phase:Phase.t
  -> f:
       (Dom_html.event Js.t
        -> [ `Show_warning | `Do_nothing | `Custom_best_effort of unit Ui_effect.t ]
             Ui_effect.t)
  -> Attr.t

module For_testing : sig
  type 'a t =
    { capture : 'a option
    ; bubbling : 'a option
    }

  (** In tests, you might just want to run all global listeners. This helper will run the
      `combine` and `bubbling` handlers in parallel. If your tests include running other
      handlers, you probably don't want this. *)
  val combine_capture_and_bubbling
    :  ('a -> unit Ui_effect.t) t
    -> ('a -> unit Ui_effect.t)

  val mousedown_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) t Type_equal.Id.t
  val mouseup_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) t Type_equal.Id.t
  val mousemove_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) t Type_equal.Id.t
  val click_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) t Type_equal.Id.t

  val contextmenu_type_id
    : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) t Type_equal.Id.t

  val keydown_type_id
    : (Dom_html.keyboardEvent Js.t -> unit Ui_effect.t) t Type_equal.Id.t

  val visibilitychange_type_id
    : (Dom_html.event Js.t -> unit Ui_effect.t) t Type_equal.Id.t

  val beforeunload_type_id : (Dom_html.event Js.t -> unit Ui_effect.t) t Type_equal.Id.t
end
