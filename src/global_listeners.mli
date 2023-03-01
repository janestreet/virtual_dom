open! Core
open! Js_of_ocaml

(** Hooks to set events listeners on [window]. This is needed as if we only set
    them on individual elements we will miss ones that happen outside of the viewport

    https://coderwall.com/p/79hkbw/js-mouse-events-that-work-even-when-mouse-is-moved-outside-the-window *)

(** Mouse-event handlers *)

val mouseup : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t
val mousemove : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t
val click : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t
val contextmenu : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t

(** Keyboard-event handlers *)

val keydown : (Dom_html.keyboardEvent Js.t -> unit Ui_effect.t) -> Attr.t

(** Other event handlers *)
val visibilitychange : (Dom_html.event Js.t -> unit Ui_effect.t) -> Attr.t

val beforeunload
  :  (Dom_html.event Js.t -> [ `Show_warning | `Do_nothing ] Ui_effect.t)
  -> Attr.t

module For_testing : sig
  val mouseup_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val mousemove_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val click_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val contextmenu_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val keydown_type_id : (Dom_html.keyboardEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val visibilitychange_type_id : (Dom_html.event Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val beforeunload_type_id : (Dom_html.event Js.t -> unit Ui_effect.t) Type_equal.Id.t
end
