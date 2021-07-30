open! Core
open! Js_of_ocaml

(** Hooks to set mouse events listeners on [window]. This is needed as if we only set
    them on individual elements we will miss ones that happen outside of the viewport

    https://coderwall.com/p/79hkbw/js-mouse-events-that-work-even-when-mouse-is-moved-outside-the-window *)

val mouseup : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t
val mousemove : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) -> Attr.t

module For_testing : sig
  val mouse_up_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
  val mouse_move_type_id : (Dom_html.mouseEvent Js.t -> unit Ui_effect.t) Type_equal.Id.t
end
