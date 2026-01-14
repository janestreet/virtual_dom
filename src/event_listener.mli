open! Core
open! Js_of_ocaml

module Phase : sig
  type t =
    | Capture
    | Bubbling
end

module Capture_and_bubbling : sig
  type 'a t =
    { capture : 'a option
    ; bubbling : 'a option
    }
  [@@deriving sexp_of]
end

module Target : sig
  type t =
    | Window
    | Element
end

module Make : functor
    (Arg : sig
       type event = private #Dom_html.event

       val event_kind : event Js.t Dom.Event.typ
       val target : Target.t
     end)
    -> sig
  val create : Phase.t -> f:(Arg.event Js.t -> unit Ui_effect.t) -> Hooks.t

  module For_testing : sig
    val type_id
      : (Arg.event Js.t -> unit Ui_effect.t) Capture_and_bubbling.t Type_equal.Id.t
  end
end
