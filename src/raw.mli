open Base
open Js_of_ocaml

module Attrs : sig
  type t = private Js.Unsafe.any

  val create : unit -> t
  val has_property : t -> string -> bool
  val has_attribute : t -> string -> bool
  val set_property : t -> string -> Js.Unsafe.any -> unit
  val set_attribute : t -> string -> Js.Unsafe.any -> unit
end

module Node : sig
  type t

  val node : string -> Attrs.t -> t Js.js_array Js.t -> string option -> t
  val text : string -> t
  val svg : string -> Attrs.t -> t Js.js_array Js.t -> string option -> t
  val to_dom : t -> Dom_html.element Js.t
end

module Patch : sig
  type t

  val create : previous:Node.t -> current:Node.t -> t
  val apply : Dom_html.element Js.t -> t -> Dom_html.element Js.t
  val is_empty : t -> bool
end

module Widget : sig
  type t = Node.t

  val create
    :  ?vdom_for_testing:Node.t Lazy.t
    -> ?destroy:('s -> (#Dom_html.element as 'a) Js.t -> unit)
    -> ?update:('s -> 'a Js.t -> 's * 'a Js.t)
    -> id:('s * 'a Js.t) Type_equal.Id.t
    -> init:(unit -> 's * 'a Js.t)
    -> unit
    -> t
end
