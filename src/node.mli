open Js_of_ocaml
open Core_kernel.Std

type t

val text : string -> t

val body   : Attr.t list -> t list -> t
val button : Attr.t list -> t list -> t
val div    : Attr.t list -> t list -> t
val input  : Attr.t list -> t list -> t
val span   : Attr.t list -> t list -> t
val table  : Attr.t list -> t list -> t
val td     : Attr.t list -> t list -> t
val th     : Attr.t list -> t list -> t
val tr     : Attr.t list -> t list -> t
val thead  : Attr.t list -> t list -> t
val tbody  : Attr.t list -> t list -> t
val h1     : Attr.t list -> t list -> t
val h2     : Attr.t list -> t list -> t
val h3     : Attr.t list -> t list -> t
val h4     : Attr.t list -> t list -> t
val h5     : Attr.t list -> t list -> t


val create
  :  string
  -> Attr.t list
  -> t list
  -> t

val svg
  :  string
  -> Attr.t list
  -> t list
  -> t

val to_dom : t -> Dom_html.element Js.t

(* If you want a widget to be diffed against another (by calling the update
   function of the new widget), the two widgets must have physically equal
   ids. *)
val widget
  :  ?destroy:('s -> (#Dom_html.element as 'e) Js.t -> unit)
  -> ?update:('s -> 'e Js.t -> 's * 'e Js.t)
  -> id:('s * 'e Js.t) Type_equal.Id.t
  -> init:(unit -> 's * 'e Js.t)
  -> unit
  -> t

module Lazy : sig
  val create : ('a -> t) -> 'a -> t
  val create2 : ('a -> 'b -> t) -> 'a -> 'b -> t
  val create3 : ('a -> 'b -> 'c -> t) -> 'a -> 'b -> 'c -> t
  val create4 : ('a -> 'b -> 'c -> 'd -> t) -> 'a -> 'b -> 'c -> 'd -> t
  val create5 : ('a -> 'b -> 'c -> 'd -> 'e -> t) -> 'a -> 'b -> 'c -> 'd -> 'e -> t
end

module Patch : sig
  type node = t
  type t

  val create
    :  previous:node
    -> current:node
    -> t

  val apply : t -> Dom.element Js.t -> Dom.element Js.t
end
