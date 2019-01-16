open Js_of_ocaml
open Base

(** The values associated with an Element and element like nodes.
    (that is in practice all nodes that aren't just text). *)
module Element : sig
  type t

  val tag : t -> string
  val attrs : t -> Attrs.t
  val key : t -> string option
  val map_attrs : t -> f:(Attrs.t -> Attrs.t) -> t
  val add_style : t -> Css_gen.t -> t
  val add_class : t -> string -> t
end

module Widget : sig
  (* If you want a widget to be diffed against another (by calling the update
     function of the new widget), the two widgets must have physically equal
     ids. *)

  type t

  val create
    :  ?destroy:('s -> (#Dom_html.element as 'e) Js.t -> unit)
    -> ?update:('s -> 'e Js.t -> 's * 'e Js.t)
    -> id:('s * 'e Js.t) Type_equal.Id.t
    -> init:(unit -> 's * 'e Js.t)
    -> unit
    -> t
end

type t =
  | Text of string
  | Element of Element.t
  | Widget of Widget.t

type node_creator = ?key:string -> Attr.t list -> t list -> t
type node_creator_childless = ?key:string -> Attr.t list -> t

val text : string -> t
val a : node_creator
val body : node_creator
val button : node_creator
val div : node_creator
val footer : node_creator
val h1 : node_creator
val h2 : node_creator
val h3 : node_creator
val h4 : node_creator
val h5 : node_creator
val header : node_creator
val html : node_creator
val input : node_creator
val textarea : node_creator
val select : node_creator
val option : node_creator
val label : node_creator
val li : node_creator
val p : node_creator
val pre : node_creator
val section : node_creator
val span : node_creator
val strong : node_creator
val table : node_creator
val tbody : node_creator
val td : node_creator
val th : node_creator
val thead : node_creator
val tr : node_creator
val ul : node_creator
val br : node_creator_childless
val hr : node_creator_childless

(** [key] is used by Virtual_dom as a hint during diffing/patching *)
val create : string -> ?key:string -> Attr.t list -> t list -> t

val svg : string -> ?key:string -> Attr.t list -> t list -> t
val to_dom : t -> Dom_html.element Js.t

(** convenience wrapper [widget ... = Widget (Widget.create ...)] *)
val widget
  :  ?destroy:('s -> (#Dom_html.element as 'e) Js.t -> unit)
  -> ?update:('s -> 'e Js.t -> 's * 'e Js.t)
  -> id:('s * 'e Js.t) Type_equal.Id.t
  -> init:(unit -> 's * 'e Js.t)
  -> unit
  -> t

module Patch : sig
  type node = t
  type t

  val create : previous:node -> current:node -> t
  val apply : t -> Dom_html.element Js.t -> Dom_html.element Js.t
  val is_empty : t -> bool
end
