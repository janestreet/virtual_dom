open Js_of_ocaml

type t
type node_creator = ?key:string -> Attr.t list -> t list -> t

val text : string -> t

val body   : node_creator
val button : node_creator
val div    : node_creator
val input  : node_creator
val span   : node_creator
val table  : node_creator
val td     : node_creator
val th     : node_creator
val tr     : node_creator
val thead  : node_creator
val tbody  : node_creator
val h1     : node_creator
val h2     : node_creator
val h3     : node_creator
val h4     : node_creator
val h5     : node_creator


(** [key] is used by Virtual_dom as a hint during diffing/patching *)
val create
  :  string
  -> ?key:string
  -> Attr.t list
  -> t list
  -> t

val svg
  :  string
  -> ?key:string
  -> Attr.t list
  -> t list
  -> t

val to_dom : t -> Dom_html.element Js.t

type object_id = int (* TODO *)

(* If you want a widget to be diffed against another (by calling the update
   function of the new widget), the two widgets must have physically equal
   ids. *)
val widget
  :  ?destroy:('s -> (#Dom_html.element as 'e) Js.t -> unit)
  -> ?update:('s -> 'e Js.t -> 's * 'e Js.t)
  -> id:object_id
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
