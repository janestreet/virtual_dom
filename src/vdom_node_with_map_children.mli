open! Core
open! Js_of_ocaml

(** When given a map of vdom nodes, this function will wrap them in an element whose tag
    is determined by the first argument, and efficiently diff them against new nodes in
    the future that were created by this function. *)
val make : ?is_svg:bool -> tag:string -> ?attr:Attr.t -> (_, Node.t, _) Map.t -> Node.t

type ('a, 'b) node_creator := ?attrs:Attr.t list -> ('a, Node.t, 'b) Map.t -> Node.t

val div : _ node_creator
val span : _ node_creator
val ul : _ node_creator
val ol : _ node_creator
