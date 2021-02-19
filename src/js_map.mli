open! Js_of_ocaml
open! Gen_js_api

(** https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map *)

type ('a, 'b) t

val create : unit -> ('a, 'b) t [@@js.new "Map"]
val set : ('a, 'b) t -> 'a -> 'b -> unit
val get : ('a, 'b) t -> 'a -> 'b option
val delete : ('a, 'b) t -> 'a -> unit
