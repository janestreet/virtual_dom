open! Core

type t = Raw.Node.t

val create : key:string option -> 'a -> f:('a -> Raw.Node.t) -> Raw.Node.t
