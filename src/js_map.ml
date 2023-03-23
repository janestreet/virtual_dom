open! Js_of_ocaml
open! Js

class type ['a, 'b] map = object ('self)
  method set : 'a -> 'b -> unit meth
  method get : 'a -> 'b Optdef.t meth
  method delete : 'a -> unit meth
end

type ('a, 'b) t = ('a, 'b) map Js.t

let map : unit -> ('a, 'b) map Js.t Js.constr = fun () -> Unsafe.global##._Map

let create () =
  let map = map () in
  new%js map
;;

let set t a b = t##set a b
let get t a = Js.Optdef.to_option (t##get a)
let delete t a = t##delete a
