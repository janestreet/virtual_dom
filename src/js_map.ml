[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
open! Js_of_ocaml
open! Gen_js_api
type ('a, 'b) t = Ojs.t
let rec (t_of_js : (Ojs.t -> 'a) -> (Ojs.t -> 'b) -> Ojs.t -> ('a, 'b) t) =
  fun _ -> fun _ -> fun x2 -> x2
and (t_to_js : ('a -> Ojs.t) -> ('b -> Ojs.t) -> ('a, 'b) t -> Ojs.t) =
  fun _ -> fun _ -> fun x1 -> x1
let (create : unit -> ('a, 'b) t) =
  fun () ->
    t_of_js Obj.magic Obj.magic
      (Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Map") [||])
let (set : ('a, 'b) t -> 'a -> 'b -> unit) =
  fun x7 ->
    fun x5 ->
      fun x6 ->
        ignore
          (Ojs.call (t_to_js Obj.magic Obj.magic x7) "set"
             [|(Obj.magic x5);(Obj.magic x6)|])
let (get : ('a, 'b) t -> 'a -> 'b option) =
  fun x11 ->
    fun x10 ->
      Ojs.option_of_js Obj.magic
        (Ojs.call (t_to_js Obj.magic Obj.magic x11) "get" [|(Obj.magic x10)|])
let (delete : ('a, 'b) t -> 'a -> unit) =
  fun x16 ->
    fun x15 ->
      ignore
        (Ojs.call (t_to_js Obj.magic Obj.magic x16) "delete"
           [|(Obj.magic x15)|])
