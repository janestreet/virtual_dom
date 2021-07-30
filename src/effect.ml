open Base
open Js_of_ocaml
include Ui_effect

(* All visibility handlers see all events, so a simple list is enough.  *)
let visibility_handlers : (unit -> unit) list ref = ref []

module type Visibility_handler = sig
  val handle : unit -> unit
end

module Define_visibility (VH : Visibility_handler) = struct
  let () = visibility_handlers := VH.handle :: !visibility_handlers
end

module Obj = struct
  module Extension_constructor = struct
    [@@@ocaml.warning "-3"]

    let id = Caml.Obj.extension_id
    let of_val = Caml.Obj.extension_constructor
  end
end

type _ t +=
  | Viewport_changed
  | Stop_propagation
  | Stop_immediate_propagation
  | Prevent_default

let sequence_as_sibling left ~unless_stopped =
  let rec contains_stop = function
    | Many es -> List.exists es ~f:contains_stop
    | Stop_immediate_propagation -> true
    | _ -> false
  in
  if contains_stop left then left else Ui_effect.Many [ left; unless_stopped () ]
;;

(* We need to keep track of the current dom event here so that
   movement between [Vdom.Effect.Expert.handle] and
   [Ui_concrete.Effect.Expert.handle] keeps the original
   dom event around. *)
let current_dom_event = ref None

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:Obj.Extension_constructor.(id (of_val Viewport_changed))
    ~data:(fun _ -> List.iter !visibility_handlers ~f:(fun f -> f ()))
;;

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:Obj.Extension_constructor.(id (of_val Stop_propagation))
    ~data:(fun _ -> Option.iter !current_dom_event ~f:Dom_html.stopPropagation)
;;

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:Obj.Extension_constructor.(id (of_val Prevent_default))
    ~data:(fun _ -> Option.iter !current_dom_event ~f:Dom.preventDefault)
;;

module Expert = struct
  let handle_non_dom_event_exn = Expert.handle

  let handle dom_event event =
    let old = !current_dom_event in
    current_dom_event := Some (dom_event :> Dom_html.element Dom.event Js.t);
    Expert.handle event;
    current_dom_event := old
  ;;
end
