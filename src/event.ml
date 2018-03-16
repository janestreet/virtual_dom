open Base
open Js_of_ocaml

include Event_intf

type t = ..
type t +=
  | Ignore
  | Viewport_changed
  | Stop_propagation
  | Prevent_default
  | Many of t list

(* We use this table for dispatching to the appropriate handler in an efficient way.  *)
let handlers : (t -> unit) Hashtbl.M(Int).t =
  Hashtbl.create (module Int) ~size:8

(* All visibility handlers see all events, so a simple list is enough.  *)
let visibility_handlers : (unit -> unit) list ref = ref []

module Define (Handler : Handler)
  : S with type action := Handler.Action.t
       and type t := t
= struct
  type t += C : Handler.Action.t -> t

  let key = Caml.Obj.extension_id [%extension_constructor C]

  let () =
    Hashtbl.add_exn handlers ~key ~data:(fun inp ->
      match inp with
      | C value -> Handler.handle value
      | _ -> raise_s [%message "Unrecognized variant"]
    )

  let inject v = C v
end

module Define_visibility (VH : Visibility_handler) = struct
  let () =
    visibility_handlers := VH.handle :: !visibility_handlers
end

let get_key t = Caml.Obj.extension_id (Caml.Obj.extension_constructor t)

let handle_registered_event t = Hashtbl.find_exn handlers (get_key t) t

module Expert = struct
  let handle evt =
    let rec handle t =
      match t with
      | Ignore -> ()
      | Many l -> List.iter ~f:handle l
      | Viewport_changed -> List.iter !visibility_handlers ~f:(fun f -> f ())
      | Stop_propagation ->
        (* Safe to do because [stopPropagation] is defined equivalently to
           [preventDefault] *)
        Dom_html.stopPropagation evt
      | Prevent_default -> Dom.preventDefault evt
      | t -> handle_registered_event t
    in
    handle


  let rec handle_non_dom_event_exn t =
    match t with
    | Ignore -> ()
    | Many l -> List.iter ~f:handle_non_dom_event_exn l
    | Viewport_changed -> List.iter !visibility_handlers ~f:(fun f -> f ())
    | Stop_propagation ->
      failwith "[handle_non_dom_event_exn] called with [Stop_propagation] \
        which requires a dom event"
    | Prevent_default ->
      failwith "[handle_non_dom_event_exn] called with [Prevent_default] \
        which requires a dom event"
    | t -> handle_registered_event t

end
