open! Core_kernel
include Ui_event_intf

type t = ..
type t += Ignore | Many of t list

(* We use this table for dispatching to the appropriate handler in an efficient way.  *)
let handlers : (t -> unit) Hashtbl.M(Int).t = Hashtbl.create (module Int) ~size:8

module Obj = struct
  module Extension_constructor = struct
    [@@@ocaml.warning "-3"]

    let id = Caml.Obj.extension_id
    let of_val = Caml.Obj.extension_constructor
  end
end

module Define (Handler : Handler) :
  S with type action := Handler.Action.t and type t := t = struct
  type t += C : Handler.Action.t -> t

  let key = Obj.Extension_constructor.id [%extension_constructor C]

  let () =
    Hashtbl.add_exn handlers ~key ~data:(fun inp ->
      match inp with
      | C value -> Handler.handle value
      | _ -> raise_s [%message "Unrecognized variant"])
  ;;

  let inject v = C v
end

let get_key t = Obj.Extension_constructor.id (Obj.Extension_constructor.of_val t)
let handle_registered_event t = Hashtbl.find_exn handlers (get_key t) t

module Expert = struct
  let rec handle t =
    match t with
    | Ignore -> ()
    | Many l -> List.iter ~f:handle l
    | t -> handle_registered_event t
  ;;

  let handlers = handlers
end
