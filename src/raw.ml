open Base
open Js_of_ocaml

module Js_object = struct
  type t = Js.Unsafe.any

  let empty_obj () = Js.Unsafe.obj [||]
  let set_prop_ascii = Js.Unsafe.set
  let has_property t name = Js.Optdef.test (Js.Unsafe.get t name)
  let is_undefined a = not (Js.Optdef.test a)
end

module Attrs = struct
  type t = Js_object.t

  let create () : t = Js_object.empty_obj ()

  let set_property : t -> Js.js_string Js.t -> t -> unit =
    fun t name value -> Js_object.set_prop_ascii t name value
  ;;

  let has_property : t -> Js.js_string Js.t -> bool = Js_object.has_property

  let has_attribute t (name : Js.js_string Js.t) =
    let attr = Js.Unsafe.get t (Js.string "attributes") in
    Js.Optdef.test attr && Js_object.has_property attr name
  ;;

  let set_attribute : t -> Js.js_string Js.t -> t -> unit =
    fun t name value ->
    let attr = Js.Unsafe.get t (Js.string "attributes") in
    let attr =
      if Js_object.is_undefined attr
      then (
        let attr = Js_object.empty_obj () in
        Js.Unsafe.set t (Js.string "attributes") attr;
        attr)
      else attr
    in
    Js_object.set_prop_ascii attr name value
  ;;
end

type virtual_dom_node
type virtual_dom_patch

module Virtual_dom = struct
  class type virtual_dom = object
    method _VNode :
      (Js.js_string Js.t
       -> Attrs.t
       -> virtual_dom_node Js.t Js.js_array Js.t
       -> Js.js_string Js.t Js.optdef
       -> virtual_dom_node Js.t)
        Js.constr
        Js.readonly_prop

    method _VText :
      (Js.js_string Js.t -> virtual_dom_node Js.t) Js.constr Js.readonly_prop

    method createElement : virtual_dom_node Js.t -> Dom_html.element Js.t Js.meth

    method diff :
      virtual_dom_node Js.t -> virtual_dom_node Js.t -> virtual_dom_patch Js.t Js.meth

    method patch :
      Dom_html.element Js.t -> virtual_dom_patch Js.t -> Dom_html.element Js.t Js.meth

    method svg :
      (Js.js_string Js.t
       -> Attrs.t
       -> virtual_dom_node Js.t Js.js_array Js.t
       -> Js.js_string Js.t Js.optdef
       -> virtual_dom_node Js.t)
        Js.constr
        Js.readonly_prop
  end

  let virtual_dom : virtual_dom Js.t = Js.Unsafe.global ##. VirtualDom
end

module Node = struct
  open Virtual_dom

  type t = virtual_dom_node Js.t

  let to_dom : virtual_dom_node Js.t -> Dom_html.element Js.t =
    fun vnode -> virtual_dom##createElement vnode
  ;;

  let node
    :  Js.js_string Js.t -> Attrs.t -> virtual_dom_node Js.t Js.js_array Js.t
    -> string option -> virtual_dom_node Js.t
    =
    fun tag attrs children key ->
    let key =
      match key with
      | None -> Js.Optdef.empty
      | Some key -> Js.Optdef.return (Js_of_ocaml.Js.string key)
    in
    let vnode = virtual_dom##._VNode in
    new%js vnode tag attrs children key
  ;;

  let svg
    :  Js.js_string Js.t -> Attrs.t -> virtual_dom_node Js.t Js.js_array Js.t
    -> string option -> virtual_dom_node Js.t
    =
    fun tag attrs children key ->
    let key =
      match key with
      | None -> Js.Optdef.empty
      | Some key -> Js.Optdef.return (Js.string key)
    in
    let vsvg = virtual_dom##.svg in
    new%js vsvg tag attrs children key
  ;;

  let text s =
    let vtext = virtual_dom##._VText in
    new%js vtext (Js.string s)
  ;;
end

module Patch = struct
  open Virtual_dom

  type t = virtual_dom_patch Js.t

  let diff : virtual_dom_node Js.t -> virtual_dom_node Js.t -> virtual_dom_patch Js.t =
    fun a b -> virtual_dom##diff a b
  ;;

  let patch : Dom_html.element Js.t -> virtual_dom_patch Js.t -> Dom_html.element Js.t =
    fun element vnode -> virtual_dom##patch element vnode
  ;;

  let create ~previous ~current = diff previous current
  let apply = patch

  let is_empty : t -> bool =
    let f =
      Js.Unsafe.pure_js_expr
        {js|
        (function (patch) {
          for (var key in patch) {
            if (key !== 'a') return false
          }
          return true
        })
      |js}
    in
    fun (t : t) -> Js.Unsafe.fun_call f [| Js.Unsafe.inject t |] |> Js.to_bool
  ;;
end

module Widget = struct
  class type ['s, 'element] widget = object
    constraint 'element = #Dom_html.element Js.t
    method type_ : Js.js_string Js.t Js.writeonly_prop

    (*=virtual-dom considers two widgets of being of the same "kind" if either
       of the following holds:

       1. They both have a "name" attribute and their "id" fields are equal.
       (I think this is probably a bug in virtual-dom and have field an issue
       on github: [https://github.com/Matt-Esch/virtual-dom/issues/380])

       2. Their [init] methods are "===" equal. This is true when using virtual-dom
       widgets in the usual style in Javascript, since the [init] method will be defined
       on a prototype, but is not true in this binding as it is redefined for each
       call to [widget].

       So, we go with option 1 and must have a trivial field called [name].
    *)
    method name : unit Js.writeonly_prop
    method id : ('s * 'element) Type_equal.Id.t Js.prop
    method state : 's Js.prop
    method vdomForTesting : Node.t Lazy.t option Js.prop
    method info : Sexp.t Lazy.t option Js.prop
    method destroy : ('element -> unit) Js.callback Js.writeonly_prop

    method update :
      (('other_state, 'other_element) widget Js.t -> 'element -> 'element) Js.callback
        Js.writeonly_prop

    method init : (unit -> 'element) Js.callback Js.writeonly_prop
  end

  (* We model JS level objects here so there is a lot of throwing away of type
     information. We could possibly try to rediscover more of it. Or maybe we should see
     if we can get rid Widget completely. the unit type parameters here are not actually
     unit, but part of the type info we have thrown away into our dance with JS *)
  type t = Node.t

  (* here is how we throw away type information. Our good old friend Obj.magic, but
     constrained a little bit *)
  let node_of_widget : (_, _) widget Js.t -> Node.t = Stdlib.Obj.magic

  module State_keeper = struct
    type box = T : ('a * _) Type_equal.Id.t * 'a -> box

    let t : (Js.Unsafe.any, box) Js_map.t = Js_map.create ()
    let set ~id element state = Js_map.set t (Js.Unsafe.inject element) (T (id, state))

    let get : type a b. id:(a * b) Type_equal.Id.t -> _ -> a =
      fun ~id element ->
      let element = Js.Unsafe.inject element in
      match Js_map.get t element with
      | None ->
        let id_sexp = Type_equal.Id.sexp_of_t (fun _ -> Sexp.Atom "<opaque>") id in
        raise_s [%message "BUG: element state not found" (id_sexp : Sexp.t)]
      | Some (T (f_id, state)) ->
        let T = Type_equal.Id.same_witness_exn id f_id in
        state
    ;;

    let delete element = Js_map.delete t (Js.Unsafe.inject element)
  end

  let create
    (type s)
    ?(vdom_for_testing : Node.t Lazy.t option)
    ?(destroy : s -> 'element -> unit = fun _ _ -> ())
    ?(update : s -> 'element -> s * 'element = fun s elt -> s, elt)
    ~(id : (s * 'element) Type_equal.Id.t)
    ~(init : unit -> s * 'element)
    ()
    =
    let obj : (s, _) widget Js.t = Js.Unsafe.obj [||] in
    obj##.type_ := Js.string "Widget";
    obj##.name := ();
    obj##.id := id;
    obj##.vdomForTesting := vdom_for_testing;
    obj##.init
    := Js.wrap_callback (fun () ->
         let s0, dom_node = init () in
         State_keeper.set ~id dom_node s0;
         dom_node);
    obj##.update
    := Js.wrap_callback (fun prev dom_node ->
         (*=The [update] method of [obj] is only called by virtual-dom after it has checked
         that the [id]s of [prev] and [obj] are "===" equal. Thus [same_witness_exn] will
         never raise. *)
         match Type_equal.Id.same_witness_exn prev##.id id with
         | Type_equal.T ->
           let prev_state = State_keeper.get ~id dom_node in
           let state', dom_node' = update prev_state dom_node in
           State_keeper.delete dom_node;
           State_keeper.set ~id dom_node' state';
           dom_node');
    obj##.destroy
    := Js.wrap_callback (fun dom_node ->
         let prev_state = State_keeper.get ~id dom_node in
         destroy prev_state dom_node;
         State_keeper.delete dom_node);
    node_of_widget obj
  ;;
end
