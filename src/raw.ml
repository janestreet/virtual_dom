open Base
open Js_of_ocaml
open Gen_js_api

module Native_node : sig
  type t = Dom_html.element Js.t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end = struct
  type t = Dom_html.element Js.t

  let t_of_js x = Stdlib.Obj.magic x
  let t_to_js x = Stdlib.Obj.magic x
end

module Attrs : sig
  type t = private Ojs.t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
  val create : unit -> t
  val has_property : t -> string -> bool
  val has_attribute : t -> string -> bool
  val set_property : t -> string -> Ojs.t -> unit
  val set_attribute : t -> string -> Ojs.t -> unit
end = struct
  type t = Ojs.t

  let t_of_js x = x
  let t_to_js x = x
  let create () : t = Ojs.empty_obj ()

  let set_property : t -> string -> t -> unit =
    fun t name value -> Ojs.set_prop_ascii t name value
  ;;

  let has_property : t -> string -> bool = Ojs.has_property

  let has_attribute t name =
    Ojs.has_property t "attributes"
    && Ojs.has_property (Ojs.get_prop_ascii t "attributes") name
  ;;

  let set_attribute : t -> string -> t -> unit =
    fun t name value ->
      if phys_equal (Ojs.get_prop_ascii t "attributes") (Ojs.variable "undefined")
      then Ojs.set_prop_ascii t "attributes" (Ojs.empty_obj ());
      Ojs.set_prop_ascii (Ojs.get_prop_ascii t "attributes") name value
  ;;
end

module Element_array = struct
  (* This type must only be instantiated with Node.t as the
     type parameter because it ignores the inner-most conversion
     function.  This is safe because Node.t is defined as Ojs.t,
     but we can't take advantage of that fact because the Node
     module is generated via ppx. *)

  type 'a t = 'a Js_of_ocaml.Js.js_array Js.t

  let t_of_js : _ -> Ojs.t -> 'a t = fun _ -> Caml.Obj.magic
  let t_to_js : _ -> 'a t -> Ojs.t = fun _ -> Caml.Obj.magic
end

module Node =
  [%js:
    type t = private Ojs.t

    val t_of_js : Ojs.t -> t
    val t_to_js : t -> Ojs.t

    val node : string -> Attrs.t -> t Element_array.t -> string option -> t
    [@@js.new "VirtualDom.VNode"]

    val text : string -> t [@@js.new "VirtualDom.VText"]

    val svg : string -> Attrs.t -> t Element_array.t -> string option -> t
    [@@js.new "VirtualDom.svg"]

    val to_dom : t -> Native_node.t [@@js.global "VirtualDom.createElement"]]

module Patch =
  [%js:
    type t = private Ojs.t

    val t_of_js : Ojs.t -> t
    val t_to_js : t -> Ojs.t
    val create : previous:Node.t -> current:Node.t -> t [@@js.global "VirtualDom.diff"]
    val apply : Native_node.t -> t -> Native_node.t [@@js.global "VirtualDom.patch"]

    val is_empty : t -> bool
    [@@js.custom
      let is_empty =
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
      ;;]]

module Widget = struct
  class type ['s, 'element] widget =
    object
      constraint 'element = #Dom_html.element Js.t
      method type_ : Js.js_string Js.t Js.writeonly_prop

      (* virtual-dom considers two widgets of being of the same "kind" if either
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
      method info : Sexp.t Lazy.t option Js.prop
      method destroy : ('element -> unit) Js.callback Js.writeonly_prop

      method update :
        (('other_state, 'other_element) widget Js.t -> 'element -> 'element) Js.callback
          Js.writeonly_prop

      method init : (unit -> 'element) Js.callback Js.writeonly_prop
    end

  (* We model JS level objects here so there is a lot of throwing away of type
     information.  We could possibly try to rediscover more of it.  Or maybe we
     should see if we can get rid Widget completely.
     the unit type parameters here are not actually unit, but part of
     the type info we have thrown away into our dance
     with JS *)
  type t = Node.t

  (* here is how we throw away type information.  Our good old friend Obj.magic,
     but constrained a little bit *)
  external ojs_of_js : (_, _) widget Js.t -> Ojs.t = "%identity"

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
        ?info
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
    obj##.info := info;
    obj##.init
    := Js.wrap_callback (fun () ->
      let s0, dom_node = init () in
      State_keeper.set ~id dom_node s0;
      dom_node);
    obj##.update
    := Js.wrap_callback (fun prev dom_node ->
      (* The [update] method of [obj] is only called by virtual-dom after it has checked
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
    Node.t_of_js (ojs_of_js obj)
  ;;
end
