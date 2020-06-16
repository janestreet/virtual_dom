open Base
open Js_of_ocaml

type virtual_dom_node
type virtual_dom_patch

class type virtual_dom =
  object
    method _VNode :
      (Js.js_string Js.t
       -> < > Js.t
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
       -> < > Js.t
       -> virtual_dom_node Js.t Js.js_array Js.t
       -> Js.js_string Js.t Js.optdef
       -> virtual_dom_node Js.t)
        Js.constr
        Js.readonly_prop
  end

let virtual_dom : virtual_dom Js.t = Js.Unsafe.global ##. VirtualDom

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

      method info : Sexp.t option Js.prop

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
  type t = (unit, Dom_html.element Js.t) widget Js.t

  (* here is how we throw away type information.  Our good old friend Obj.magic,
     but constrained a little bit *)
  external t_of_widget : (_, _) widget Js.t -> t = "%identity"

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
      obj##.state := s0;
      dom_node);
    obj##.update
    := Js.wrap_callback (fun prev dom_node ->
      (* The [update] method of [obj] is only called by virtual-dom after it has checked
         that the [id]s of [prev] and [obj] are "===" equal. Thus [same_witness_exn] will
         never raise.
      *)
      match Type_equal.Id.same_witness_exn prev##.id id with
      | Type_equal.T ->
        let state', dom_node' = update prev##.state dom_node in
        obj##.state := state';
        dom_node');
    obj##.destroy := Js.wrap_callback (fun dom_node -> destroy obj##.state dom_node);
    t_of_widget obj
  ;;

  external to_js : t -> virtual_dom_node Js.t = "%identity"
end

module T : sig
  module Element : sig
    type t

    val tag : t -> string
    val attrs : t -> Attrs.t
    val key : t -> string option
    val with_key : t -> string -> t
    val map_attrs : t -> f:(Attrs.t -> Attrs.t) -> t
    val add_class : t -> string -> t
    val add_style : t -> Css_gen.t -> t
  end

  type t =
    | None
    | Text of string
    | Element of Element.t
    | Widget of Widget.t

  val text : string -> t

  val widget
    :  ?info:Sexp.t
    -> ?destroy:('s -> (#Dom_html.element as 'e) Js.t -> unit)
    -> ?update:('s -> 'e Js.t -> 's * 'e Js.t)
    -> id:('s * 'e Js.t) Type_equal.Id.t
    -> init:(unit -> 's * 'e Js.t)
    -> unit
    -> t

  val create : string -> ?key:string -> Attr.t list -> t list -> t
  val create_childless : string -> ?key:string -> Attr.t list -> t
  val svg : string -> ?key:string -> Attr.t list -> t list -> t
  val to_js : t -> virtual_dom_node Js.t
end = struct
  type element =
    { tag : string
    ; key : string option
    ; attrs : Attrs.t
    ; children : virtual_dom_node Js.t list
    ; kind : [ `Vnode | `Svg ]
    }

  and t =
    | None
    | Text of string
    | Element of element
    | Widget of Widget.t

  module Element = struct
    type t = element

    let tag t = t.tag
    let attrs t = t.attrs
    let key t = t.key
    let with_key t key = { t with key = Some key }
    let map_attrs t ~f = { t with attrs = f t.attrs }
    let add_class t c = map_attrs t ~f:(fun a -> Attrs.add_class a c)
    let add_style t s = map_attrs t ~f:(fun a -> Attrs.add_style a s)
  end

  let string_to_js_text s =
    let vtext = virtual_dom##._VText in
    new%js vtext (Js.string s)
  ;;

  let to_js = function
    | None ->
      (* We normally filter these out, but if [to_js] is called directly on a [None] node,
         we use this hack. Aside from having a [Text] node without any text present in the
         Dom, there should be no unwanted side-effects.  In an Incr_dom application, this
         can only happen when the root view Incremental is inhabited by a [None]. *)
      string_to_js_text ""
    | Text s -> string_to_js_text s
    | Element { tag; key; attrs; children; kind = `Vnode } ->
      let vnode = virtual_dom##._VNode in
      (match key with
       | None ->
         new%js vnode
           (Js.string tag)
           (Attr.list_to_obj attrs)
           (Js.array (Array.of_list children))
           Js.Optdef.empty
       | Some key ->
         new%js vnode
           (Js.string tag)
           (Attr.list_to_obj attrs)
           (Js.array (Array.of_list children))
           (Js.Optdef.return (Js.string key)))
    | Element { tag; key; attrs; children; kind = `Svg } ->
      let vnode = virtual_dom##.svg in
      (match key with
       | None ->
         new%js vnode
           (Js.string tag)
           (Attr.list_to_obj attrs)
           (Js.array (Array.of_list children))
           Js.Optdef.empty
       | Some key ->
         new%js vnode
           (Js.string tag)
           (Attr.list_to_obj attrs)
           (Js.array (Array.of_list children))
           (Js.Optdef.return (Js.string key)))
    | Widget w -> Widget.to_js w
  ;;

  let element kind ~tag ~key attrs children =
    let children =
      List.filter_map children ~f:(function
        | None -> None
        | other -> Some (to_js other))
    in
    { kind; tag; key; attrs; children }
  ;;

  let text s = Text s

  let widget ?info ?destroy ?update ~id ~init () =
    Widget (Widget.create ?info ?destroy ?update ~id ~init ())
  ;;

  let create tag ?key attrs children = Element (element `Vnode ~tag ~key attrs children)
  let create_childless tag ?key attrs = create tag ?key attrs []
  let svg tag ?key attrs children = Element (element `Svg ~tag ~key attrs children)
end

module Element = T.Element

type t = T.t =
  | None
  | Text of string
  | Element of Element.t
  | Widget of Widget.t

let none = None
let text = T.text
let textf format = Printf.ksprintf text format
let create = T.create
let create_svg = T.svg
let create_childless = T.create_childless
let widget = T.widget

type node_creator = ?key:string -> Attr.t list -> t list -> t
type node_creator_childless = ?key:string -> Attr.t list -> t

let to_dom t : Dom_html.element Js.t = virtual_dom##createElement (T.to_js t)

let inner_html `This_html_is_sanitized_and_is_totally_safe_trust_me ~tag ~content =
  let sexp tag content =
    Sexp.List [ Sexp.Atom "inner-html"; Sexp.Atom tag; Sexp.Atom content ]
  in
  let id =
    Type_equal.Id.create ~name:"inner-html-node" (fun ((tag, content), _) ->
      sexp tag content)
  in
  widget
    ~info:(sexp tag content)
    ~id
    ~init:(fun () ->
      let element = to_dom (create tag [] []) in
      element##.innerHTML := Js.string content;
      (tag, content), element)
    ()
;;

let unsafe_to_js t = t |> T.to_js |> Js.Unsafe.inject
let a = create "a"
let body = create "body"
let button = create "button"
let code = create "code"
let div = create "div"
let footer = create "footer"
let h1 = create "h1"
let h2 = create "h2"
let h3 = create "h3"
let h4 = create "h4"
let h5 = create "h5"
let h6 = create "h6"
let header = create "header"
let html = create "html"
let input = create "input"
let textarea = create "textarea"
let select = create "select"
let option = create "option"
let label = create "label"
let li = create "li"
let p = create "p"
let pre = create "pre"
let section = create "section"
let span = create "span"
let strong = create "strong"
let table = create "table"
let tbody = create "tbody"
let td = create "td"
let th = create "th"
let thead = create "thead"
let tr = create "tr"
let ul = create "ul"
let ol = create "ol"
let br = create_childless "br"
let hr = create_childless "hr"

module Patch = struct
  type node = t
  type t = virtual_dom_patch Js.t

  let create ~previous ~current = virtual_dom##diff (T.to_js previous) (T.to_js current)
  let apply t elt = virtual_dom##patch elt t

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
  ;;
end
