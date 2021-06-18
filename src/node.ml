open Base

module Widget = struct
  open Js_of_ocaml
  include Raw.Widget

  module type S = sig
    type dom = private #Dom_html.element

    module Input : sig
      type t [@@deriving sexp_of]
    end

    module State : sig
      type t [@@deriving sexp_of]
    end

    val name : string
    val create : Input.t -> State.t * dom Js.t

    val update
      :  prev_input:Input.t
      -> input:Input.t
      -> state:State.t
      -> element:dom Js.t
      -> State.t * dom Js.t

    val destroy : prev_input:Input.t -> state:State.t -> element:dom Js.t -> unit
  end

  let of_module (type input) (module M : S with type Input.t = input) =
    let module State = struct
      type t =
        { input : M.Input.t
        ; state : M.State.t
        }
      [@@deriving sexp_of]
    end
    in
    let sexp_of_dom : M.dom Js.t -> Sexp.t = fun _ -> Sexp.Atom "<opaque>" in
    let id = Type_equal.Id.create ~name:M.name [%sexp_of: State.t * dom] in
    Base.Staged.stage (fun input ->
      let info = lazy (M.Input.sexp_of_t input) in
      create
        ~id
        ~info
        ~init:(fun () ->
          let state, element = M.create input in
          { input; state }, element)
        ~update:(fun { State.input = prev_input; state } element ->
          let state, element = M.update ~prev_input ~input ~state ~element in
          { input; state }, element)
        ~destroy:(fun { State.input = prev_input; state } element ->
          M.destroy ~prev_input ~state ~element)
        ())
  ;;
end

type element =
  { tag : string
  ; key : string option
  ; attrs : Attr.t
  ; raw_attrs : Raw.Attrs.t Lazy.t
  ; children : Raw.Node.t list
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

  let map_attrs t ~f =
    let attrs = f t.attrs in
    let raw_attrs = lazy (Attr.to_raw attrs) in
    { t with attrs; raw_attrs }
  ;;

  let add_class t c = map_attrs t ~f:(fun a -> Attr.(a @ class_ c))
  let add_style t s = map_attrs t ~f:(fun a -> Attr.(a @ style s))
end

let t_to_js = function
  | None ->
    (* We normally filter these out, but if [to_js] is called directly on a [None] node,
       we use this hack. Aside from having a [Text] node without any text present in the
       Dom, there should be no unwanted side-effects.  In an Incr_dom application, this
       can only happen when the root view Incremental is inhabited by a [None]. *)
    Raw.Node.text ""
  | Text s -> Raw.Node.text s
  | Element { tag; key; attrs = _; raw_attrs = (lazy raw_attrs); children; kind = `Vnode }
    -> Raw.Node.node tag raw_attrs children key
  | Element { tag; key; attrs = _; raw_attrs = (lazy raw_attrs); children; kind = `Svg }
    -> Raw.Node.svg tag raw_attrs children key
  | Widget w -> w
;;

let element kind ~tag ~key attrs children =
  let attrs = attrs in
  let children =
    List.filter_map children ~f:(function
      | None -> None
      | (Text _ | Element _ | Widget _) as other -> Some (t_to_js other))
  in
  let raw_attrs = lazy (Attr.to_raw attrs) in
  { kind; tag; key; attrs; raw_attrs; children }
;;

let text s = Text s

let widget ?info ?destroy ?update ~id ~init () =
  Widget (Widget.create ?info ?destroy ?update ~id ~init ())
;;

let create tag ?key attrs children =
  Element (element `Vnode ~tag ~key (Attr.many_without_merge attrs) children)
;;

let create_monoid tag ?key ?(attr = Attr.empty) children =
  Element (element `Vnode ~tag ~key attr children)
;;

let create_childless tag ?key attr = create_monoid tag ?key ~attr []

let create_svg tag ?key ?(attr = Attr.empty) children =
  Element (element `Svg ~tag ~key attr children)
;;

let create_svg_monoid tag ?key ?(attr = Attr.empty) children =
  Element (element `Svg ~tag ~key attr children)
;;

let none = None
let textf format = Printf.ksprintf text format

let widget_of_module m =
  let f = Base.Staged.unstage (Widget.of_module m) in
  Base.Staged.stage (fun i -> Widget (f i))
;;

type node_creator_old = ?key:string -> Attr.t list -> t list -> t
type node_creator = ?key:string -> ?attr:Attr.t -> t list -> t
type node_creator_childless = ?key:string -> Attr.t list -> t
type node_creator_childless_monoid = ?key:string -> Attr.t -> t

let to_raw = t_to_js
let to_dom t = Raw.Node.to_dom (to_raw t)

let inner_html
      create
      ~tag
      ~attr
      ~this_html_is_sanitized_and_is_totally_safe_trust_me:content
  =
  let element = create tag ~attr [] in
  let build_sexp ~extra ~content =
    Sexp.List [ Sexp.Atom "inner-html"; extra; Sexp.Atom content ]
  in
  let id =
    Type_equal.Id.create ~name:"inner-html-node" (fun ((element, content), _) ->
      build_sexp ~extra:element ~content)
  in
  let debug =
    match element with
    | Element element -> Sexp.Atom (Element.tag element)
    | Widget _ -> failwith "Vdom.Node.inner_html was given a 'widget'"
    | None -> failwith "Vdom.Node.inner_html was given a 'none'"
    | Text _ -> failwith "Vdom.Node.inner_html was given a 'text'"
  in
  widget
    ~id
    ~info:(lazy (build_sexp ~extra:debug ~content))
    ~init:(fun () ->
      let element = to_dom element in
      element##.innerHTML := Js_of_ocaml.Js.string content;
      (debug, content), element)
    ()
;;

let inner_html_svg = inner_html (fun tag ~attr -> create_svg_monoid tag ?key:None ~attr)
let inner_html = inner_html (fun tag ~attr -> create_monoid tag ?key:None ~attr)
let a = create_monoid "a"
let body = create_monoid "body"
let button = create_monoid "button"
let code = create_monoid "code"
let div = create_monoid "div"
let footer = create_monoid "footer"
let h1 = create_monoid "h1"
let h2 = create_monoid "h2"
let h3 = create_monoid "h3"
let h4 = create_monoid "h4"
let h5 = create_monoid "h5"
let h6 = create_monoid "h6"
let header = create_monoid "header"
let html = create_monoid "html"
let input = create_monoid "input"
let textarea = create_monoid "textarea"
let select = create_monoid "select"
let option = create_monoid "option"
let label = create_monoid "label"
let li = create_monoid "li"
let p = create_monoid "p"
let pre = create_monoid "pre"
let section = create_monoid "section"
let span = create_monoid "span"
let strong = create_monoid "strong"
let table = create_monoid "table"
let tbody = create_monoid "tbody"
let td = create_monoid "td"
let th = create_monoid "th"
let thead = create_monoid "thead"
let tr = create_monoid "tr"
let ul = create_monoid "ul"
let ol = create_monoid "ol"
let br = create_childless "br"
let hr = create_childless "hr"

let sexp_for_debugging ?indent sexp =
  sexp |> Sexp.to_string_hum ?indent |> text |> List.return |> pre ~attr:Attr.empty
;;

module Patch = struct
  type node = t
  type t = Raw.Patch.t

  let create ~previous ~current =
    Raw.Patch.create ~previous:(t_to_js previous) ~current:(t_to_js current)
  ;;

  let apply t elt = Raw.Patch.apply elt t
  let is_empty t = Raw.Patch.is_empty t
end
