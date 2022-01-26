open Base
open Js_of_ocaml

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
  ; children : Raw.Node.t Js.js_array Js.t
  ; kind : [ `Vnode | `Svg ]
  }

and t =
  | None
  | Text of string
  | Element of element
  | Widget of Widget.t

module Aliases = struct
  type node_creator = ?key:string -> ?attr:Attr.t -> t list -> t
  type node_creator_childless = ?key:string -> ?attr:Attr.t -> unit -> t
end

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
  let add_classes t c = map_attrs t ~f:(fun a -> Attr.(a @ classes c))
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
  let children_raw = new%js Js.array_empty in
  List.iter children ~f:(function
    | None -> ()
    | (Text _ | Element _ | Widget _) as other ->
      let (_ : int) = children_raw##push (t_to_js other) in
      ());
  let raw_attrs = lazy (Attr.to_raw attrs) in
  { kind; tag; key; attrs; raw_attrs; children = children_raw }
;;

let element_expert kind ~tag ?key attrs children =
  let raw_attrs = lazy (Attr.to_raw attrs) in
  { kind; tag; key; attrs; raw_attrs; children }
;;

let text s = Text s

let widget ?info ?destroy ?update ~id ~init () =
  Widget (Widget.create ?info ?destroy ?update ~id ~init ())
;;

let create tag ?key ?(attr = Attr.empty) children =
  Element (element `Vnode ~tag ~key attr children)
;;

let create_childless tag ?key ?attr () = create tag ?key ?attr []

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
      element##.innerHTML := Js.string content;
      (debug, content), element)
    ()
;;

let inner_html_svg = inner_html (fun tag ~attr -> create_svg_monoid tag ?key:None ~attr)
let inner_html = inner_html (fun tag ~attr -> create tag ?key:None ~attr)
let a = create "a"
let body = create "body"
let button = create "button"
let code = create "code"
let div = create "div"
let main = create "main"
let fieldset = create "fieldset"
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

let sexp_for_debugging ?indent sexp =
  sexp |> Sexp.to_string_hum ?indent |> text |> List.return |> pre ~attr:Attr.empty
;;

module Patch = struct
  type t = Raw.Patch.t

  let create ~previous ~current =
    Raw.Patch.create ~previous:(t_to_js previous) ~current:(t_to_js current)
  ;;

  let apply t elt = Raw.Patch.apply elt t
  let is_empty t = Raw.Patch.is_empty t
end

module Expert = struct
  let create ?key tag attrs children =
    Element (element_expert `Vnode ?key ~tag attrs children)
  ;;

  let create_svg ?key tag attrs children =
    Element (element_expert `Svg ?key ~tag attrs children)
  ;;
end
