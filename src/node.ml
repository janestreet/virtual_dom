open Base
open Js_of_ocaml

type element =
  { tag : Js.js_string Js.t
  ; key : string option
  ; attrs : Attr.t
  ; raw_attrs : Raw.Attrs.t Lazy.t
  ; children : Raw.Node.t Js.js_array Js.t
  ; kind : [ `Vnode | `Svg ]
  }

and widget = Raw.Widget.t

and t =
  | None
  | Fragment of t list
  | Text of string
  | Element of element
  | Widget of widget
  | Lazy of
      { key : string option
      ; t : t Lazy.t
      }

module Aliases = struct
  type node_creator = ?key:string -> ?attrs:Attr.t list -> t list -> t
  type node_creator_childless = ?key:string -> ?attrs:Attr.t list -> unit -> t
end

module Element = struct
  type t = element

  let tag t = Js.to_string t.tag
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

let to_raw_children children ~t_to_js =
  let children_raw = new%js Js.array_empty in
  let rec append_children children =
    List.iter children ~f:(function
      | None -> ()
      | Fragment children -> append_children children
      | (Text _ | Element _ | Widget _ | Lazy _) as other ->
        let (_ : int) = children_raw##push (t_to_js other) in
        ())
  in
  append_children children;
  children_raw
;;

(* We emit a warning when a fragment is located in an invalid vdom location. This ref is
   to ensure we don't spam the console. *)
let has_emitted_fragment_warning = ref false

let rec t_to_js = function
  | None ->
    (* We normally filter these out, but if [to_js] is called directly on a [None] node,
       we use this hack. Aside from having a [Text] node without any text present in the
       Dom, there should be no unwanted side-effects. In an Incr_dom application, this can
       only happen when the root view Incremental is inhabited by a [None]. *)
    Raw.Node.text ""
  | Fragment children ->
    if not (Core.am_running_test || !has_emitted_fragment_warning)
    then (
      Console.console##error
        [%string
          {|Vdom.Node.to_dom was given a Vdom.Node.fragment, which contains multiple nodes.
            In order to produce a single dom node, we are wrapping the fragment children in a <div>|}];
      has_emitted_fragment_warning := true);
    let attrs = Attr.to_raw Attr.empty in
    let key : string option = None in
    let children_raw = to_raw_children children ~t_to_js in
    Raw.Node.node (Js.string "div") attrs children_raw key
  | Text s -> Raw.Node.text s
  | Element { tag; key; attrs = _; raw_attrs = (lazy raw_attrs); children; kind = `Vnode }
    -> Raw.Node.node tag raw_attrs children key
  | Element { tag; key; attrs = _; raw_attrs = (lazy raw_attrs); children; kind = `Svg }
    -> Raw.Node.svg tag raw_attrs children key
  | Widget w -> w
  | Lazy { t; key } -> Thunk.create ~key t ~f:t_to_js_lazy

and t_to_js_lazy (lazy t) = t_to_js t

let text s = Text s

let element kind ~tag ~key attrs children =
  let children_raw = to_raw_children children ~t_to_js in
  let raw_attrs = lazy (Attr.to_raw attrs) in
  { kind; tag; key; attrs; raw_attrs; children = children_raw }
;;

let create tag =
  let tag = Js.string tag in
  fun ?key ?(attrs = []) children ->
    Element (element `Vnode ~tag ~key (Attr.many attrs) children)
;;

module Widget = struct
  open Js_of_ocaml

  let create_element = create

  type vdom_node = t

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
    val to_vdom_for_testing : [ `Custom of Input.t -> vdom_node | `Sexp_of_input ]
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
      let vdom_for_testing =
        lazy
          (t_to_js
             (match M.to_vdom_for_testing with
              | `Custom f -> f input
              | `Sexp_of_input ->
                let children =
                  match M.Input.sexp_of_t input with
                  | Atom "<opaque>" -> []
                  | other -> [ text (Sexp.to_string_hum other) ]
                in
                create_element (M.name ^ "-widget") children))
      in
      create
        ~id
        ~vdom_for_testing
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

let lazy_ ?key t = Lazy { key; t }

let element_expert kind ~tag ?key attrs children =
  let raw_attrs = lazy (Attr.to_raw attrs) in
  { kind; tag; key; attrs; raw_attrs; children }
;;

let widget ?vdom_for_testing ?destroy ?update ~id ~init () =
  let vdom_for_testing =
    lazy
      (match vdom_for_testing with
       | Some t -> t_to_js (Lazy.force t)
       | None -> t_to_js (create (Type_equal.Id.name id ^ "-widget") []))
  in
  Widget (Widget.create ~vdom_for_testing ?destroy ?update ~id ~init ())
;;

let create_childless tag =
  let create = create tag in
  fun ?key ?attrs () -> create ?key ?attrs []
;;

let create_svg tag ?key ?(attrs = []) children =
  let tag = Js.string tag in
  Element (element `Svg ~tag ~key (Attr.many attrs) children)
;;

let create_svg_monoid tag ?key ?(attrs = []) children =
  Element (element `Svg ~tag ~key (Attr.many attrs) children)
;;

let none_deprecated = None

let none =
  let comment_content = Js_of_ocaml.Js.string " Vdom.Node.none " in
  let id = Type_equal.Id.create ~name:"Vdom.Node.none" Core.sexp_of_opaque in
  widget
    ~destroy:(fun _ _ -> ())
    ~update:(fun () e -> (), e)
    ~id
    ~init:(fun () ->
      let comment = Js_of_ocaml.Dom_html.document##createComment comment_content in
      (* The widget signature wants to produce an element node (which almost always makes
         sense), but here we have a coment node. The underlying virtual dom library is ok
         with all node types, so it's safe to lie here. *)
      let element = Stdlib.Obj.magic comment in
      (), element)
    ()
;;

let of_opt = function
  | Option.None -> none
  | Some node -> node
;;

let fragment children = Fragment children
let textf format = Printf.ksprintf text format

let widget_of_module m =
  let f = Base.Staged.unstage (Widget.of_module m) in
  Base.Staged.stage (fun i -> Widget (f i))
;;

let to_raw = t_to_js
let to_dom t = Raw.Node.to_dom (to_raw t)

module Inner_html = struct
  let update'
    ~create
    ~prev:(prev_content, prev_tag, prev_attr)
    ~next:(content, tag, attrs)
    ~element
    =
    let element =
      (* if the tag or the attributes are different, do a diff/patch cycle to get it up to
         date *)
      if (not (String.equal prev_tag tag)) || not (phys_equal prev_attr attrs)
      then
        Raw.Patch.create
          ~previous:(create prev_tag ~attrs:prev_attr [] |> to_raw)
          ~current:(create tag ~attrs [] |> to_raw)
        |> Raw.Patch.apply element
      else element
    in
    (* if the tag changed, then [element] will be empty, so we need to update the
       innerHTML. If the content changed, then we need to set the innerHTML for obvious
       reasons. *)
    if (not (String.equal prev_tag tag)) || not (String.equal prev_content content)
    then element##.innerHTML := Js.string content;
    (content, tag, attrs), element
  ;;

  let widget ~name create =
    let id =
      (* stage the id generation *)
      Type_equal.Id.create ~name (fun ((content, _, _), _) -> Sexp.Atom content)
    in
    Staged.stage
      (fun
          ?override_vdom_for_testing
          ~tag
          ~attrs
          ~this_html_is_sanitized_and_is_totally_safe_trust_me:content
          ()
        ->
         let element = create tag ~attrs [] in
         let init () =
           let element = to_dom element in
           element##.innerHTML := Js.string content;
           (content, tag, attrs), element
         in
         let update prev element =
           update' ~create ~prev ~next:(content, tag, attrs) ~element
         in
         let destroy prev element =
           let _state, _elem = update' ~create ~prev ~next:("", "div", []) ~element in
           ()
         in
         let vdom_for_testing =
           match override_vdom_for_testing with
           | None -> lazy (create tag ~attrs [ text content ])
           | Some v -> v
         in
         (* We use the [widget] function directly, rather than through the easier-to-use
            [widget_of_module] function because we want to explicitly create the id such
            that it is distinct between [inner_html] and [inner_html_svg]. *)
         widget ~id ~vdom_for_testing ~init ~update ~destroy ())
  ;;
end

let inner_html_svg =
  Inner_html.widget ~name:"inner-html-svg-node" (fun tag ~attrs ->
    create_svg_monoid (Js.string tag) ?key:None ~attrs)
  |> Staged.unstage
;;

let inner_html =
  Inner_html.widget ~name:"inner-html-node" (fun tag ~attrs ->
    create tag ?key:None ~attrs)
  |> Staged.unstage
;;

let a = create "a"
let abbr = create "abbr"
let address = create "address"
let area = create_childless "area"
let article = create "article"
let aside = create "aside"
let audio = create "audio"
let b = create "b"
let bdi = create "bdi"
let bdo = create "bdo"
let blockquote = create "blockquote"
let body = create "body"
let br = create_childless "br"
let button = create "button"
let canvas = create "canvas"
let caption = create "caption"
let cite = create "cite"
let code = create "code"
let col = create_childless "col"
let colgroup = create "colgroup"
let data = create "data"
let datalist = create "datalist"
let dd = create "dd"
let del = create "del"
let details = create "details"
let dfn = create "dfn"
let dialog = create "dialog"
let div = create "div"
let dl = create "dl"
let dt = create "dt"
let em = create "em"
let fieldset = create "fieldset"
let figcaption = create "figcaption"
let figure = create "figure"
let footer = create "footer"
let form = create "form"
let h1 = create "h1"
let h2 = create "h2"
let h3 = create "h3"
let h4 = create "h4"
let h5 = create "h5"
let h6 = create "h6"
let head = create "head"
let header = create "header"
let hgroup = create "hgroup"
let hr = create_childless "hr"
let html = create "html"
let i = create "i"
let iframe = create "iframe"
let img = create_childless "img"
let input = create_childless "input"
let ins = create "ins"
let kbd = create "kbd"
let label = create "label"
let legend = create "legend"
let li = create "li"
let map = create "map"
let mark = create "mark"
let main = create "main"
let menu = create "menu"
let meter = create "meter"
let nav = create "nav"
let noscript = create "noscript"
let ol = create "ol"
let optgroup = create "optgroup"
let option = create "option"
let output = create "output"
let p = create "p"
let picture = create "picture"
let pre = create "pre"
let progress = create "progress"
let q = create "q"
let rp = create "rp"
let rt = create "rt"
let ruby = create "ruby"
let samp = create "samp"
let search = create "search"
let section = create "section"
let select = create "select"
let slot = create "slot"
let source = create "source"
let small = create "small"
let span = create "span"
let strong = create "strong"
let sub = create "sub"
let summary = create "summary"
let sup = create "sup"
let table = create "table"
let tbody = create "tbody"
let td = create "td"
let template = create "template"
let textarea = create "textarea"
let tfoot = create "tfoot"
let th = create "th"
let thead = create "thead"
let time = create "time"
let tr = create "tr"
let track = create_childless "track"
let u = create "u"
let ul = create "ul"
let var = create "var"
let video = create "video"
let wbr = create_childless "wbr"
let input_deprecated = create "input"

let sexp_for_debugging ?indent sexp =
  sexp |> Sexp.to_string_hum ?indent |> text |> List.return |> pre ~attrs:[]
;;

module Patch = struct
  type t = Raw.Patch.t

  let create ~previous ~current =
    Raw.Patch.create ~previous:(t_to_js previous) ~current:(t_to_js current)
  ;;

  let apply t elt = Raw.Patch.apply elt t
  let is_empty t = Raw.Patch.is_empty t
end

module For_changing_dom = struct
  let with_on_mount_at_end = On_mount.with_on_mount_at_end
end

module Expert = struct
  let create ?key tag attrs children =
    let tag = Js.string tag in
    Element (element_expert `Vnode ?key ~tag attrs children)
  ;;

  let create_svg ?key tag attrs children =
    let tag = Js.string tag in
    Element (element_expert `Svg ?key ~tag attrs children)
  ;;
end
