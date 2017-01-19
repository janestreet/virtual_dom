open Base
open Js_of_ocaml

type node

type t = node Js.t

let text (s : string) =
  let vtext = Js.Unsafe.global##.VirtualDom##.VText in
  new%js vtext (Js.string s)

type node_creator = ?key:string -> Attr.t list -> t list -> t

let create tag ?key (attrs : Attr.t list) children =
  let vnode = Js.Unsafe.global##.VirtualDom##.VNode in
  match key with
  | None ->
    new%js vnode
      (Js.string tag)
      (Attr.list_to_obj attrs)
      (Js.array (Array.of_list children))
  | Some key ->
    new%js vnode
      (Js.string tag)
      (Attr.list_to_obj attrs)
      (Js.array (Array.of_list children))
      (Js.string key)

let svg tag ?key (attrs : Attr.t list) children =
  let vnode = Js.Unsafe.global##.VirtualDom##.svg in
  match key with
  | None ->
    new%js vnode
      (Js.string tag)
      (Attr.list_to_obj attrs)
      (Js.array (Array.of_list children))
  | Some key ->
    new%js vnode
      (Js.string tag)
      (Attr.list_to_obj attrs)
      (Js.array (Array.of_list children))
      (Js.string key)

let to_dom t =
  Js.Unsafe.global##.VirtualDom##createElement t

let a        = create "a"
let body     = create "body"
let button   = create "button"
let div      = create "div"
let footer   = create "footer"
let h1       = create "h1"
let h2       = create "h2"
let h3       = create "h3"
let h4       = create "h4"
let h5       = create "h5"
let header   = create "header"
let html     = create "html"
let input    = create "input"
let textarea = create "textarea"
let select   = create "select"
let option   = create "option"
let label    = create "label"
let li       = create "li"
let p        = create "p"
let section  = create "section"
let span     = create "span"
let strong   = create "strong"
let table    = create "table"
let tbody    = create "tbody"
let td       = create "td"
let th       = create "th"
let thead    = create "thead"
let tr       = create "tr"
let ul       = create "ul"

class type ['s, 'element] widget = object
  constraint 'element = #Dom_html.element Js.t
  method type_   : Js.js_string Js.t Js.writeonly_prop
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
  method name    : unit Js.writeonly_prop
  method id      : ('s * 'element) Type_equal.Id.t Js.prop

  method state   : 's Js.prop
  method destroy : ('element -> unit) Js.callback Js.writeonly_prop
  method update  : (('other_state, 'other_element) widget Js.t -> 'element -> 'element)
                     Js.callback Js.writeonly_prop
  method init    : (unit -> 'element) Js.callback Js.writeonly_prop
end

external t_of_widget : (_, _) widget Js.t -> t = "%identity"

let widget (type s)
      ?(destroy : s -> 'element -> unit = fun _ _ -> ())
      ?(update : s -> 'element -> s * 'element = fun s elt -> (s, elt))
      ~(id : (s * 'element) Type_equal.Id.t)
      ~(init : unit -> s * 'element)
      ()
  =
  let obj : (s, _) widget Js.t = Js.Unsafe.obj [||] in
  obj##.type_ := Js.string "Widget";
  obj##.name := ();
  obj##.id := id;
  obj##.init :=
    Js.wrap_callback (fun () ->
      let (s0, dom_node) = init () in
      obj##.state := s0;
      dom_node);
  obj##.update :=
    Js.wrap_callback (fun prev dom_node ->
      (* The [update] method of [obj] is only called by virtual-dom after it has checked
         that the [id]s of [prev] and [obj] are "===" equal. Thus [same_witness_exn] will
         never raise.
      *)
      match Type_equal.Id.same_witness_exn prev##.id id with
      | Type_equal.T ->
        let (state', dom_node') = update prev##.state dom_node in
        obj##.state := state';
        dom_node');
  obj##.destroy :=
    Js.wrap_callback (fun dom_node ->
      destroy obj##.state dom_node);
  t_of_widget obj
;;

module Lazy = struct

  class type thunk = object
    method params  : Js.Unsafe.any Js.js_array Js.t Js.prop
    method thunk   : unit -> t Js.meth
    method vnode   : t Js.prop
  end

  let should_update (previous : thunk Js.t) (current : thunk Js.t) =
    let rec loop i =
      if i < 0
      then false
      else not (phys_equal (Js.array_get current##.params i) (Js.array_get previous##.params i )) || loop (i - 1)
    in
    loop (current##.params##.length - 1)
  ;;

  let thunk =
    let thunk =
      Js.Unsafe.js_expr "(function(params, thunk) { \
                         this.params = params; \
                         this.thunk  = thunk; \
                         })"
    in
    thunk##.prototype##.type_ := Js.string "Thunk";
    thunk##.prototype##.render :=
      Js.wrap_meth_callback (fun (this : thunk Js.t) (previous : thunk Js.t Js.Opt.t) ->
        Js.Opt.case previous
          (fun () -> this##thunk())
          (fun previous ->
             if should_update previous this
             then this##thunk()
             else previous##.vnode)
      );
    thunk
  ;;

  let create f x1 : t =
    let open Js.Unsafe in
    let args = Js.array [|inject f; inject x1|] in
    new%js thunk args (fun () -> f x1)
  ;;

  let create2 f x1 x2 : t =
    let open Js.Unsafe in
    let args = Js.array [|inject f; inject x1; inject x2|] in
    new%js thunk args (fun () -> f x1 x2)
  ;;

  let create3 f x1 x2 x3 : t =
    let open Js.Unsafe in
    let args = Js.array [|inject f; inject x1; inject x2; inject x3|] in
    new%js thunk args (fun () -> f x1 x2 x3)
  ;;

  let create4 f x1 x2 x3 x4 : t =
    let open Js.Unsafe in
    let args = Js.array [|inject f; inject x1; inject x2; inject x3; inject x4|] in
    new%js thunk args (fun () -> f x1 x2 x3 x4)
  ;;

  let create5 f x1 x2 x3 x4 x5 : t =
    let open Js.Unsafe in
    let args =
      Js.array
        [|inject f; inject x1; inject x2; inject x3; inject x4; inject x5|]
    in
    new%js thunk args (fun () -> f x1 x2 x3 x4 x5)
  ;;

end

module Patch = struct
  type node = t
  type t

  let create ~previous ~current =
    Js.Unsafe.global##.VirtualDom##diff previous current
  ;;

  let apply t elt =
    Js.Unsafe.global##.VirtualDom##patch elt t
  ;;

  let is_empty =
    let f =
      Js.Unsafe.pure_js_expr {js|
        (function (patch) {
          for (var key in patch) {
            if (key !== 'a') return false
          }
          return true
        })
      |js}
    in
    fun (t : t) -> Js.Unsafe.fun_call f [| Js.Unsafe.inject t |] |> Js.to_bool
end
