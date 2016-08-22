open Core_kernel.Std
open Js_of_ocaml

type t =
  | Property of string * Js.Unsafe.any
  | Attribute of string * Js.Unsafe.any

let create name value = Attribute (name, Js.Unsafe.inject (Js.string value))
let property  name value = Property (name, value)

let class_ c = create "class" c
let classes classes = class_ (String.concat classes ~sep:" ")

let id s          = create "id" s
let href r        = create "href" r
let checked s     = create "checked" s
let placeholder x = create "placeholder" x
let autofocus b   = create "autofocus" (Bool.to_string b)
let for_ x        = create "for" x
let type_ x       = create "type" x
let value x       = create "value" x

let string_property name value =
  Property (name,Js.Unsafe.inject (Js.string value))

let on event convert_to_vdom_event : t =
  let f e = Event.handle e (convert_to_vdom_event e); Js._true in
  Property ("on" ^ event, Js.Unsafe.inject (Dom.handler f))

let style props =
  let obj = Js.Unsafe.obj [||] in
  List.iter ~f:(fun (k, v) ->
    Js.Unsafe.set obj (Js.string k) (Js.string v))
    props;
  Property ("style", obj)

let style_css css =
  create "style" css

let on_focus = on "focus"
let on_blur  = on "blur"

let on_click        = on "click"
let on_double_click = on "dblclick"
let on_mousemove    = on "mousemove"
let on_mouseup      = on "mouseup"
let on_mousedown    = on "mousedown"
let on_mouseenter   = on "mouseenter"
let on_mouseleave   = on "mouseleave"
let on_mouseover    = on "mouseover"
let on_mouseout     = on "mouseout"

let on_keyup    = on "keyup"
let on_keypress = on "keypress"
let on_keydown  = on "keydown"

let const_ignore _ = Event.Ignore

class type value_element = object
  inherit Dom_html.element

  method value : Js.js_string Js.t Js.prop
end

type value_coercion = Dom_html.element Js.t -> value_element Js.t Js.opt

let run_coercion coercion target prev =
  match prev with
  | Some _ -> prev
  | None   -> Js.Opt.to_option (coercion target)

let coerce_value_element target =
  let open Dom_html.CoerceTo in
  None
  |> run_coercion (input    :> value_coercion) target
  |> run_coercion (select   :> value_coercion) target
  |> run_coercion (textarea :> value_coercion) target

let on_input_event event handler =
  on event (fun ev ->
    Js.Opt.case (ev##.target) const_ignore (fun target ->
      Option.value_map (coerce_value_element target)
        ~default:Event.Ignore
        ~f:(fun target ->
          let text = Js.to_string (target##.value) in
          handler ev text
        )
    ))

let on_change = on_input_event "change"
let on_input  = on_input_event "input"

let list_to_obj attrs =
  let attrs_obj = Js.Unsafe.obj [||] in
  List.iter ~f:(function
    | Property (name, value) ->
      Js.Unsafe.set attrs_obj
        (Js.string name)
        value
    | Attribute (name, value) ->
      if not (Js.Optdef.test attrs_obj##.attributes)
      then attrs_obj##.attributes := Js.Unsafe.obj [||];
      Js.Unsafe.set (attrs_obj##.attributes)
        (Js.string name)
        value
  )
    attrs;
  attrs_obj
