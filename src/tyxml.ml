open Js_of_ocaml
open Tyxml_f

module type XML =
  Xml_sigs.T
  with type uri = string
   and type event_handler = Dom_html.event Js.t -> bool
   and type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
   and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
   and type elt = Vdom.Node.t

module Xml = struct

  module W = Xml_wrap.NoWrap
  type 'a wrap = 'a
  type 'a list_wrap = 'a list

  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s
  type aname = string

  class type biggest_event = object
    inherit Dom_html.event
    inherit Dom_html.mouseEvent
    inherit Dom_html.keyboardEvent
  end

  type biggest_event_handler = biggest_event Js.t -> bool
  type event_handler = Dom_html.event Js.t -> bool
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
  type attrib = Attr.t

  let attr name value =
    match name with
    | "value" | "checked" | "selected" ->
      Attr.property name (Js.Unsafe.inject (Js.string value))
    | name ->
      Attr.create name value

  let attr_ev name value =
    Attr.property name (Js.Unsafe.inject (fun ev -> Js.bool (value ev)))

  let float_attrib name value : attrib = attr name (string_of_float value)
  let int_attrib name value = attr name (string_of_int value)
  let string_attrib name value = attr name value
  let space_sep_attrib name values = attr name (String.concat " " values)
  let comma_sep_attrib name values = attr name (String.concat "," values)
  let event_handler_attrib name (value : event_handler) =
    attr_ev name (value :> (biggest_event_handler))
  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    attr_ev name (value :> (biggest_event_handler))
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    attr_ev name (value :> (biggest_event_handler))
  let uri_attrib name value = attr name value
  let uris_attrib name values = attr name (String.concat " " values)

  (** Element *)

  type elt = Vdom.Node.t
  type ename = string

  let make_a x = x

  let empty () = assert false
  let comment _c = assert false

  let pcdata s = Vdom.Node.text s
  let encodedpcdata s = Vdom.Node.text s
  let entity e =
    let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in
    Vdom.Node.text (Js.to_string entity)

  let leaf ?(a=[]) name =
    Vdom.Node.create name (make_a a) []
  let node ?(a=[]) name children =
    Vdom.Node.create name (make_a a) children

  let cdata s = pcdata s
  let cdata_script s = cdata s
  let cdata_style s = cdata s
end

module Xml_Svg = struct
  include Xml

  let leaf ?(a = []) name =
    Vdom.Node.svg name (make_a a) []

  let node ?(a = []) name children =
    Vdom.Node.svg name (make_a a) children
end

module Svg = Svg_f.Make(Xml_Svg)
module Html = Html_f.Make(Xml)(Svg)
