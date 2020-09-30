open! Js_of_ocaml

(** This type covers both properties and attributes, despite the name. *)
type t

(** [create name value] creates a simple string-only attribute *)
val create : string -> string -> t

(** [create_float name float] creates a simple float-only attribute *)
val create_float : string -> float -> t

(** [string_property name value] creates a simple string-only property *)
val string_property : string -> string -> t

(** [bool_property name value] creates a simple bool-only property *)
val bool_property : string -> bool -> t

(** [property name value] creates a property with a generic value *)
val property : string -> Js.Unsafe.any -> t

val on : string -> (#Dom_html.event Js.t -> Event.t) -> t
val autofocus : bool -> t


val checked : t
val class_ : string -> t
val to_class : t -> Base.Set.M(Base.String).t option
val classes : string list -> t
val classes' : Base.Set.M(Base.String).t -> t
val disabled : t
val for_ : string -> t
val href : string -> t
val id : string -> t
val name : string -> t
val placeholder : string -> t
val selected : t
val hidden : t
val style : Css_gen.t -> t
val min : float -> t
val max : float -> t

(** [to_style (style c) = Some c], [None] otherwise *)
val to_style : t -> Css_gen.t option

val tabindex : int -> t
val type_ : string -> t
val value : string -> t
val title : string -> t
val src : string -> t
val on_focus : (Dom_html.focusEvent Js.t -> Event.t) -> t
val on_blur : (Dom_html.focusEvent Js.t -> Event.t) -> t
val to_raw : t list -> Raw.Attrs.t

(** [on_input] fires every time the input changes, i.e., whenever a key is pressed in
    the input field.  The current contents are returned as an OCaml string as
    a convenience *)
val on_input : (Dom_html.event Js.t -> string -> Event.t) -> t

(** [on_change] fires when the input is complete, i.e., when enter is pressed in the
    input field or the input field loses focus.  The current contents are returned as an
    OCaml string as a convenience *)
val on_change : (Dom_html.event Js.t -> string -> Event.t) -> t

val on_click : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_contextmenu : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_double_click : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mousemove : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mouseup : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mousedown : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mouseenter : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mouseleave : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mouseover : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_mouseout : (Dom_html.mouseEvent Js.t -> Event.t) -> t
val on_keyup : (Dom_html.keyboardEvent Js.t -> Event.t) -> t
val on_keypress : (Dom_html.keyboardEvent Js.t -> Event.t) -> t
val on_keydown : (Dom_html.keyboardEvent Js.t -> Event.t) -> t

module Expert : sig
  val create_basic_hook
    :  string
    -> ?hook:(Dom_html.element Js.t -> unit)
    -> ?unhook:(Dom_html.element Js.t -> unit)
    -> unit
    -> t
  [@@deprecated
    "[since 2019-05] Do not use.  This API is in beta and I _will_ break you."]

  val create_stateful_hook
    :  string
    -> hook:(Dom_html.element Js.t -> 'a)
    -> unhook:('a -> Dom_html.element Js.t -> unit)
    -> id:'a Core_kernel.Type_equal.Id.t
    -> t
  [@@deprecated
    "[since 2019-05] Do not use.  This API is in beta and I _will_ break you."]

  val create_persistent_hook
    :  string
    -> init:(Dom_html.element Js.t -> 'state)
    -> update:('state -> Dom_html.element Js.t -> 'state)
    -> destroy:('state -> Dom_html.element Js.t -> unit)
    -> id:'state Core_kernel.Type_equal.Id.t
    -> t
  [@@deprecated
    "[since 2019-05] Do not use.  This API is in beta and I _will_ break you."]
end

