open! Js_of_ocaml
open Base

(** This type covers both properties and attributes, despite the name. *)
type t

(** [get_name] returns the attribute or property name *)
val get_name : t -> string

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

(** [create_hook name hook] creates a hook attribute with a name *)
val create_hook : string -> Hooks.t -> t

val autofocus : bool -> t


val checked : t
val class_ : string -> t
val to_class : t -> Set.M(String).t option
val classes : string list -> t
val classes' : Set.M(String).t -> t
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
val on_scroll : (Dom_html.event Js.t -> Event.t) -> t
val on_submit : (Dom_html.submitEvent Js.t -> Event.t) -> t
val on_pointerdown : (Dom_html.pointerEvent Js.t -> Event.t) -> t

val on_mousewheel : (Dom_html.mousewheelEvent Js.t -> Event.t) -> t
val on_copy : (Dom_html.clipboardEvent Js.t -> Event.t) -> t
val on_cut : (Dom_html.clipboardEvent Js.t -> Event.t) -> t
val on_paste : (Dom_html.clipboardEvent Js.t -> Event.t) -> t
val on_reset : (Dom_html.event Js.t -> Event.t) -> t


module Always_focus_hook : sig
  (* This hook always causes the element to which it is attached to become
     focused when the element is attached to the DOM. This may behave
     unpredictably, since elements which are moved from one part of a page
     may be removed and re-inserted from the DOM, thus causing this attribute
     to steal back focus *)
  val attr : [ `Read_the_docs__this_hook_is_unpredictable ] -> t
end

module Single_focus_hook () : sig
  (* A hook that makes the element it is attached to become focused immediately
     after the attribute is applied. Afterward, the attribute has no effect on
     the element or any other element.

     Since an element in the virtual dom can be removed and re-inserted into
     the DOM, a hook which focuses an element every time its [on_mount]
     function is called will not have the right behavior. Elements which move
     from one part of the tree to another may unexpectedly gain focus.

     To avoid this problem, we treat each instance of the attribute as
     distinct from all the other instances. Each individual focus attribute
     can only be used once before it is deactivated which prevents it
     from causing any elements from being focused again.

     This functor creates a new instance of a focus attribute. Since each
     functor instantiation is distinct, avoid calling this from within an
     Incremental graph, or you will not get the desired effect. *)

  val attr : [ `Read_the_docs__this_hook_is_unpredictable ] -> after:Ui_event.t -> t
end
