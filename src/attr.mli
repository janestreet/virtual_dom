open! Js_of_ocaml
open Core

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

(** This function does not affect hooks, styles, classes, or [on_*] handlers,
    since warnings due to merging those can be avoided. It allows
    disabling warnings for attributes that are unmergeable. Note that no
    merging behavior is changed by this function - it only changes whether
    warnings are emitted.

    Example: If [href] is already on a node, then adding
    a [suppress_merge_warning (href input)] attribute to the node will not
    trigger a warning. However, adding another [href] which does not use
    [suppress_merge_warnings] to the node will again emit a warning. In other
    words, this function only suppresses warnings for an instance of an
    attribute, not all attributes of the same type.
 **)
val suppress_merge_warnings : t -> t

(** [create_hook name hook] creates a hook attribute with a name *)
val create_hook : string -> Hooks.t -> t

(** [of_opt attr] returns the underlying Attr.t for a Some, and Attr.empty for a None *)
val of_opt : t option -> t

(** [many] merges several attributes into one. It merges hooks, on_* event
    handlers, classes, and styles.

    - Hooks get merged via their [Input.combine] function
    - All handlers get run in the order they appear
    - The set of classes is unioned
    - Styles are merged via concatenation
*)
val many : t list -> t

(** Like [many], except instead of merging attributes of the same type, it
    takes the last one. Don't use this function - use [many] instead. *)
val many_without_merge : t list -> t

(** Equivalent to [many []]. It adds no attributes to the DOM. *)
val empty : t

(** Equivalent to [combine] *)
val ( @ ) : t -> t -> t

(** Equivalent to [many [x; y]] *)
val combine : t -> t -> t

val autofocus : bool -> t
val checked : t
val class_ : string -> t
val classes : string list -> t
val classes' : Set.M(String).t -> t
val disabled : t
val allow : string -> t
val for_ : string -> t
val label : string -> t
val href : string -> t
val target : string -> t
val id : string -> t
val name : string -> t
val placeholder : string -> t
val role : string -> t
val selected : t
val hidden : t
val readonly : t
val style : Css_gen.t -> t
val min : float -> t
val max : float -> t
val min_date : Date.t -> t
val max_date : Date.t -> t

(** Sets the min time of a datetime-local picker to any time in the specified date.
    This function receives a [Date.t] instead of a [Time_ns.t] because the
    browser doesn't actually limit the time part of a datetime-local picker,
    even if a time is specified. *)
val min_date_time : Date.t -> t

(** Similar to [min_date_time], but sets the maximum instead. *)
val max_date_time : Date.t -> t

val colspan : int -> t
val rowspan : int -> t
val draggable : bool -> t
val tabindex : int -> t
val type_ : string -> t
val value : string -> t

(* The following 2 attributes apply only to TextAreas. *)
val rows : int -> t
val cols : int -> t

(* "value" can be both an attribute and a property. *)
val value_prop : string -> t
val title : string -> t
val alt : string -> t
val src : string -> t
val open_ : t
val start : int -> t
val on_focus : (Dom_html.focusEvent Js.t -> unit Effect.t) -> t
val on_blur : (Dom_html.focusEvent Js.t -> unit Effect.t) -> t

module Unmerged_warning_mode : sig
  (** Controls whether [to_raw] should print warning messages when one attribute
      overrides another of the same name (for example, if there are two [title]
      attributes, the second will end up in the result, and a warning will be
      emitted).

      If an application never emits any warnings, it is probably safe to always
      use good merge semantics (that is, use [many] or [@] to combine lists of
      attributes) for everything. *)
  type t =
    | No_warnings
    | All_warnings
    | Stop_after_quota of int

  (** Defaults to [Stop_after_quota 100] *)
  val current : t ref

  module For_testing : sig
    val reset_warning_count : unit -> unit
  end
end

val to_raw : t -> Raw.Attrs.t

(** [on_input] fires every time the input changes, i.e., whenever a key is pressed in
    the input field.  The current contents are returned as an OCaml string as
    a convenience *)
val on_input : (Dom_html.event Js.t -> string -> unit Effect.t) -> t

(** [on_change] fires when the input is complete, i.e., when enter is pressed in the
    input field or the input field loses focus.  The current contents are returned as an
    OCaml string as a convenience *)
val on_change : (Dom_html.event Js.t -> string -> unit Effect.t) -> t

(** [on_file_input] is like [on_input] but for file picker input elements (i.e.
    [type=file]). Instead of passing the value of the input as a string, the list of
    selected files is passed.

    See Vdom_input_widgets.File_select, or, if you are a bonsai user,
    Bonsai_web_ui_form.Elements.File_select, for a convenient API that wraps this.
*)
val on_file_input : (Dom_html.event Js.t -> File.fileList Js.t -> unit Effect.t) -> t

val on_cancel : (Dom_html.event Js.t -> unit Effect.t) -> t
val on_click : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_close : (Dom_html.event Js.t -> unit Effect.t) -> t
val on_contextmenu : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_double_click : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_drag : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_dragstart : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_dragend : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_dragenter : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_dragleave : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_dragover : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_drop : (Dom_html.dragEvent Js.t -> unit Effect.t) -> t
val on_mousemove : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_mouseup : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_mousedown : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_mouseenter : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_mouseleave : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_mouseover : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_mouseout : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> t
val on_keyup : (Dom_html.keyboardEvent Js.t -> unit Effect.t) -> t
val on_keypress : (Dom_html.keyboardEvent Js.t -> unit Effect.t) -> t
val on_keydown : (Dom_html.keyboardEvent Js.t -> unit Effect.t) -> t
val on_scroll : (Dom_html.event Js.t -> unit Effect.t) -> t
val on_load : (Dom_html.event Js.t -> unit Effect.t) -> t
val on_error : (Dom_html.event Js.t -> unit Effect.t) -> t
val on_submit : (Dom_html.submitEvent Js.t -> unit Effect.t) -> t
val on_pointerdown : (Dom_html.pointerEvent Js.t -> unit Effect.t) -> t
val on_pointerup : (Dom_html.pointerEvent Js.t -> unit Effect.t) -> t
val on_mousewheel : (Dom_html.mousewheelEvent Js.t -> unit Effect.t) -> t
val on_wheel : (Js_of_ocaml_patches.Dom_html.wheelEvent Js.t -> unit Effect.t) -> t
val on_copy : (Dom_html.clipboardEvent Js.t -> unit Effect.t) -> t
val on_cut : (Dom_html.clipboardEvent Js.t -> unit Effect.t) -> t
val on_paste : (Dom_html.clipboardEvent Js.t -> unit Effect.t) -> t
val on_reset : (Dom_html.event Js.t -> unit Effect.t) -> t
val on_animationend : (Dom_html.animationEvent Js.t -> unit Effect.t) -> t

(** Sets a css named variable on the element. The "--" prefix is added by this function:
    [css_var ~name:"foo" "red"] is equivalent to the css [--foo: red]. *)
val css_var : name:string -> string -> t

(** For ppx use only *)
val __css_vars_no_kebabs : (string * string) list -> t

module Multi : sig
  (** A collection of CSS attributes. *)

  type attr := t
  type t = attr list

  (** [merge_classes_and_styles] groups together the class attributes and style attributes
      from the given list into a single style and class attribute, e.g.:

      [ class="foo"; style="color:blue"; class="bar"; id="id"; style="margin:30px;" ]

      becomes

      [ class="foo bar"; style="color:blue; margin:30px;"; id="id" ]
  *)
  val merge_classes_and_styles : t -> t

  (** If there is no style attribute the empty Css_gen.t will be passed to f.
      Most of the time you probably want to use add_style instead. *)
  val map_style : t -> f:(Css_gen.t -> Css_gen.t) -> t

  val add_style : t -> Css_gen.t -> t
  val add_class : t -> string -> t
end

module Always_focus_hook : sig
  (* CONSIDER USING [Bonsai_web.Effect.Focus.*] functions *)

  (* This hook always causes the element to which it is attached to become
     focused when the element is attached to the DOM. This may behave
     unpredictably, since elements which are moved from one part of a page
     may be removed and re-inserted from the DOM, thus causing this attribute
     to steal back focus *)
  val attr : [ `Read_the_docs__this_hook_is_unpredictable ] -> t
end

module Single_focus_hook () : sig
  (* CONSIDER USING [Bonsai_web.Effect.Focus.*] functions *)

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

  val attr : [ `Read_the_docs__this_hook_is_unpredictable ] -> after:unit Ui_effect.t -> t
end

module No_op_hook (M : sig
  module Input : Hooks.Input

  val name : string
end) : sig
  val attr : M.Input.t -> t
  val type_id : M.Input.t Type_equal.Id.t
end

module Expert : sig
  (** [contains_name] checks the attribute or group of attributes contains anything
      with the specified name.

      You probably shouldn't use this function.
  *)
  val contains_name : string -> t -> bool

  (* removes all the attributes that don't pass the predicate. *)
  val filter_by_kind
    :  t
    -> f:([> `Attribute | `Class | `Handler | `Hook | `Property | `Style ] -> bool)
    -> t
end
