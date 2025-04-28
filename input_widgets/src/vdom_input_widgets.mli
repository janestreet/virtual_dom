open Core
open Import
open Virtual_dom.Vdom
include module type of Vdom_input_widgets_intf

(** [Decimal] can be used to create decimal [number] inputs

    Note that [of_string]/[to_string] raise if [t] is NaN, +Inf, or -Inf, since these are
    not valid decimals and cannot be used in number inputs. *)
module Decimal : Stringable.S with type t = float

module Merge_behavior : sig
  type t =
    | Merge
    (** Specifies that a widget should use [Vdom.Attr.many] when combining several
        attributes into one. *)
    | Legacy_dont_merge
    (** Specifies that a widget should use [Vdom.Attr.many_without_merge] when combining
        several attributes into one. This is legacy behavior, so new code should always
        prefer to use [Merge]. *)
end

module Validated : sig
  (* A type implementing storage for a text field that can be interpreted as ['a].

     It can be stored in incr_dom model and provided to [Entry.validate] to get an input
     field that is parsed as ['a], but retains any unparsable value, and also remembers
     last parsed value.
  *)
  type 'a t [@@deriving equal, sexp, bin_io, compare]

  (** The [update] type is provided when a user changes the input in an entry. It cannot
      be used on its own, but it can be applied to an existing [t] to access the new
      current or last values. *)
  type 'a update [@@deriving equal, sexp, bin_io, compare]

  (** [initial_empty] is a special value that, when provided to an [Entry.validate]
      widget, will reset the contents regardless of whether the user has focus or not. *)
  val initial_empty : 'a t

  val is_initial_empty : _ t -> bool
  val return : 'a -> 'a t
  val get_current : 'a t -> 'a option
  val get_last : 'a t -> 'a option
  val get_error : 'a t -> string option
  val update : 'a t -> 'a update -> 'a t

  val lift
    :  (module Stringable with type t = 'a)
    -> (module Stringable with type t = 'a t)
end

(** For checkboxes and radio buttons, you can choose between having their visual display
    be the default native rendering, or if you want them to look like actual buttons, then
    the input element is hidden, which gives you much better control over the styling. *)
module Selectable_style : sig
  type t =
    | Native
    | Button_like
end

module Dropdown : sig
  (** Creates a dropdown that automatically updates when the current value changes and
      emits typed actions when the user selects a different item. *)
  val of_values
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?extra_option_attrs:('a -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?key:string
    -> (module Equal with type t = 'a)
    -> 'a list
    -> selected:'a
    -> on_change:('a -> unit Effect.t)
    -> Node.t

  (** Same as [of_values], but includes a blank first entry to represent [None]. *)
  val of_values_opt
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?extra_option_attrs:('a -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?placeholder:string (** default "" *)
    -> ?key:string
    -> (module Equal with type t = 'a)
    -> 'a list
    -> selected:'a option
    -> on_change:('a option -> unit Effect.t)
    -> Node.t

  (** Instead of passing individual [values], uses [all] from the module to determine the
      items and order. *)
  val of_enum
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?extra_option_attrs:('a -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?key:string
    -> (module Enum with type t = 'a)
    -> selected:'a
    -> on_change:('a -> unit Effect.t)
    -> Node.t

  (** Instead of passing individual [values], uses [all] from the module to determine the
      items and order. Include a blank entry to represent [None] *)
  val of_enum_opt
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?extra_option_attrs:('a -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?placeholder:string (** default "" *)
    -> ?key:string
    -> (module Enum with type t = 'a)
    -> selected:'a option
    -> on_change:('a option -> unit Effect.t)
    -> Node.t
end

module Checkbox : sig
  val simple
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?key:string
    -> is_checked:bool
    -> label:string
    -> on_toggle:unit Effect.t
    -> unit
    -> Node.t
end

module Checklist : sig
  (** Creates a list of checkboxes with labels. *)
  val of_values
    :  ?style:Selectable_style.t (** default [Native] *)
    -> ?extra_container_attrs:Attr.t list (** default empty *)
    -> ?extra_checkbox_attrs:(checked:bool -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?layout:[ `Vertical | `Horizontal ] (** default `Vertical *)
    -> ?merge_behavior:Merge_behavior.t
    -> (module Display with type t = 'a)
    -> 'a list
    -> is_checked:('a -> bool)
    -> on_toggle:('a -> unit Effect.t)
    -> Node.t

  (** Instead of passing individual [values], uses [all] from the module to determine the
      items and order. *)
  val of_enum
    :  ?style:Selectable_style.t (** default [Native] *)
    -> ?extra_container_attrs:Attr.t list (** default empty *)
    -> ?extra_checkbox_attrs:(checked:bool -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> (module Enum with type t = 'a)
    -> is_checked:('a -> bool)
    -> on_toggle:('a -> unit Effect.t)
    -> Node.t
end

module Multi_select : sig
  (** Creates a multiple-selection list. This is different from Checklist in that the
      <select multiple> element is used underneath, rather than independent checkboxes. As
      a result:

      - Clicking an element makes it the only one selected.
      - Ctrl+clicking an element toggles it independently.
      - Shift+clicking an element selects everything between it and the previous click.
      - Clicking and dragging selects a range. *)

  module Repeated_click_behavior : sig
    (** When a single element is already selected and you click it again, you can
        configure a special behavior. *)
    type t =
      | No_action
      | Clear_all
      | Select_all
  end

  val of_values
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?repeated_click_behavior:Repeated_click_behavior.t (** default [No_action] *)
    -> ?disabled:bool (** default false *)
    -> ?size:int (** default: length of the values list *)
    -> ?merge_behavior:Merge_behavior.t
    -> (module Set with type t = 'a and type comparator_witness = 'cmp)
    -> 'a list
    -> selected:('a, 'cmp) Set.t
    -> on_change:(('a, 'cmp) Set.t -> unit Effect.t)
    -> Node.t

  val of_enum
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?repeated_click_behavior:Repeated_click_behavior.t (** default [No_action] *)
    -> ?disabled:bool (** default false *)
    -> ?size:int (** default: number of enum values *)
    -> ?merge_behavior:Merge_behavior.t
    -> (module Enum_set with type t = 'a and type comparator_witness = 'cmp)
    -> selected:('a, 'cmp) Set.t
    -> on_change:(('a, 'cmp) Set.t -> unit Effect.t)
    -> Node.t
end

module Entry : sig
  module Call_on_input_when : sig
    type t =
      | Text_changed
      | Enter_key_pressed_or_focus_lost
  end

  (** Creates a text input where the [value] is *exactly* what is displayed in the box. If
      you change [value], it will change the value out from under the user, even if they
      have the box focused. This is one solution for specific use-cases where pressing
      [Enter] will submit the value and you want to clear the box as a result.

      [on_return] is called when the user presses the enter key with the text box focused. *)
  val raw
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?on_return:(unit -> unit Effect.t) (** default no-op *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?key:string
    -> value:string
    -> on_input:(string -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a text input of some serializable type. If [of_string] raises an exception,
      [None] is returned. *)
  val of_stringable
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> (module Stringable.S with type t = 'a)
    -> value:'a option
    -> on_input:('a option -> unit Effect.t)
    -> Node.t

  (** Creates a text input of a serializable type, wrapping it in a type that stores
      either a valid input or last invalid string.

      Note that you usually need to use Validated.merge in your action handler.

      To provide visual feedback to user, use css with selector on aria-invalid, e.g.
      {v
        input[aria-invalid="true"] {
          bg-color: red;
        }
      v}

      If you ever change [~value] to be [Validated.initial_empty], it will clear the
      contents of the box regardless of whether the user has focus. *)
  val validated
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?on_return:(unit -> unit Effect.t) (** default no-op *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> (module Stringable with type t = 'a)
    -> value:'a Validated.t
    -> on_input:('a Validated.update -> unit Effect.t)
    -> Node.t

  (** Creates a text input that equates an empty input with [None] and a non-empty input
      as [Some string]. *)
  val text
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> value:string option
    -> on_input:(string option -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a password input that equates an empty input with [None] and a non-empty
      input as [Some string]. *)
  val password
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> value:string option
    -> on_input:(string option -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a number input that equates an empty input with [None] and a non-empty input
      as [Some 'a]. Because number input values are represented as strings in HTML, you
      can use any module that deserializes integral values. If [of_string] raises an
      exception, [None] is returned. Note that [step] controls how up/down
      increments/decrements the value and does not enforce that the value is a multiple of
      [step].

      You cannot use [Float] because of the way the default [to_string] adds trailing
      periods to whole numbers like "0.". Use [Decimal] exported from this library
      instead. *)
  val number
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> (module Stringable.S with type t = 'a)
    -> value:'a option
    -> step:float
    -> on_input:('a option -> unit Effect.t)
    -> Node.t

  (** A slider bar. Roughly equivalent to [number], but with a different appearance. We
      still need to take a [Stringable.S] interface because the browser only gives us
      values as strings. *)
  val range
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> (module Stringable.S with type t = 'a)
    -> value:'a option
    -> step:float
    -> on_input:('a option -> unit Effect.t)
    -> Node.t

  (** Creates a time input that equates an empty input with [None] and a non-empty input
      as [Some Time_ns.Ofday.t]. *)
  val time
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> value:Time_ns.Ofday.t option
    -> on_input:(Time_ns.Ofday.t option -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a date input that equates an empty input with [None] and a non-empty input
      as [Some Date.t]. *)
  val date
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> value:Date.t option
    -> on_input:(Date.t option -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a time input that equates an empty input with [None] and a non-empty input
      as [Some Time_ns.t].

      Because the underlying datepicker expects the datetime_local to be specified in the
      format yyyy-MM-ddThh:mm with optional ":ss" or ":ss.SSS" when using this widget a
      timezone should be specified. *)
  val datetime_local
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?utc_offset:Time_ns.Span.t
         (** If blank the browser local timezone is used. Max accuracy 1h. *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> value:Time_ns.t option
    -> on_input:(Time_ns.t option -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a textarea input without a [None] representation. *)
  val text_area
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?placeholder:string (** default blank *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
         (** Whether or not to ignore changes to the value when the user is focused on the
             input element. Defaults to [`Always] *)
    -> ?key:string
    -> value:string
    -> on_input:(string -> unit Effect.t)
    -> unit
    -> Node.t

  (** Creates a color input that produces values are of the form (`Hex color) where
      [color] is a hexadecimal string like "#ffcc00". Note that using
      [~call_on_input_when:Text_changed] fires the [on_input] callback whenever a new
      color is selected. *)
  val color_picker
    :  ?extra_attr:Attr.t (** default Attr.empty *)
    -> ?call_on_input_when:Call_on_input_when.t (** default [Text_changed] *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> ?key:string
    -> value:[ `Hex of string ]
    -> on_input:([> `Hex of string ] -> unit Effect.t)
    -> unit
    -> Node.t
end

module Button : sig
  (** Though [simple] is fairly trivial, it is provided as a convenient parallel to
      [with_validation] *)
  val simple
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?merge_behavior:Merge_behavior.t
    -> string
    -> on_click:(unit -> unit Effect.t)
    -> Node.t

  (** Takes a [('a, string) Result.t] that is typically the result of some validation
      performed incrementally. If the validation failed, the button will be disabled and
      will show a tooltip on hover with the error reason. If the validation is ok, the
      ['a] is passed to the [on_click] handler. You can compose multiple validity checks
      with the [Result] applicative interface. *)
  val with_validation
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?merge_behavior:Merge_behavior.t
    -> string
    -> validation:('a, string) Result.t
    -> on_click:('a -> unit Effect.t)
    -> Node.t
end

module Radio_buttons : sig
  (** Creates a radio button input with a button for each of the corresponding [values]
      passed in. Extra attrs for individual buttons can be passed through
      [extra_element_attrs], which gets access to whether the current element is selected
      or not. *)
  val of_values
    :  ?extra_container_attrs:Attr.t list (** default empty *)
    -> ?extra_button_attrs:(checked:bool -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?style:Selectable_style.t (** default [Native] *)
    -> ?merge_behavior:Merge_behavior.t
    -> (module Equal with type t = 'a)
    -> name:string
    -> on_click:('a -> unit Effect.t)
    -> selected:'a option
    -> 'a list
    -> Node.t

  (** Identical to [of_values], but adds some style tags so that the list is displayed
      horizontally instead of vertically. *)
  val of_values_horizontal
    :  ?extra_container_attrs:Attr.t list (** default empty *)
    -> ?extra_button_attrs:(checked:bool -> Attr.t list) (** default empty *)
    -> ?disabled:bool (** default false *)
    -> ?style:Selectable_style.t (** default [Native] *)
    -> ?merge_behavior:Merge_behavior.t
    -> (module Equal with type t = 'a)
    -> name:string
    -> on_click:('a -> unit Effect.t)
    -> selected:'a option
    -> 'a list
    -> Node.t
end

module File_select : sig
  (** Elements for selecting files from local disk

      NOTE: File inputs are unique in that they cannot be set programmatically, other than
      setting them to empty. Therefore, they don't take a "current value" parameter and
      instead act more like uncontrolled components. *)

  val single
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> ?merge_behavior:Merge_behavior.t
         (** Restrict the user's choice to certain files, by extension or by mimetype. *)
    -> on_input:(Js_of_ocaml.File.file Js.t option -> unit Effect.t)
    -> unit
    -> Node.t

  val list
    :  ?extra_attrs:Attr.t list (** default empty *)
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> ?merge_behavior:Merge_behavior.t
         (** Restrict the user's choice to certain files, by extension or by mimetype. *)
    -> on_input:(Js_of_ocaml.File.file Js.t list -> unit Effect.t)
    -> unit
    -> Node.t
end
