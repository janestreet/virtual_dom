(** A set of functions to generate css declaration lists.  This library can be used
    to programmatically produce strings suitable for the HTML style attribute,
    e.g. style="display:flex;background-color:red". *)

open Core

type css_global_values =
  [ `Inherit
  | `Initial
  ]
[@@deriving sexp, compare]

module Color : sig
  module RGBA : sig
    type t [@@deriving sexp, bin_io, compare]

    (** [create ~r ~g ~b ~a] creates a color that corresponds to rgba([r],[g],[b],[a])

        If [a] is omitted then it creates a color that corresponds to rgb([r],[g],[b])
    *)
    val create : r:int -> g:int -> b:int -> ?a:Percent.t -> unit -> t
  end

  module HSLA : sig
    type t [@@deriving sexp, bin_io, compare]

    (** [create ~h ~s ~l ~a] creates a color that corresponds to hsla([h],[s],[l],[a])

        If [a] is omitted then it creates a color that corresponds to hsl([h],[s],[l])
    *)

    val create : h:int -> s:Percent.t -> l:Percent.t -> ?a:Percent.t -> unit -> t
  end

  type t =
    [ `RGBA of RGBA.t
    | `HSLA of HSLA.t
    | `Name of string
    | `Hex of string
    | `Var of string
    | css_global_values
    ]
  [@@deriving sexp, bin_io, compare]

  val to_string_css : [< t ] -> string
end

module Length : sig
  type t =
    [ `Raw of string
    | `Ch of float
    | `Rem of float
    | `Em of int
    | `Em_float of float
    | `Percent of Percent.t
    | `Pt of float
    | `Px of int
    | `Px_float of float
    | `Vh of Percent.t
    | `Vw of Percent.t
    | css_global_values
    ]
  [@@deriving sexp, compare]

  (** Convenience around `Percent (Percent.of_percentage 100.) *)
  val percent100 : t

  val to_string_css : t -> string
end

module Auto_or_length : sig
  type t =
    [ `Auto
    | Length.t
    ]
  [@@deriving sexp, compare]

  val to_string_css : t -> string
end

type t [@@deriving sexp, compare, bin_io]


(** Create a single property, value pair (a declaration in CSS parlance).
    The value must be a valid CSS literal.  We do run a simple CSS parser on the value
    to validate this and will throw an exception if that parser fails.  Note that
    the parser is less forgiving than many browsers.  That is browsers will silently
    accept or drop many illegal constructs.  We prefer to raise on them, so that
    errors are detected earlier.

    It is recommended to use one of the other constructors instead if they are
    available.  If they are not, consider adding them to this library. *)
val create : field:string -> value:string -> t

val empty : t
val is_empty : t -> bool

(** Set the position attribute and optionally top, bottom,left,right
    Note that left and top have no effect when position is `Static. *)
val position
  :  ?top:Length.t
  -> ?bottom:Length.t
  -> ?left:Length.t
  -> ?right:Length.t
  -> [ `Static | `Absolute | `Sticky | `Relative | `Fixed ]
  -> t

(** Add the top property alone. *)
val top : Length.t -> t

(** Add the bottom property alone. *)
val bottom : Length.t -> t

(** Add the left property alone. *)
val left : Length.t -> t

(** Add the right property alone. *)
val right : Length.t -> t

(** Neither [combine] nor [concat] validate that each [t] is unique.
    For [combine x y], [y] will override [x] if they are the same attribute.
    For [concat l], the greatest index of an attribute will prevail. *)
val combine : t -> t -> t

val ( @> ) : t -> t -> t
val concat : t list -> t
val to_string_list : t -> (string * string) list
val to_string_css : t -> string

(** The inverse of to_string_css.  Primarily useful if you want to reuse a css
    literal from the web (aka copy paste web design). Raises if the string
    fails validation.  See create for comments on the validation we do. *)
val of_string_css_exn : string -> t

val box_sizing : [ `Content_box | `Border_box | css_global_values ] -> t

val display
  :  [ `Inline
     | `Block
     | `Inline_block
     | `List_item
     | `Table
     | `Inline_table
     | `None
     | `Inline_grid
     | css_global_values
     ]
  -> t

val visibility : [ `Visible | `Hidden | `Collapse | css_global_values ] -> t

type overflow =
  [ `Visible
  | `Hidden
  | `Scroll
  | `Auto
  | css_global_values
  ]

val overflow : overflow -> t
val overflow_x : overflow -> t
val overflow_y : overflow -> t
val z_index : int -> t
val opacity : float -> t

type font_style =
  [ `Normal
  | `Italic
  | `Oblique
  | css_global_values
  ]

type font_weight =
  [ `Normal
  | `Bold
  | `Bolder
  | `Lighter
  | `Number of int
  | css_global_values
  ]

type font_variant =
  [ `Normal
  | `Small_caps
  | css_global_values
  ]

val font_size : Length.t -> t
val font_family : string list -> t
val font_style : font_style -> t
val font_weight : font_weight -> t
val font_variant : font_variant -> t

val font
  :  size:Length.t
  -> family:string list
  -> ?style:font_style
  -> ?weight:font_weight
  -> ?variant:font_variant
  -> unit
  -> t

val bold : t

(* Note: css gradients are actually much more complicated. Please feel free to extend
   these if you need something more *)
type stops = (Percent.t * Color.t) list

type linear_gradient =
  { direction : [ `Deg of int ]
  ; stops : stops
  }

type radial_gradient = { stops : stops }

type background_image =
  [ `Url of string
  | `Linear_gradient of linear_gradient
  | `Radial_gradient of radial_gradient
  ]

val create_with_color : field:string -> color:[< Color.t ] -> t
val color : [< Color.t ] -> t
val background_color : [< Color.t ] -> t
val background_image : background_image -> t
val fill : Color.t -> t
val text_align : [ `Left | `Right | `Center | `Justify | css_global_values ] -> t
val horizontal_align : [ `Left | `Right | `Center | css_global_values ] -> t
val vertical_align : [ `Top | `Bottom | `Middle | `Super | `Sub | css_global_values ] -> t

val white_space
  :  [ `Normal | `Nowrap | `Pre | `Pre_line | `Pre_wrap | css_global_values ]
  -> t

val float : [ `None | `Left | `Right | css_global_values ] -> t
val width : Length.t -> t
val min_width : Length.t -> t
val max_width : Length.t -> t
val height : Length.t -> t
val min_height : Length.t -> t
val max_height : Length.t -> t
val padding_top : Length.t -> t
val padding_bottom : Length.t -> t
val padding_left : Length.t -> t
val padding_right : Length.t -> t
val uniform_padding : Length.t -> t

val padding
  :  ?top:Length.t
  -> ?bottom:Length.t
  -> ?left:Length.t
  -> ?right:Length.t
  -> unit
  -> t

val margin_top : Auto_or_length.t -> t
val margin_bottom : Auto_or_length.t -> t
val margin_left : Auto_or_length.t -> t
val margin_right : Auto_or_length.t -> t
val uniform_margin : Auto_or_length.t -> t

val margin
  :  ?top:Auto_or_length.t
  -> ?bottom:Auto_or_length.t
  -> ?left:Auto_or_length.t
  -> ?right:Auto_or_length.t
  -> unit
  -> t

type border_style =
  [ `None
  | `Hidden
  | `Dotted
  | `Dashed
  | `Solid
  | `Double
  | `Groove
  | `Ridge
  | `Inset
  | `Outset
  | css_global_values
  ]

val border_top : ?width:Length.t -> ?color:[< Color.t ] -> style:border_style -> unit -> t

val border_bottom
  :  ?width:Length.t
  -> ?color:[< Color.t ]
  -> style:border_style
  -> unit
  -> t

val border_left
  :  ?width:Length.t
  -> ?color:[< Color.t ]
  -> style:border_style
  -> unit
  -> t

val border_right
  :  ?width:Length.t
  -> ?color:[< Color.t ]
  -> style:border_style
  -> unit
  -> t

val border : ?width:Length.t -> ?color:[< Color.t ] -> style:border_style -> unit -> t
val border_radius : Length.t -> t
val border_collapse : [ `Separate | `Collapse | css_global_values ] -> t
val border_spacing : Length.t -> t
val outline : ?width:Length.t -> ?color:[< Color.t ] -> style:border_style -> unit -> t

type text_decoration_line =
  [ `None
  | `Underline
  | `Overline
  | `Line_through
  | css_global_values
  ]

type text_decoration_style =
  [ `Solid
  | `Double
  | `Dotted
  | `Dashed
  | `Wavy
  | css_global_values
  ]

val text_decoration
  :  ?style:text_decoration_style
  -> ?color:[< Color.t ]
  -> line:text_decoration_line list
  -> unit
  -> t

type item_alignment =
  [ `Auto
  | `Flex_start
  | `Flex_end
  | `Center
  | `Baseline
  | `Stretch
  ]

type justify_content =
  [ `Flex_start
  | `Flex_end
  | `Center
  | `Space_between
  | `Space_around
  | `Space_evenly
  ]

val flex_container
  :  ?inline:bool
  -> ?direction:[ `Row | `Row_reverse | `Column | `Column_reverse ]
  -> ?wrap:[ `Nowrap | `Wrap | `Wrap_reverse ]
  -> ?align_items:item_alignment
  -> ?justify_content:justify_content
  -> unit
  -> t

val flex_item
  :  ?order:int
  -> ?basis:Auto_or_length.t
  -> ?shrink:float
  -> grow:float
  -> unit
  -> t

val align_self : item_alignment -> t
val resize : [ `None | `Both | `Horizontal | `Vertical | css_global_values ] -> t

(** Note: You must include the [name]s @keyframes in the stylesheet *)
val animation
  :  name:string
  -> duration:Time_ns.Span.t
  -> ?delay:Time_ns.Span.t
  -> ?direction:
       [ `Normal | `Reverse | `Alternate | `Alternate_reverse | css_global_values ]
  -> ?fill_mode:[ `None | `Forwards | `Backwards | `Both | css_global_values ]
  -> ?iter_count:int
  -> ?timing_function:string
  -> unit
  -> t

type user_select =
  [ `All
  | `Auto
  | `None
  | `Text
  ]

val user_select : user_select -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, compare, bin_io]
  end
end

module Expert : sig
  (** By default, the Css_gen constructors validate that all values are well-formed.  This
      can be useful for debugging, as it'll throw an exception when the programmer makes a
      mistake. However, it also incurs a heavy cost and should be avoided in tight loops.
      [should_validate] allows the programmer to disable css value validation when
      necessary. *)
  val should_validate : bool ref
end

module Private : sig
  val float_to_string_with_fixed : (int -> float -> string) ref
end
