module Stable = struct
  open Core.Core_stable

  module V1 = struct
    (** (field * value) list. Where value should be escaped / quoted as necessary as per
        https://www.w3.org/TR/CSS21/syndata.html#rule-sets. *)
    type t = (string * string) list
    [@@deriving sexp, compare, equal, bin_io, stable_witness, sexp_grammar]
  end
end

open Core
include Stable.V1

type 'a css_global_values =
  [ `Inherit
  | `Initial
  | `Var of string
  | `Var_with_default of string * 'a
  ]
[@@deriving sexp, bin_io, compare, equal, sexp_grammar]

let global_to_string_css (t : 'a css_global_values) ~(to_string_css : 'a -> string) =
  match t with
  | `Inherit -> "inherit"
  | `Initial -> "initial"
  | `Var var -> [%string "var(%{var})"]
  | `Var_with_default (var, default) -> [%string "var(%{var}, %{to_string_css default})"]
;;

module Private = struct
  let float_to_string_with_fixed = ref (fun digits f -> sprintf "%.*f" digits f)
end

let f2s digits f = !Private.float_to_string_with_fixed digits f

module Color = struct
  module T = struct
    module RGBA = struct
      type t =
        { r : int
        ; g : int
        ; b : int
        ; a : Percent.t option
        }
      [@@deriving sexp, bin_io, compare, equal, sexp_grammar, fields ~getters]

      let create ~r ~g ~b ?a () = { r; g; b; a }
    end

    module HSLA = struct
      type t =
        { h : int
        ; s : Percent.t
        ; l : Percent.t
        ; a : Percent.t option
        }
      [@@deriving sexp, bin_io, compare, equal, sexp_grammar, fields ~getters]

      let create ~h ~s ~l ?a () = { h; s; l; a }
    end

    module LCHA = struct
      type t =
        { l : Percent.t
        ; c : Percent.t
        ; h : float
        ; a : Percent.t option
        }
      [@@deriving sexp, bin_io, compare, equal, sexp_grammar, fields ~getters]

      let create ~l ~c ~h ?a () = { l; c; h; a }
    end

    module OKLCHA = struct
      type t =
        { l : Percent.t
        ; c : Percent.t
        ; h : float
        ; a : Percent.t option
        }
      [@@deriving sexp, bin_io, compare, equal, sexp_grammar, fields ~getters]

      let create ~l ~c ~h ?a () = { l; c; h; a }
    end

    type t =
      [ `RGBA of RGBA.t
      | `HSLA of HSLA.t
      | `LCHA of LCHA.t
      | `OKLCHA of OKLCHA.t
      | `Name of string
      | `Hex of string
      | t css_global_values
      ]
    [@@deriving sexp, bin_io, compare, equal, sexp_grammar]
  end

  include T
  include Sexpable.To_stringable (T)

  let clamp_percent = Percent.clamp_exn ~min:Percent.zero ~max:Percent.one_hundred_percent
  let alpha_to_string a = f2s 2 (Percent.to_mult a)
  let percent_to_string percent = f2s 0 (Percent.to_percentage percent)
  let angle_to_string angle = f2s 2 angle

  let rec to_string_css : t -> string = function
    | `RGBA { RGBA.r; g; b; a } ->
      (match a with
       | None -> [%string "rgb(%{r#Int},%{g#Int},%{b#Int})"]
       | Some a -> [%string "rgba(%{r#Int},%{g#Int},%{b#Int},%{alpha_to_string a})"])
    | `HSLA { HSLA.h; s; l; a } ->
      let s = percent_to_string s in
      let l = percent_to_string l in
      (match a with
       | None -> [%string "hsl(%{h#Int},%{s}%,%{l}%)"]
       | Some a -> [%string "hsla(%{h#Int},%{s}%,%{l}%,%{alpha_to_string a})"])
    | `LCHA { LCHA.l; c; h; a } ->
      let l = percent_to_string l in
      let c = percent_to_string c in
      let h = angle_to_string h in
      (match a with
       | None -> [%string "lch(%{l}% %{c}% %{h})"]
       | Some a -> [%string "lch(%{l}% %{c}% %{h} / %{alpha_to_string a})"])
    | `OKLCHA { OKLCHA.l; c; h; a } ->
      let l = percent_to_string l in
      let c = percent_to_string c in
      let h = angle_to_string h in
      (match a with
       | None -> [%string "oklch(%{l}% %{c}% %{h})"]
       | Some a -> [%string "oklch(%{l}% %{c}% %{h} / %{alpha_to_string a})"])
    | `Name name -> name
    | `Hex hex -> hex
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  ;;

  let to_string_css t = to_string_css (t :> t)

  module Hue_interpolation_method = struct
    type t =
      | Shorter
      | Longer
      | Increasing
      | Decreasing

    let to_string = function
      | Shorter -> "shorter"
      | Longer -> "longer"
      | Increasing -> "increasing"
      | Decreasing -> "decreasing"
    ;;
  end

  module Xyz_space = struct
    type t =
      | Xyz
      | Xyz_d50
      | Xyz_d65

    let to_string = function
      | Xyz -> "xyz"
      | Xyz_d50 -> "xyz-d50"
      | Xyz_d65 -> "xyz-d65"
    ;;
  end

  module Rectangular_color_space = struct
    type t =
      | Srgb
      | Srgb_linear
      | Display_p3
      | A98_rgb
      | Prophoto_rgb
      | Rec2020
      | Lab
      | Oklab

    let to_string = function
      | Srgb -> "srgb"
      | Srgb_linear -> "srgb-linear"
      | Display_p3 -> "display-p3"
      | A98_rgb -> "a98-rgb"
      | Prophoto_rgb -> "prophoto-rgb"
      | Rec2020 -> "rec2020"
      | Lab -> "lab"
      | Oklab -> "oklab"
    ;;
  end

  module Polar_color_space = struct
    type t =
      | Hsl
      | Hwb
      | Lch
      | Oklch

    let to_string = function
      | Hsl -> "hsl"
      | Hwb -> "hwb"
      | Lch -> "lch"
      | Oklch -> "oklch"
    ;;
  end

  module Color_interpolation_method = struct
    type t =
      | Rectangular_color_space of Rectangular_color_space.t
      | Xyz_space of Xyz_space.t
      | Polar_color_space of
          { color_space : Polar_color_space.t
          ; hue_interpolation_method : Hue_interpolation_method.t option
          }

    let to_string = function
      | Rectangular_color_space color_space ->
        [%string "in %{Rectangular_color_space.to_string color_space}"]
      | Xyz_space xyz_space -> [%string "in %{Xyz_space.to_string xyz_space}"]
      | Polar_color_space { color_space; hue_interpolation_method } ->
        let interpolation_method =
          match hue_interpolation_method with
          | None -> ""
          | Some hue_interpolation_method ->
            [%string " %{hue_interpolation_method#Hue_interpolation_method} hue"]
        in
        [%string "in %{color_space#Polar_color_space}%{interpolation_method}"]
    ;;
  end

  let mix
    ?(color_interpolation_method =
      Color_interpolation_method.Polar_color_space
        { color_space = Oklch; hue_interpolation_method = None })
    ~from
    ~to_
    percent
    =
    let percent_str = f2s 2 (clamp_percent percent |> Percent.to_percentage) in
    `Name
      [%string
        "color-mix(%{color_interpolation_method#Color_interpolation_method}, \
         %{to_string_css from}, %{to_string_css to_} %{percent_str}%)"]
  ;;
end

module Length = struct
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
    | t css_global_values
    ]
  [@@deriving sexp, bin_io, compare]

  let rec to_string_css = function
    | `Raw s -> s
    | `Ch c -> [%string "%{f2s 2 c}ch"]
    | `Rem f -> [%string "%{f2s 2 f}rem"]
    | `Em i -> [%string "%{i#Int}em"]
    | `Em_float f -> [%string "%{f2s 2 f}em"]
    | `Percent p -> [%string "%{f2s 2 (Percent.to_percentage p)}%"]
    | `Pt p -> [%string "%{f2s 2 p}pt"]
    | `Px i -> [%string "%{i#Int}px"]
    | `Px_float f -> [%string "%{f2s 2 f}px"]
    | `Vh p -> [%string "%{f2s 2 (Percent.to_percentage p)}vh"]
    | `Vw p -> [%string "%{f2s 2 (Percent.to_percentage p)}vw"]
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  ;;

  let to_string_css t = to_string_css (t :> t)
  let percent100 = `Percent (Percent.of_percentage 100.)
  let raw s = `Raw s
  let ch f = `Ch f
  let rem f = `Rem f
  let em i = `Em i
  let em_float f = `Em_float f
  let percent p = `Percent p
  let pt f = `Pt f
  let px i = `Px i
  let px_float f = `Px_float f
  let vh p = `Vh p
  let vw p = `Vw p
end

module Auto_or_length = struct
  type t =
    [ `Auto
    | Length.t
    ]
  [@@deriving bin_io, compare, sexp]

  let to_string_css = function
    | `Auto -> "auto"
    | #Length.t as l -> Length.to_string_css l
  ;;
end

let value_map o ~f = Option.value_map o ~default:"" ~f
let combine t1 t2 = t1 @ t2
let ( @> ) = combine
let concat l = List.concat l
let to_string_list = Fn.id

let to_string_css t =
  List.map t ~f:(fun (field, value) -> [%string "%{field}: %{value}"])
  |> String.concat ~sep:";"
;;

let of_string_css_exn s = Css_parser.parse_declaration_list s |> Or_error.ok_exn

(** create_raw creates a single field, value pair. It assumes that the value is a valid
    css value. As such it is unsafe to use with arbitrary value strings. But for the vast
    majority of combinators in this module it is the right thing to use, as we know by
    construction that the values do not need quoting / escaping. *)
let create_raw ~field ~value = [ field, value ]

module Expert = struct
  let should_validate = ref false
end

let create ~field ~value =
  if !Expert.should_validate then Css_parser.validate_value value |> Or_error.ok_exn;
  create_raw ~field ~value
;;

let empty = []
let is_empty = List.is_empty
let create_placement name length = create ~field:name ~value:(Length.to_string_css length)
let left = create_placement "left"
let top = create_placement "top"
let bottom = create_placement "bottom"
let right = create_placement "right"

let position ?top:tp ?bottom:bt ?left:lt ?right:rt pos =
  let pos =
    let value =
      match pos with
      | `Static -> "static"
      | `Absolute -> "absolute"
      | `Sticky -> "sticky"
      | `Relative -> "relative"
      | `Fixed -> "fixed"
    in
    create ~field:"position" ~value
  in
  let convert opt_l f = Option.value_map opt_l ~default:empty ~f in
  concat [ pos; convert tp top; convert lt left; convert rt right; convert bt bottom ]
;;

type box_sizing =
  [ `Content_box
  | `Border_box
  | box_sizing css_global_values
  ]

let box_sizing v =
  let rec to_string_css = function
    | `Content_box -> "content-box"
    | `Border_box -> "border-box"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"box-sizing" ~value:(to_string_css v)
;;

type display =
  [ `Inline
  | `Block
  | `Inline_block
  | `List_item
  | `Table
  | `Inline_table
  | `None
  | `Inline_grid
  | display css_global_values
  ]

let display v =
  let rec to_string_css = function
    | `Inline -> "inline"
    | `Block -> "block"
    | `Inline_block -> "inline-block"
    | `List_item -> "list-item"
    | `Table -> "table"
    | `Inline_table -> "inline-table"
    | `None -> "none"
    | `Flex -> "flex"
    | `Inline_flex -> "inline-flex"
    | `Inline_grid -> "inline-grid"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"display" ~value:(to_string_css v)
;;

type visibility =
  [ `Visible
  | `Hidden
  | `Collapse
  | visibility css_global_values
  ]

let visibility v =
  let rec to_string_css = function
    | `Visible -> "visible"
    | `Hidden -> "hidden"
    | `Collapse -> "collapse"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"visibility" ~value:(to_string_css v)
;;

type overflow =
  [ `Visible
  | `Hidden
  | `Scroll
  | `Auto
  | overflow css_global_values
  ]

let make_overflow field v =
  let rec to_string_css = function
    | `Visible -> "visible"
    | `Hidden -> "hidden"
    | `Scroll -> "scroll"
    | `Auto -> "auto"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field ~value:(to_string_css v)
;;

let overflow = make_overflow "overflow"
let overflow_x = make_overflow "overflow-x"
let overflow_y = make_overflow "overflow-y"
let z_index i = create_raw ~field:"z-index" ~value:(Int.to_string i)
let opacity i = create_raw ~field:"opacity" ~value:(f2s 6 i)

type text_overflow =
  [ `Clip
  | `Ellipsis
  | text_overflow css_global_values
  ]

let text_overflow v =
  let rec to_string_css = function
    | `Clip -> "clip"
    | `Ellipsis -> "ellipsis"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"text-overflow" ~value:(to_string_css v)
;;

let create_length_field field l =
  create_raw ~field ~value:(Auto_or_length.to_string_css l)
;;

let white_space v =
  let rec to_string_css = function
    | `Normal -> "normal"
    | `Nowrap -> "nowrap"
    | `Pre -> "pre"
    | `Pre_line -> "pre-line"
    | `Pre_wrap -> "pre-wrap"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create ~field:"white-space" ~value:(to_string_css v)
;;

type font_style =
  [ `Normal
  | `Italic
  | `Oblique
  | font_style css_global_values
  ]

type font_weight =
  [ `Normal
  | `Bold
  | `Bolder
  | `Lighter
  | `Number of int
  | font_weight css_global_values
  ]

type font_variant =
  [ `Normal
  | `Small_caps
  | font_variant css_global_values
  ]

let font_size = create_length_field "font-size"
let font_family l = create_raw ~field:"font-family" ~value:(String.concat l ~sep:",")

let font_style s =
  let rec to_string_css = function
    | `Normal -> "normal"
    | `Italic -> "italic"
    | `Oblique -> "oblique"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"font-style" ~value:(to_string_css s)
;;

let font_weight s =
  let rec to_string_css = function
    | `Number i -> Int.to_string i
    | `Bold -> "bold"
    | `Normal -> "normal"
    | `Lighter -> "lighter"
    | `Bolder -> "bolder"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"font-weight" ~value:(to_string_css s)
;;

let bold = font_weight `Bold

let font_variant s =
  let rec to_string_css = function
    | `Normal -> "normal"
    | `Small_caps -> "small-caps"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"font-variant" ~value:(to_string_css s)
;;

let font ~size ~family ?style ?weight ?variant () =
  [ Some (font_size size)
  ; Some (font_family family)
  ; Option.map style ~f:font_style
  ; Option.map weight ~f:font_weight
  ; Option.map variant ~f:font_variant
  ]
  |> List.filter_opt
  |> concat
;;

let create_with_color ~field ~color = create_raw ~field ~value:(Color.to_string_css color)
let color color = create_with_color ~field:"color" ~color
let background_color color = create_with_color ~field:"background-color" ~color
let fill color = create_with_color ~field:"fill" ~color

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

type text_align =
  [ `Left
  | `Right
  | `Center
  | `Justify
  | text_align css_global_values
  ]

type horizontal_align =
  [ `Left
  | `Right
  | `Center
  | horizontal_align css_global_values
  ]

type vertical_align =
  [ `Top
  | `Bottom
  | `Middle
  | `Super
  | `Sub
  | vertical_align css_global_values
  ]

type white_space =
  [ `Normal
  | `Nowrap
  | `Pre
  | `Pre_line
  | `Pre_wrap
  | white_space css_global_values
  ]

let stops_to_string stops =
  List.map stops ~f:(fun (pct, color) ->
    (* Note: Percent.to_string produced e.g. "0x", "1x", won't work here. *)
    [%string "%{Color.to_string_css color} %{f2s 6 (Percent.to_percentage pct)}%"])
  |> String.concat ~sep:", "
;;

let background_image spec =
  let value =
    match spec with
    | `Url url -> [%string "url(%{url})"]
    | `Linear_gradient { direction = `Deg direction; stops } ->
      [%string "linear-gradient(%{direction#Int}deg, %{stops_to_string stops})"]
    | `Radial_gradient { stops } -> [%string "radial-gradient(%{stops_to_string stops})"]
  in
  create_raw ~field:"background-image" ~value
;;

let text_align value =
  let rec to_string_css = function
    | `Justify -> "justify"
    | `Right -> "right"
    | `Left -> "left"
    | `Center -> "center"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"text-align" ~value:(to_string_css value)
;;

let horizontal_align value =
  let rec to_string_css = function
    | `Right -> "right"
    | `Left -> "left"
    | `Center -> "center"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"horizontal-align" ~value:(to_string_css value)
;;

let vertical_align value =
  let rec to_string_css = function
    | `Top -> "top"
    | `Middle -> "middle"
    | `Bottom -> "bottom"
    | `Super -> "super"
    | `Sub -> "sub"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"vertical-align" ~value:(to_string_css value)
;;

let float f =
  let rec to_string_css = function
    | `None -> "none"
    | `Left -> "left"
    | `Right -> "right"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"float" ~value:(to_string_css f)
;;

let line_height = create_length_field "line-height"
let width = create_length_field "width"
let min_width = create_length_field "min-width"
let max_width = create_length_field "max-width"
let height = create_length_field "height"
let min_height = create_length_field "min-height"
let max_height = create_length_field "max-height"
let padding_top = create_length_field "padding-top"
let padding_bottom = create_length_field "padding-bottom"
let padding_left = create_length_field "padding-left"
let padding_right = create_length_field "padding-right"

let padding ?top ?bottom ?left ?right () =
  let m = Option.map in
  [ m top ~f:padding_top
  ; m bottom ~f:padding_bottom
  ; m left ~f:padding_left
  ; m right ~f:padding_right
  ]
  |> List.filter_opt
  |> concat
;;

let uniform_padding l = padding ~top:l ~bottom:l ~left:l ~right:l ()
let margin_top = create_length_field "margin-top"
let margin_bottom = create_length_field "margin-bottom"
let margin_left = create_length_field "margin-left"
let margin_right = create_length_field "margin-right"

let margin ?top ?bottom ?left ?right () =
  let m = Option.map in
  [ m top ~f:margin_top
  ; m bottom ~f:margin_bottom
  ; m left ~f:margin_left
  ; m right ~f:margin_right
  ]
  |> List.filter_opt
  |> concat
;;

let uniform_margin l = margin ~top:l ~bottom:l ~left:l ~right:l ()
let row_gap = create_length_field "row-gap"
let column_gap = create_length_field "column-gap"

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
  | border_style css_global_values
  ]

(** Concat 2 values with a space in between. If either is the empty string don't put in
    unnecessary whitespace. *)
let concat2v v1 v2 =
  match v1, v2 with
  | "", x -> x
  | x, "" -> x
  | x, y -> x ^ " " ^ y
;;

(** Concat up to 3 values with spaces in between. *)
let concat3v v1 v2 v3 = concat2v (concat2v v1 v2) v3

let border_value ?width ?color ~(style : border_style) () =
  let rec style_to_string_css = function
    | `Ridge -> "ridge"
    | `Outset -> "outset"
    | `None -> "none"
    | `Groove -> "groove"
    | `Dashed -> "dashed"
    | `Inset -> "inset"
    | `Hidden -> "hidden"
    | `Double -> "double"
    | `Dotted -> "dotted"
    | `Solid -> "solid"
    | #css_global_values as global ->
      global_to_string_css global ~to_string_css:style_to_string_css
  in
  let width = value_map width ~f:Length.to_string_css in
  let color = value_map color ~f:Color.to_string_css in
  concat3v width (style_to_string_css style) color
;;

let create_border ?side () =
  let field =
    match side with
    | Some `Top -> "border-top"
    | Some `Bottom -> "border-bottom"
    | Some `Right -> "border-right"
    | Some `Left -> "border-left"
    | None -> "border"
  in
  fun ?width ?color ~style () ->
    create_raw ~field ~value:(border_value ?width ?color ~style ())
;;

let border_top ?width ?color ~style () =
  create_border ~side:`Top () ?width ?color ~style ()
;;

let border_bottom ?width ?color ~style () =
  create_border ~side:`Bottom () ?width ?color ~style ()
;;

let border_left ?width ?color ~style () =
  create_border ~side:`Left () ?width ?color ~style ()
;;

let border_right ?width ?color ~style () =
  create_border ~side:`Right () ?width ?color ~style ()
;;

let border ?width ?color ~style () = create_border ?side:None ?width ?color ~style () ()

let outline ?width ?color ~style () =
  create_raw ~field:"outline" ~value:(border_value ?width ?color ~style ())
;;

type border_collapse =
  [ `Separate
  | `Collapse
  | border_collapse css_global_values
  ]

let border_collapse v =
  let rec to_string_css = function
    | `Separate -> "separate"
    | `Collapse -> "collapse"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"border-collapse" ~value:(to_string_css v)
;;

let border_spacing = create_length_field "border-spacing"
let border_radius l = create ~field:"border-radius" ~value:(Length.to_string_css l)

type text_decoration_line =
  [ `None
  | `Underline
  | `Overline
  | `Line_through
  | text_decoration_line css_global_values
  ]
[@@deriving sexp]

type text_decoration_style =
  [ `Solid
  | `Double
  | `Dotted
  | `Dashed
  | `Wavy
  | text_decoration_style css_global_values
  ]
[@@deriving sexp]

let text_decoration ?style ?color ~line () =
  let value =
    let line =
      let rec to_string_css = function
        | `Line_through -> "line-through"
        | `None -> "none"
        | `Overline -> "overline"
        | `Underline -> "underline"
        | #css_global_values as global -> global_to_string_css global ~to_string_css
      in
      List.map line ~f:to_string_css |> String.concat ~sep:" "
    in
    let style =
      let rec to_string_css = function
        | `Solid -> "solid"
        | `Double -> "double"
        | `Dotted -> "dotted"
        | `Dashed -> "dashed"
        | `Wavy -> "wavy"
        | #css_global_values as global -> global_to_string_css global ~to_string_css
      in
      Option.value_map style ~f:to_string_css ~default:""
    in
    let color = value_map color ~f:Color.to_string_css in
    concat3v line style color
  in
  create_raw ~field:"text-decoration" ~value
;;

type content_alignment =
  [ `Normal
  | `Flex_start
  | `Flex_end
  | `Center
  | `Space_between
  | `Space_around
  | `Space_evenly
  | `Stretch
  ]

let content_alignment_to_string_css = function
  | `Normal -> "normal"
  | `Flex_start -> "flex-start"
  | `Flex_end -> "flex-end"
  | `Center -> "center"
  | `Space_between -> "space-between"
  | `Space_around -> "space-around"
  | `Space_evenly -> "space-evenly"
  | `Stretch -> "stretch"
;;

type item_alignment =
  [ `Auto
  | `Flex_start
  | `Flex_end
  | `Center
  | `Baseline
  | `Stretch
  ]

let item_alignment_to_string_css = function
  | `Auto -> "auto"
  | `Flex_start -> "flex-start"
  | `Flex_end -> "flex-end"
  | `Center -> "center"
  | `Baseline -> "baseline"
  | `Stretch -> "stretch"
;;

type justify_content =
  [ `Flex_start
  | `Flex_end
  | `Center
  | `Space_between
  | `Space_around
  | `Space_evenly
  ]

let justify_content_to_string_css = function
  | `Flex_start -> "flex-start"
  | `Flex_end -> "flex-end"
  | `Center -> "center"
  | `Space_between -> "space-between"
  | `Space_around -> "space-around"
  | `Space_evenly -> "space-evenly"
;;

let flex_container
  ?(inline = false)
  ?(direction = `Row)
  ?(wrap = `Nowrap)
  ?align_items
  ?align_content
  ?justify_content
  ?row_gap:rg
  ?column_gap:cg
  ()
  =
  let direction =
    let make_dir v = create_raw ~field:"flex-direction" ~value:v in
    match direction with
    | `Row -> make_dir "row"
    | `Row_reverse -> make_dir "row-reverse"
    | `Column -> make_dir "column"
    | `Column_reverse -> make_dir "column-reverse"
    | `Default -> empty
  in
  let wrap =
    let make_wrap v = create_raw ~field:"flex-wrap" ~value:v in
    match wrap with
    | `Nowrap -> make_wrap "nowrap"
    | `Wrap -> make_wrap "wrap"
    | `Wrap_reverse -> make_wrap "wrap-reverse"
    | `Default -> empty
  in
  let align_items =
    match align_items with
    | None -> empty
    | Some a -> create_raw ~field:"align-items" ~value:(item_alignment_to_string_css a)
  in
  let align_content =
    match align_content with
    | None -> empty
    | Some a ->
      create_raw ~field:"align-content" ~value:(content_alignment_to_string_css a)
  in
  let justify_content =
    match justify_content with
    | None -> empty
    | Some a ->
      create_raw ~field:"justify-content" ~value:(justify_content_to_string_css a)
  in
  let row_gap = Option.value_map ~default:empty ~f:row_gap rg in
  let column_gap = Option.value_map ~default:empty ~f:column_gap cg in
  concat
    [ display (if inline then `Inline_flex else `Flex)
    ; direction
    ; wrap
    ; align_items
    ; align_content
    ; justify_content
    ; column_gap
    ; row_gap
    ]
;;

let flex_item ?order ?(basis = `Auto) ?(shrink = 1.) ~grow () =
  let order =
    Option.map order ~f:(fun i -> create_raw ~field:"order" ~value:(Int.to_string i))
    |> Option.to_list
    |> List.join
  in
  let flex =
    let basis = Auto_or_length.to_string_css basis in
    create_raw ~field:"flex" ~value:[%string "%{f2s 6 grow} %{f2s 6 shrink} %{basis}"]
  in
  concat [ flex; order ]
;;

let align_self a =
  let value = item_alignment_to_string_css a in
  create_raw ~field:"align-self" ~value
;;

type resize =
  [ `None
  | `Both
  | `Horizontal
  | `Vertical
  | resize css_global_values
  ]

let resize value =
  let rec to_string_css = function
    | `None -> "none"
    | `Both -> "both"
    | `Horizontal -> "horizontal"
    | `Vertical -> "vertical"
    | #css_global_values as global -> global_to_string_css global ~to_string_css
  in
  create_raw ~field:"resize" ~value:(to_string_css value)
;;

type direction =
  [ `Normal
  | `Reverse
  | `Alternate
  | `Alternate_reverse
  | direction css_global_values
  ]

type fill_mode =
  [ `None
  | `Forwards
  | `Backwards
  | `Both
  | fill_mode css_global_values
  ]

let animation ~name ~duration ?delay ?direction ?fill_mode ?iter_count ?timing_function ()
  =
  let m = Option.map in
  let span_to_string s = [%string "%{f2s 2 (Time_ns.Span.to_sec s)}s"] in
  let direction =
    m direction ~f:(fun d ->
      let rec to_string_css = function
        | `Normal -> "normal"
        | `Reverse -> "reverse"
        | `Alternate -> "alternate"
        | `Alternate_reverse -> "alternate-reverse"
        | #css_global_values as global -> global_to_string_css global ~to_string_css
      in
      create_raw ~field:"animation-direction" ~value:(to_string_css d))
  in
  let fill_mode =
    m fill_mode ~f:(fun f ->
      let rec to_string_css = function
        | `None -> "none"
        | `Forwards -> "forwards"
        | `Backwards -> "backwards"
        | `Both -> "both"
        | #css_global_values as global -> global_to_string_css global ~to_string_css
      in
      create_raw ~field:"animation-fill-mode" ~value:(to_string_css f))
  in
  [ Some (create_raw ~field:"animation-name" ~value:name)
  ; Some (create_raw ~field:"animation-duration" ~value:(span_to_string duration))
  ; m delay ~f:(fun s -> create_raw ~field:"animation-delay" ~value:(span_to_string s))
  ; m iter_count ~f:(fun i ->
      create_raw ~field:"animation-iteration-count" ~value:(Int.to_string i))
  ; m timing_function ~f:(fun value ->
      create_raw ~field:"animation-timing-function" ~value)
  ; direction
  ; fill_mode
  ]
  |> List.filter_opt
  |> concat
;;

type user_select =
  [ `All
  | `Auto
  | `None
  | `Text
  ]

let user_select_to_string_css = function
  | `All -> "all"
  | `Auto -> "auto"
  | `None -> "none"
  | `Text -> "text"
;;

let user_select s =
  let value = user_select_to_string_css s in
  create_raw ~field:"user-select" ~value
;;

module%test [@name "tests"] _ = struct
  let%expect_test "to_string_css -> of_string_css_exn -> to_string_css" =
    let t css =
      let s = to_string_css css in
      let s2 = to_string_css (of_string_css_exn s) in
      print_endline s;
      print_endline s2
    in
    t (flex_item ~grow:1.0 () @> overflow `Scroll);
    t (flex_container ~inline:true ~direction:`Column () @> border ~style:`Dashed ());
    t (color (`RGBA (Color.RGBA.create ~r:100 ~g:100 ~b:100 ())));
    t
      (color
         (`HSLA
           (Color.HSLA.create
              ~h:100
              ~s:(Percent.of_mult 0.75)
              ~l:(Percent.of_mult 0.60)
              ())));
    t (create ~field:"content" ~value:{|";"|});
    [%expect
      {|
      flex: 1.000000 1.000000 auto;overflow: scroll
      flex: 1.000000 1.000000 auto;overflow: scroll
      display: inline-flex;flex-direction: column;flex-wrap: nowrap;border: dashed
      display: inline-flex;flex-direction: column;flex-wrap: nowrap;border: dashed
      color: rgb(100,100,100)
      color: rgb(100,100,100)
      color: hsl(100,75%,60%)
      color: hsl(100,75%,60%)
      content: ";"
      content: ";"
      |}]
  ;;

  let%expect_test "gradients" =
    let p x = Percent.of_mult x in
    let c s = `Name s in
    let t css = print_endline (to_string_css css) in
    t
      (background_image
         (`Linear_gradient
           { direction = `Deg 90
           ; stops =
               [ p 0., c "black"
               ; p 0.2, c "#ff0000"
               ; p 0.4, c "red"
               ; ( p 1.
                 , `RGBA
                     (Color.RGBA.create ~r:100 ~g:50 ~b:30 ~a:(Percent.of_mult 0.75) ()) )
               ]
           }));
    [%expect
      {| background-image: linear-gradient(90deg, black 0.000000%, #ff0000 20.000000%, red 40.000000%, rgba(100,50,30,0.75) 100.000000%) |}];
    t (background_image (`Radial_gradient { stops = [ p 0., c "black"; p 1., c "red" ] }));
    [%expect {| background-image: radial-gradient(black 0.000000%, red 100.000000%) |}]
  ;;
end
