module Stable = struct
  open Core.Core_stable

  module V1 = struct
    (** (field * value) list.  Where value should be escaped / quoted
        as necessary as per https://www.w3.org/TR/CSS21/syndata.html#rule-sets. *)
    type t = (string * string) list [@@deriving sexp, compare, bin_io]
  end
end

open Core
include Stable.V1

type css_global_values =
  [ `Inherit
  | `Initial
  ]
[@@deriving sexp, bin_io, compare]

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
      [@@deriving sexp, bin_io, compare, fields]

      let create ~r ~g ~b ?a () = { r; g; b; a }
    end

    module HSLA = struct
      type t =
        { h : int
        ; s : Percent.t
        ; l : Percent.t
        ; a : Percent.t option
        }
      [@@deriving sexp, bin_io, compare, fields]

      let create ~h ~s ~l ?a () = { h; s; l; a }
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
  end

  include T
  include Sexpable.To_stringable (T)

  let to_string_css : [< t ] -> string = function
    | `Inherit -> "inherit"
    | `Initial -> "initial"
    | `RGBA { RGBA.r; g; b; a } ->
      (match a with
       | None -> [%string "rgb(%{r#Int},%{g#Int},%{b#Int})"]
       | Some p ->
         [%string "rgba(%{r#Int},%{g#Int},%{b#Int},%{f2s 2 (Percent.to_mult p)})"])
    | `HSLA { HSLA.h; s; l; a } ->
      (match a with
       | None ->
         [%string
           "hsl(%{h#Int},%{f2s 0 (Percent.to_percentage s)}%,%{f2s 0 \
            (Percent.to_percentage l)}%)"]
       | Some p ->
         [%string
           "hsla(%{h#Int},%{f2s 0 (Percent.to_percentage s)}%,%{f2s 0 \
            (Percent.to_percentage l)}%,%{f2s 2 (Percent.to_mult p)})"])
    | `Name name -> name
    | `Hex hex -> hex
    | `Var var -> [%string "var(%{var})"]
  ;;
end

module Alignment = struct
  type t =
    [ `Left
    | `Right
    | `Center (* horizontal *)
    | `Top
    | `Bottom
    | `Middle (* vertical *)
    | `Justify (* text-align (in addition to [horizontal]) *)
    | `Super (* vertical *)
    | `Sub (* vertical *)
    | css_global_values
    ]
  [@@deriving bin_io, compare]

  let to_string_css = function
    | `Justify -> "justify"
    | `Top -> "top"
    | `Right -> "right"
    | `Left -> "left"
    | `Center -> "center"
    | `Inherit -> "inherit"
    | `Middle -> "middle"
    | `Bottom -> "bottom"
    | `Super -> "super"
    | `Sub -> "sub"
    | `Initial -> "initial"
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
    | css_global_values
    ]
  [@@deriving sexp, bin_io, compare]

  let to_string_css = function
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
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  ;;

  let percent100 = `Percent (Percent.of_percentage 100.)
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

(** create_raw creates a single field, value pair.  It assumes that the value is a valid
    css value.  As such it is unsafe to use with arbitrary value strings.  But for the
    vast majority of combinators in this module it is the right thing to use, as we know
    by construction that the values do not need quoting / escaping. *)
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

let box_sizing v =
  let value =
    match v with
    | `Content_box -> "content-box"
    | `Border_box -> "border-box"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"box-sizing" ~value
;;

let display v =
  let value =
    match v with
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
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"display" ~value
;;

let visibility v =
  let value =
    match v with
    | `Visible -> "visible"
    | `Hidden -> "hidden"
    | `Collapse -> "collapse"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"visibility" ~value
;;

type overflow =
  [ `Visible
  | `Hidden
  | `Scroll
  | `Auto
  | css_global_values
  ]

let make_overflow field v =
  let value =
    match v with
    | `Visible -> "visible"
    | `Hidden -> "hidden"
    | `Scroll -> "scroll"
    | `Auto -> "auto"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field ~value
;;

let overflow = make_overflow "overflow"
let overflow_x = make_overflow "overflow-x"
let overflow_y = make_overflow "overflow-y"
let z_index i = create_raw ~field:"z-index" ~value:(Int.to_string i)
let opacity i = create_raw ~field:"opacity" ~value:(f2s 6 i)

let create_length_field field l =
  create_raw ~field ~value:(Auto_or_length.to_string_css l)
;;

let white_space v =
  let value =
    match v with
    | `Normal -> "normal"
    | `Nowrap -> "nowrap"
    | `Pre -> "pre"
    | `Pre_line -> "pre-line"
    | `Pre_wrap -> "pre-wrap"
    | `Initial -> "initial"
    | `Inherit -> "inherit"
  in
  create ~field:"white-space" ~value
;;

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

let font_size = create_length_field "font-size"
let font_family l = create_raw ~field:"font-family" ~value:(String.concat l ~sep:",")

let font_style s =
  let value =
    match s with
    | `Normal -> "normal"
    | `Italic -> "italic"
    | `Oblique -> "oblique"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"font-style" ~value
;;

let font_weight s =
  let value =
    match s with
    | `Number i -> Int.to_string i
    | `Bold -> "bold"
    | `Normal -> "normal"
    | `Lighter -> "lighter"
    | `Inherit -> "inherit"
    | `Bolder -> "bolder"
    | `Initial -> "initial"
  in
  create_raw ~field:"font-weight" ~value
;;

let bold = font_weight `Bold

let font_variant s =
  let value =
    match s with
    | `Normal -> "normal"
    | `Small_caps -> "small-caps"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"font-variant" ~value
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

let create_alignment field a =
  create_raw ~field ~value:(Alignment.to_string_css (a :> Alignment.t))
;;

let text_align = create_alignment "text-align"
let horizontal_align = create_alignment "horizontal-align"
let vertical_align = create_alignment "vertical-align"

let float f =
  let value =
    match f with
    | `None -> "none"
    | `Left -> "left"
    | `Right -> "right"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"float" ~value
;;

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

(** Concat 2 values with a space in between.  If either is the empty string
    don't put in unnecessary whitespace. *)
let concat2v v1 v2 =
  match v1, v2 with
  | "", x -> x
  | x, "" -> x
  | x, y -> x ^ " " ^ y
;;

(** Concat up to 3 values with spaces in between. *)
let concat3v v1 v2 v3 = concat2v (concat2v v1 v2) v3

let border_value ?width ?color ~(style : border_style) () =
  let style =
    match style with
    | `Ridge -> "ridge"
    | `Outset -> "outset"
    | `None -> "none"
    | `Groove -> "groove"
    | `Dashed -> "dashed"
    | `Inherit -> "inherit"
    | `Inset -> "inset"
    | `Hidden -> "hidden"
    | `Double -> "double"
    | `Dotted -> "dotted"
    | `Initial -> "initial"
    | `Solid -> "solid"
  in
  let width = value_map width ~f:Length.to_string_css in
  let color = value_map color ~f:Color.to_string_css in
  concat3v width style color
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

let border_collapse v =
  let value =
    match v with
    | `Separate -> "separate"
    | `Collapse -> "collapse"
    | `Inherit -> "inherit"
    | `Initial -> "initial"
  in
  create_raw ~field:"border-collapse" ~value
;;

let border_spacing = create_length_field "border-spacing"
let border_radius l = create ~field:"border-radius" ~value:(Length.to_string_css l)

type text_decoration_line =
  [ `None
  | `Underline
  | `Overline
  | `Line_through
  | css_global_values
  ]
[@@deriving sexp]

type text_decoration_style =
  [ `Solid
  | `Double
  | `Dotted
  | `Dashed
  | `Wavy
  | css_global_values
  ]
[@@deriving sexp]

let text_decoration ?style ?color ~line () =
  let value =
    let line =
      List.map line ~f:(function
        | `Line_through -> "line-through"
        | `None -> "none"
        | `Inherit -> "inherit"
        | `Overline -> "overline"
        | `Underline -> "underline"
        | `Initial -> "initial")
      |> String.concat ~sep:" "
    in
    let style =
      match style with
      | None -> ""
      | Some `Solid -> "solid"
      | Some `Double -> "double"
      | Some `Dotted -> "dotted"
      | Some `Dashed -> "dashed"
      | Some `Wavy -> "wavy"
      | Some `Inherit -> "inherit"
      | Some `Initial -> "initial"
    in
    let color = value_map color ~f:Color.to_string_css in
    concat3v line style color
  in
  create_raw ~field:"text-decoration" ~value
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
      ?justify_content
      ()
  =
  let direction =
    match direction with
    | `Row -> "row"
    | `Row_reverse -> "row-reverse"
    | `Column -> "column"
    | `Column_reverse -> "column-reverse"
  in
  let wrap =
    match wrap with
    | `Nowrap -> "nowrap"
    | `Wrap -> "wrap"
    | `Wrap_reverse -> "wrap-reverse"
  in
  let align_items =
    match align_items with
    | None -> empty
    | Some a -> create_raw ~field:"align-items" ~value:(item_alignment_to_string_css a)
  in
  let justify_content =
    match justify_content with
    | None -> empty
    | Some a ->
      create_raw ~field:"justify-content" ~value:(justify_content_to_string_css a)
  in
  concat
    [ display (if inline then `Inline_flex else `Flex)
    ; create_raw ~field:"flex-direction" ~value:direction
    ; create_raw ~field:"flex-wrap" ~value:wrap
    ; align_items
    ; justify_content
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

let resize (value : [ `None | `Both | `Horizontal | `Vertical | css_global_values ]) =
  let value =
    match value with
    | `None -> "none"
    | `Both -> "both"
    | `Horizontal -> "horizontal"
    | `Vertical -> "vertical"
    | `Initial -> "initial"
    | `Inherit -> "inherit"
  in
  create_raw ~field:"resize" ~value
;;

let animation ~name ~duration ?delay ?direction ?fill_mode ?iter_count ?timing_function ()
  =
  let m = Option.map in
  let span_to_string s = [%string "%{f2s 2 (Time_ns.Span.to_sec s)}s"] in
  let direction =
    m direction ~f:(fun d ->
      let value =
        match d with
        | `Normal -> "normal"
        | `Reverse -> "reverse"
        | `Alternate -> "alternate"
        | `Alternate_reverse -> "alternate-reverse"
        | `Inherit -> "inherit"
        | `Initial -> "initial"
      in
      create_raw ~field:"animation-direction" ~value)
  in
  let fill_mode =
    m fill_mode ~f:(fun f ->
      let value =
        match f with
        | `None -> "none"
        | `Forwards -> "forwards"
        | `Backwards -> "backwards"
        | `Both -> "both"
        | `Inherit -> "inherit"
        | `Initial -> "initial"
      in
      create_raw ~field:"animation-fill-mode" ~value)
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


let%test_module "tests" =
  (module struct
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
    content: ";" |}]
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
                        (Color.RGBA.create ~r:100 ~g:50 ~b:30 ~a:(Percent.of_mult 0.75) ())
                    )
                  ]
              }));
      [%expect
        {| background-image: linear-gradient(90deg, black 0.000000%, #ff0000 20.000000%, red 40.000000%, rgba(100,50,30,0.75) 100.000000%) |}];
      t
        (background_image
           (`Radial_gradient { stops = [ p 0., c "black"; p 1., c "red" ] }));
      [%expect {| background-image: radial-gradient(black 0.000000%, red 100.000000%) |}]
    ;;
  end)
;;
