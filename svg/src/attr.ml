open Core_kernel
open Virtual_dom.Vdom.Attr
module Number = Virtual_dom.Dom_float

type align_options =
  | None
  | X_max_y_max
  | X_max_y_mid
  | X_max_y_min
  | X_mid_y_max
  | X_mid_y_mid
  | X_mid_y_min
  | X_min_y_max
  | X_min_y_mid
  | X_min_y_min

type units =
  | Object_bounding_box
  | User_space_on_use

type angle =
  | Deg of float
  | Grad of float
  | Rad of float
  | Turn of float

type path_op =
  | Move_to_abs of
      { x : float
      ; y : float
      }
  | Move_to_rel of
      { x : float
      ; y : float
      }
  | Line_to_abs of
      { x : float
      ; y : float
      }
  | Line_to_rel of
      { x : float
      ; y : float
      }
  | Arc_to_abs of
      { rx : float
      ; ry : float
      ; x_axis_rotation : float
      ; large_arc : bool
      ; sweep : bool
      ; x : float
      ; y : float
      }
  | Arc_to_rel of
      { rx : float
      ; ry : float
      ; x_axis_rotation : float
      ; large_arc : bool
      ; sweep : bool
      ; dx : float
      ; dy : float
      }
  | Cubic_abs of
      { x1 : float
      ; y1 : float
      ; x2 : float
      ; y2 : float
      ; x : float
      ; y : float
      }
  | Cubic_rel of
      { x1 : float
      ; y1 : float
      ; x2 : float
      ; y2 : float
      ; x : float
      ; y : float
      }
  | Cubic_smooth_abs of
      { x2 : float
      ; y2 : float
      ; x : float
      ; y : float
      }
  | Cubic_smooth_rel of
      { x2 : float
      ; y2 : float
      ; x : float
      ; y : float
      }
  | Quadratic_abs of
      { x1 : float
      ; y1 : float
      ; x : float
      ; y : float
      }
  | Quadratic_rel of
      { x1 : float
      ; y1 : float
      ; x : float
      ; y : float
      }
  | Quadratic_smooth_abs of
      { x : float
      ; y : float
      }
  | Quadratic_smooth_rel of
      { x : float
      ; y : float
      }
  | Close_path

type transform_op =
  | Matrix of float * float * float * float * float * float
  | Translate of
      { dx : float
      ; dy : float
      }
  | Scale of
      { sx : float
      ; sy : float
      }
  | Rotate of
      { a : [ `Deg of float ]
      ; x : float
      ; y : float
      }
  | Skew_x of float
  | Skew_y of float

let viewbox =
  let c = create "viewBox" in
  fun ~min_x ~min_y ~width ~height ->
    c (sprintf !"%{Number} %{Number} %{Number} %{Number}" min_x min_y width height)
;;

let href = create "href"
let cx = create_float "cx"
let cy = create_float "cy"
let r = create_float "r"
let rx = create_float "rx"
let ry = create_float "ry"
let x = create_float "x"
let y = create_float "y"
let width = create_float "width"
let height = create_float "height"
let xlink_href = create "xlink:href"

let transform_op_to_string = function
  | Matrix (a, b, c, d, e, f) ->
    sprintf
      !"matrix(%{Number}, %{Number}, %{Number}, %{Number}, %{Number}, %{Number})"
      a
      b
      c
      d
      e
      f
  | Translate { dx; dy } -> sprintf !"translate(%{Number}, %{Number})" dx dy
  | Scale { sx; sy } -> sprintf !"scale(%{Number}, %{Number})" sx sy
  | Rotate { a = `Deg a; x; y } ->
    sprintf !"rotate(%{Number}, %{Number}, %{Number})" a x y
  | Skew_x s -> sprintf !"skewX(%{Number})" s
  | Skew_y s -> sprintf !"skewY(%{Number})" s
;;

let transform transforms =
  transforms
  |> List.map ~f:transform_op_to_string
  |> String.concat ~sep:" "
  |> create "transform"
;;

let preserve_aspect_ratio =
  let c = create "preserveAspectRatio" in
  fun ~(align : align_options) ?meet_or_slice () ->
    let a =
      match align with
      | None -> "none"
      | X_min_y_min -> "xMinYMin"
      | X_mid_y_min -> "xMidYMin"
      | X_max_y_min -> "xMaxYMin"
      | X_min_y_mid -> "xMinYMid"
      | X_mid_y_mid -> "xMidYMid"
      | X_max_y_mid -> "xMaxYMid"
      | X_min_y_max -> "xMinYMax"
      | X_mid_y_max -> "xMidYMax"
      | X_max_y_max -> "xMaxYMax"
    in
    let m =
      match meet_or_slice with
      | None -> a
      | Some `Meet -> sprintf "%s meet" a
      | Some `Slice -> sprintf "%s slice" a
    in
    c m
;;

let x1 = create_float "x1"
let x2 = create_float "x2"
let y1 = create_float "y1"
let y2 = create_float "y2"

let gradient_units =
  let c = create "gradientUnits" in
  function
  | User_space_on_use -> c "userSpaceOnUse"
  | Object_bounding_box -> c "objectBoundingBox"
;;

let gradient_transform transforms =
  transforms
  |> List.map ~f:transform_op_to_string
  |> String.concat ~sep:" "
  |> create "gradientTransform"
;;

let spread_method =
  let c = create "spreadMethod" in
  function
  | `Pad -> c "pad"
  | `Reflect -> c "reflect"
  | `Repeat -> c "repeat"
;;

let marker_height = create_float "markerHeight"
let marker_width = create_float "markerWidth"

let units_helper c = function
  | User_space_on_use -> c "userSpaceOnUse"
  | Object_bounding_box -> c "objectBoundingBox"
;;

let marker_units =
  let c = create "markerUnits" in
  units_helper c
;;

let orient =
  let c = create "orient" in
  function
  | `Auto -> c "auto"
  | `Auto_start_reverse -> c "auto-start-reverse"
  | `Angle (Deg f) -> c (sprintf !"%{Number}deg" f)
  | `Angle (Grad f) -> c (sprintf !"%{Number}grad" f)
  | `Angle (Rad f) -> c (sprintf !"%{Number}rad" f)
  | `Angle (Turn f) -> c (sprintf !"%{Number}turn" f)
;;

let refX = create_float "refX"
let refY = create_float "refY"

let mask_units =
  let c = create "maskUnits" in
  units_helper c
;;

let mask_content_units =
  let c = create "maskContentUnits" in
  units_helper c
;;

let d =
  let c = create "d" in
  fun commands ->
    commands
    |> List.map ~f:(function
      | Move_to_abs { x; y } -> sprintf !"M%{Number},%{Number}" x y
      | Move_to_rel { x; y } -> sprintf !"m%{Number},%{Number}" x y
      | Line_to_abs { x; y } -> sprintf !"L%{Number},%{Number}" x y
      | Line_to_rel { x; y } -> sprintf !"l%{Number},%{Number}" x y
      | Arc_to_abs { rx; ry; x_axis_rotation; large_arc; sweep; x; y } ->
        sprintf
          !"A %{Number} %{Number}, %{Number}, %{Int}, %{Int}, %{Number} %{Number}"
          rx
          ry
          x_axis_rotation
          (if large_arc then 1 else 0)
          (if sweep then 1 else 0)
          x
          y
      | Arc_to_rel { rx; ry; x_axis_rotation; large_arc; sweep; dx; dy } ->
        sprintf
          !"a %{Number} %{Number} %{Number} %{Int} %{Int} %{Number} %{Number}"
          rx
          ry
          x_axis_rotation
          (if large_arc then 1 else 0)
          (if sweep then 1 else 0)
          dx
          dy
      | Cubic_abs { x1; y1; x2; y2; x; y } ->
        sprintf
          !"C %{Number},%{Number},%{Number},%{Number},%{Number},%{Number}"
          x1
          y1
          x2
          y2
          x
          y
      | Cubic_rel { x1; y1; x2; y2; x; y } ->
        sprintf
          !"c %{Number},%{Number},%{Number},%{Number},%{Number},%{Number}"
          x1
          y1
          x2
          y2
          x
          y
      | Cubic_smooth_abs { x2; y2; x; y } ->
        sprintf !"S %{Number},%{Number},%{Number},%{Number}" x2 y2 x y
      | Cubic_smooth_rel { x2; y2; x; y } ->
        sprintf !"s %{Number},%{Number},%{Number},%{Number}" x2 y2 x y
      | Quadratic_abs { x1; y1; x; y } ->
        sprintf !"Q %{Number},%{Number},%{Number},%{Number}" x1 y1 x y
      | Quadratic_rel { x1; y1; x; y } ->
        sprintf !"q %{Number},%{Number},%{Number},%{Number}" x1 y1 x y
      | Quadratic_smooth_abs { x; y } -> sprintf !"T %{Number},%{Number}" x y
      | Quadratic_smooth_rel { x; y } -> sprintf !"t %{Number},%{Number}" x y
      | Close_path -> "Z")
    |> String.concat
    |> c
;;

let points =
  let c = create "points" in
  fun points ->
    points
    |> List.map ~f:(fun (x, y) -> sprintf !"%{Number},%{Number}" x y)
    |> String.concat ~sep:" "
    |> c
;;

let fill f =
  let s =
    match f with
    | `Url url -> sprintf "url(%s)" url
    | #Css_gen.Color.t as color -> Css_gen.Color.to_string_css color
  in
  create "fill" s
;;

let stroke style =
  let c = create "stroke" in
  c (Css_gen.Color.to_string_css style)
;;

(*
   val stroke_width: float -> t
   val stroke_linecap: [`Butt | `Round | `Square] -> t
   val stroke_dasharray: float list -> t
*)
let stroke_width = create_float "stroke-width"

let stroke_linecap linecap =
  create
    "stroke-linecap"
    (match linecap with
     | `Butt -> "butt"
     | `Round -> "round"
     | `Square -> "square")
;;

let stroke_dasharray dashes =
  dashes
  |> List.map ~f:(sprintf !"%{Number}")
  |> String.concat ~sep:","
  |> create "stroke-dasharray"
;;

let fx = create_float "fx"
let fy = create_float "fy"
let fr = create_float "fr"

let offset percent =
  percent |> Percent.to_percentage |> sprintf "%.2f%%" |> create "offset"
;;

let stop_color color =
  let c = create "stop-color" in
  c (Css_gen.Color.to_string_css color)
;;

let stop_opacity percent =
  percent |> Percent.to_mult |> sprintf "%.5f" |> create "stop-opacity"
;;

let dx = create_float "dx"
let dy = create_float "dy"

module Text = struct
  let length_adjust =
    let attr_c = create "lengthAdjust" in
    function
    | `Spacing -> attr_c "spacing"
    | `Spacing_and_glyphs -> attr_c "spacingAndGlyphs"
  ;;

  let side =
    let c = create "side" in
    function
    | `Left -> c "left"
    | `Right -> c "right"
  ;;

  let spacing =
    let c = create "spacing" in
    function
    | `Auto -> c "auto"
    | `Exact -> c "exact"
  ;;

  let length_helper c = function
    | #Css_gen.Length.t as length -> c (Css_gen.Length.to_string_css length)
    | `Percentage p -> c (sprintf !"%{Number}%%" (Percent.to_percentage p))
    | `Number n -> c (Number.to_string n)
  ;;

  let start_offset =
    let c = create "startOffset" in
    length_helper c
  ;;

  let text_length =
    let c = create "textLengh" in
    length_helper c
  ;;
end
