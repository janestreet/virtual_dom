open Virtual_dom.Vdom.Attr
open Core_kernel

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

val viewbox : min_x:float -> min_y:float -> width:float -> height:float -> t

(** {1 <a>} *)

val href : string -> t

(** {1 many things} *)

val transform : transform_op list -> t

(** {1 <circle>} *)

val cx : float -> t
val cy : float -> t
val r : float -> t

(** nothing for <defs> *)

(** {1 <ellipse>} *)

(** shares cx and cy with circle*)

val rx : float -> t
val ry : float -> t

(** nothing for <g> *)

(** {1 <image>} *)

val x : float -> t
val y : float -> t
val width : float -> t
val height : float -> t
val xlink_href : string -> t

val preserve_aspect_ratio
  :  align:align_options
  -> ?meet_or_slice:[ `Meet | `Slice ]
  -> unit
  -> t

(** {1 <line>} *)

val x1 : float -> t
val x2 : float -> t
val y1 : float -> t
val y2 : float -> t

(** {1 <linearGradient> } *)

(** shares href with <a>, x1, x2, y1, y2 with <line> *)

val gradient_units : units -> t
val gradient_transform : transform_op list -> t

(** {1 <marker>} *)

(** viewBox is shared with <svg>, preserveAspecteRatio is shared with <image> *)

val marker_height : float -> t
val marker_width : float -> t
val marker_units : units -> t
val orient : [ `Angle of angle | `Auto | `Auto_start_reverse ] -> t
val refX : float -> t
val refY : float -> t

(** {1 <mask>} *)

(** shares width, height, x, and y with <image> *)

val mask_units : units -> t
val mask_content_units : units -> t

(** {1 <path>} *)

val d : path_op list -> t

(** {1 <polygon>} *)

val points : (float * float) list -> t
val fill : [ `Url of string | Css_gen.Color.t ] -> t
val stroke : Css_gen.Color.t -> t
val stroke_width : float -> t
val stroke_linecap : [ `Butt | `Round | `Square ] -> t
val stroke_dasharray : float list -> t

(** {1 <polyline>} *)

(** shares points with <polygon> *)

(** {1 <radialGradient>} *)

(** shares spreadMethod with <linearGradient> shares cx, cy, r with <circle> *)

val fx : float -> t
val fy : float -> t
val fr : float -> t
val spread_method : [ `Pad | `Reflect | `Repeat ] -> t

(** Nothing to do for <rect> because shares x, y, rx and ry width,
    height with <image> *)

(** {1 <stop>} *)

val offset : Percent.t -> t
val stop_color : Css_gen.Color.t -> t
val stop_opacity : Percent.t -> t

(** nothing to do for <style> *)

(** Nothing to do for <symbol> as refX and refY is shared with
    <marker>, viewBox is shared with <svg>, x, y, width, and
    height are shared with <image>, *)

(** {1 <text>} *)

(** x and y is shared with <image> *)

val dx : float -> t
val dy : float -> t

module Text : sig
  val start_offset
    :  [ Css_gen.Length.t | `Percentage of Percent.t | `Number of float ]
    -> t

  val text_length
    :  [ Css_gen.Length.t | `Percentage of Percent.t | `Number of float ]
    -> t

  (** {1 <textPath>} *)

  (** shares textLength with <text> *)

  val length_adjust : [ `Spacing | `Spacing_and_glyphs ] -> t
  val side : [ `Left | `Right ] -> t

  (** Nothing to do for <title>
      Nothing to do for <tspan> as it shares everything with text 
      Nothing to do for <use> as it shares href with <linearGradient>, x, y,
      width, and height with <image> *)
  val spacing : [ `Auto | `Exact ] -> t
end
