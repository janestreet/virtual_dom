(** A collection of CSS attributes. *)
open Base

type t = Attr.t list

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

(** If there is no class attribute the empty Set will be passed to f.
    Most of the time you probably want to use add_class instead. *)
val map_class : t -> f:(Set.M(String).t -> Set.M(String).t) -> t

val add_class : t -> string -> t
