(** A collection of CSS attributes. *)
open Base

type t

val to_list : t -> Attr.t list
val of_list : Attr.t list -> t

(** merge tries to do sensible things for style and class, e.g.  {v
      "class=foo;style=color:blue;text-align:center"
    + "class=bar;background=red"
    = "class=foo bar;style=color:blue;text-align:center:background=red" v} *)
val merge : t -> t -> t

(** If there is no style attribute the empty Css.t will be passed to f.
    Most of the time you probably want to use add_style instead. *)
val map_style : t -> f:(Css.t -> Css.t) -> t

val add_style : t -> Css.t -> t

(** If there is no class attribute the empty Set will be passed to f.
    Most of the time you probably want to use add_class instead. *)
val map_class : t -> f:(Set.M(String).t -> Set.M(String).t) -> t

val add_class : t -> string -> t
