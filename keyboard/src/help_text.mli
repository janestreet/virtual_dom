open Import

(** A [Help_text.t] represents the documentation for a collection of commands. It can be
    displayed as a Vdom node.

    This can be used to create a web ui help menu.
*)

module View_spec : sig
  (** A [View_spec.t] allows the user to customize how the help text is converted to a
      Vdom node.
  *)
  type t =
    { key : string -> Vdom.Node.t
    ; plain_text : string -> Vdom.Node.t
    }

  (** [plain] converts all help text parts to text nodes without further modification. *)
  val plain : t

  (** [with_classes] converts all help text parts to text nodes and wraps them in spans
      with the given classes. This allows the user to style the help text using CSS.
  *)
  val with_classes : key_class:string -> plain_text_class:string -> t
end

module Command : sig
  (** A [Command.t] represents the help text for a single command. *)

  type t =
    { keys : Keystroke.t list
    ; description : string
    }
  [@@deriving sexp]

  module Format : sig
    type t =
      [ `Keys of [ `Sep of string ]
      | (* separator between keys *)
        (* This function can be used to modify the description before displaying it, e.g.
           capitalizing/uncapitalizing it. *)
        `Description of
          (string -> string) option
      | `Text of string
      ]
        list

    (** The [default] format is:

        "Press <[key1] or [key2] or ... [keyn]> to <uncapitalized description>."
    *)
    val default : t
  end

  (** [view] displays a help text line for a single command. For instance, this can be
      used to display the help text for opening a help menu.
  *)
  val view : t -> View_spec.t -> Format.t -> Vdom.Node.t
end

type t [@@deriving sexp, compare]

val empty : t
val is_empty : t -> bool
val of_command_list : Command.t list -> t

(** [add_command] is linear in the number of commands in [t]. *)
val add_command : t -> Command.t -> t

val commands : t -> Command.t list

(** [view] displays a help text table with one row per command.

    [view_rows] is similar to [view], but returns a list of row nodes instead of wrapping
    them in a table node.
*)
val view : ?sep:string -> t -> View_spec.t -> Vdom.Node.t

val view_rows : ?sep:string -> t -> View_spec.t -> Vdom.Node.t list
