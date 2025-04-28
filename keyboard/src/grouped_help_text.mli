open Core
open Import

(** A [Grouped_help_text.t] is similar to a [Help_text.t], but allows the user to organize
    the commands into groups. *)

module Group_name : Identifiable

(** [View_spec] is almost identical to [Help_text.View_spec], but additionally allows the
    user to customize how to display group names. *)
module View_spec : sig
  type t =
    { core_spec : Help_text.View_spec.t
    ; group_name : Group_name.t -> Vdom.Node.t
    }

  val plain : t

  (** [with_classes] behaves the same as [Help_text.View_spec.with_classes] as far as the
      [core_spec], and additionally converts group names to text nodes and wraps them in
      divs with the given group name class. *)
  val with_classes
    :  group_name_class:string
    -> key_class:string
    -> plain_text_class:string
    -> t
end

module Command = Help_text.Command

type t [@@deriving sexp, compare]

val empty : t
val is_empty : t -> bool

(** In the [*_exn] functions below, the group name is assumed to be unique for each help
    text group, and an exception is raised if duplicate group names are encountered. *)

(** [of_group_list_exn] converts a list of help text groups into a grouped help text. *)
val of_group_list_exn : (Group_name.t * Help_text.t) list -> t

(** [add_group_exn] adds a new group to a grouped help text. This is linear in the number
    of groups already in the grouped help text. *)
val add_group_exn : t -> Group_name.t -> Help_text.t -> t

(** [groups] returns the help text groups in a grouped help text. *)
val groups : t -> (Group_name.t * Help_text.t) list

(** [of_command_list], [add_command], and [commands] are analogous to the corresponding
    [group] functions above, but deal with single commands instead of help text groups.

    Commands with the same group name are grouped together.

    By default, group order is determined by the order in which the groups first appear in
    the command list. However, if [custom_group_order] is given, it will be used to
    determine the group order instead. Any groups that appear in [custom_group_order] but
    not in the command list will be omitted. Any groups that appear in the command list
    but not in [custom_group_order] will be added to the end of [custom_group_order], in
    the order in which they first appear in the command list.

    [add_command] is linear in both the number of groups in the grouped help text and the
    number of commands already in its group. *)
val of_command_list
  :  ?custom_group_order:Group_name.t list
  -> (Group_name.t * Command.t) list
  -> t

val add_command : t -> Group_name.t -> Command.t -> t
val commands : t -> (Group_name.t * Command.t) list

(** [view] displays a help text table with one row per command, organized into groups.
    Each group has a row containing the group name preceding the rows corresponding to the
    group's commands. *)
val view : t -> View_spec.t -> Vdom.Node.t
