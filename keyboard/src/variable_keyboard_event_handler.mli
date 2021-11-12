open Keyboard_event_handler

(** A [Variable_keyboard_event_handler.t] provides a way of representing a keyboard event
    handler that contains both a (possibly empty) constant set of actions and a variable
    set of actions that depends on some ['env] variable. In order to handle keyboard
    events or produce help text, it must first be converted to a
    [Keyboard_event_handler.t] using the function [to_const_handler]. *)

module Action = Action
module Command = Command

type 'env t [@@deriving sexp_of]

val empty : 'env t

val of_const_handler
  :  ?variable_actions:('env -> Action.t list)
  -> Keyboard_event_handler.t
  -> 'env t

(** [add_variable_actions], [add_variable_commands], and [add_variable_disabled_keys] add
    a new variable set of actions to a variable keyboard event handler. This does not
    replace any existing variable actions in the handler, but instead adds to them. *)
val add_variable_actions : 'env t -> ('env -> Action.t list) -> 'env t

val add_variable_commands : 'env t -> ('env -> Command.t list) -> 'env t
val add_variable_disabled_keys : 'env t -> ('env -> Keystroke.t list) -> 'env t

(** The [add_*_exn] and [set_*] functions below behave in the same was as the
    corresponding functions in [Keyboard_event_handler]. *)
val add_action_exn : 'env t -> Action.t -> 'env t

val add_command_exn : 'env t -> Command.t -> 'env t
val add_disabled_key_exn : 'env t -> Keystroke.t -> 'env t
val set_action : 'env t -> Action.t -> 'env t
val set_command : 'env t -> Command.t -> 'env t
val set_disabled_key : 'env t -> Keystroke.t -> 'env t

(** [to_const_handler] evaluates the variable set of actions for the given ['env] value,
    and combines them with the constant set of actions to create a keyboard event handler.

    It is possible that for a given ['env] value, multiple actions are defined for the
    same key. In that case, the latest variable action is used when creating the constant
    keyboard event handler. *)
val to_const_handler : 'env t -> 'env -> Keyboard_event_handler.t

(** [Variable_handler_command] and [Variable_handler_action] provide a way of representing
    commands whose keys, description and group are constant, but whose handler varies with
    the ['env] variable.

    This is useful because it provides a way of generating help text for these commands
    that does not depend on or vary with the ['env] variable. *)
module Variable_handler_command : sig
  type 'env t =
    { keys : Keystroke.t list
    ; description : string
    ; group : Grouped_help_text.Group_name.t option
    ; handler : 'env -> Handler.t
    }

  val get_help_text : _ t -> Help_text.Command.t
end

module Variable_handler_action : sig
  type 'env t =
    | Command of 'env Variable_handler_command.t
    | Disabled_key of Keystroke.t

  val get_help_text : _ t -> Help_text.Command.t
end

(** [add_variable_handler_action] and [add_variable_handler_command] are utility functions
    for adding variable handler actions to an existing variable keyboard event handler.
    Under the hood, the variable handler action is converted to a variable action of the
    form ['env -> Action.t]. *)
val add_variable_handler_action : 'env t -> 'env Variable_handler_action.t -> 'env t

val add_variable_handler_command : 'env t -> 'env Variable_handler_command.t -> 'env t
