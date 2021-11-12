open Core
open! Import
module Const_handler = Keyboard_event_handler
module Action = Const_handler.Action
module Command = Const_handler.Command

type 'env t =
  { handler : Const_handler.t
  ; variable_actions : 'env -> Action.t list
  }
[@@deriving sexp_of]

let empty_variable_actions _env = []
let empty = { handler = Const_handler.empty; variable_actions = empty_variable_actions }

let of_const_handler ?(variable_actions = empty_variable_actions) handler =
  { handler; variable_actions }
;;

let to_const_handler t env =
  let variable_actions = t.variable_actions env in
  List.fold variable_actions ~init:t.handler ~f:Const_handler.set_action
;;

let map_handler t f arg = { t with handler = f t.handler arg }
let add_action_exn t = map_handler t Const_handler.add_action_exn
let add_command_exn t = map_handler t Const_handler.add_command_exn
let add_disabled_key_exn t = map_handler t Const_handler.add_disabled_key_exn
let set_action t = map_handler t Const_handler.set_action
let set_command t = map_handler t Const_handler.set_command
let set_disabled_key t = map_handler t Const_handler.set_disabled_key

let add_variable_actions t actions =
  { t with variable_actions = (fun env -> t.variable_actions env @ actions env) }
;;

let add_variable_commands t commands =
  add_variable_actions t (fun env -> List.map (commands env) ~f:Action.command)
;;

let add_variable_disabled_keys t keys =
  add_variable_actions t (fun env -> List.map (keys env) ~f:Action.disabled_key)
;;

module Variable_handler_command = struct
  type 'env t =
    { keys : Keystroke.t list
    ; description : string
    ; group : Grouped_help_text.Group_name.t option
    ; handler : 'env -> Const_handler.Handler.t
    }

  let to_const { keys; description; group; handler } env : Command.t =
    { keys; description; group; handler = handler env }
  ;;

  let get_help_text { keys; description; _ } : Help_text.Command.t = { keys; description }
end

module Variable_handler_action = struct
  type 'env t =
    | Command of 'env Variable_handler_command.t
    | Disabled_key of Keystroke.t

  let to_const action env : Action.t =
    match action with
    | Command command -> Command (Variable_handler_command.to_const command env)
    | Disabled_key key -> Disabled_key key
  ;;

  let get_help_text action : Help_text.Command.t =
    match action with
    | Command command -> Variable_handler_command.get_help_text command
    | Disabled_key key -> Keyboard_event_handler.Action.get_help_text (Disabled_key key)
  ;;
end

let add_variable_handler_action t action =
  add_variable_actions t (fun env -> [ Variable_handler_action.to_const action env ])
;;

let add_variable_handler_command t command =
  add_variable_handler_action t (Command command)
;;
