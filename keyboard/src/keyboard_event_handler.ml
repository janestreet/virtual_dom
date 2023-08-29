open Core
open Import

module Condition = struct
  type t = Keyboard_event.t -> bool

  let true_ _ev = true
  let false_ _ev = false
  let not_ t1 ev = not (t1 ev)
  let and_ t1 t2 ev = t1 ev && t2 ev
  let or_ t1 t2 ev = t1 ev || t2 ev
  let get_target ev = Js.Opt.to_option ev##.target

  let get_target_id ev =
    Option.map (get_target ev) ~f:(fun elem -> Js.to_string elem##.id)
  ;;

  let get_target_class_list ev =
    Option.map (get_target ev) ~f:(fun elem -> elem##.classList)
  ;;

  let input_cond ev ~on_input ~on_textarea =
    match get_target ev with
    | None -> false
    | Some elem ->
      (match Dom_html.tagged elem with
       | Input i -> on_input i
       | Textarea t -> on_textarea t
       | _ -> false)
  ;;

  let has_input_target ev =
    input_cond ev ~on_input:(fun _ -> true) ~on_textarea:(fun _ -> true)
  ;;

  let has_text_input_target ev =
    input_cond
      ev
      ~on_input:(fun i -> String.equal (Js.to_string i##._type) "text")
      ~on_textarea:(fun _ -> true)
  ;;

  let has_number_input_target ev =
    input_cond
      ev
      ~on_input:(fun i -> String.equal (Js.to_string i##._type) "number")
      ~on_textarea:(fun _ -> false)
  ;;

  let has_form_element_target ev =
    let f i = Option.is_some (Js.Opt.to_option i##.form) in
    input_cond ev ~on_input:f ~on_textarea:f
  ;;

  let has_target_id ~id ev =
    match get_target_id ev with
    | None -> false
    | Some target_id -> String.equal id target_id
  ;;

  let has_target_class ~class_ ev =
    match get_target_class_list ev with
    | None -> false
    | Some class_list -> Js.to_bool (class_list##contains (Js.string class_))
  ;;

  let element_has_target_class ~selector ~class_ _ev =
    match Dom_html.document##querySelector (Js.string selector) |> Js.Opt.to_option with
    | None -> false
    | Some element -> Js.to_bool (element##.classList##contains (Js.string class_))
  ;;
end

module Handler = struct
  open Vdom

  type t = Keyboard_event.t -> unit Ui_effect.t [@@deriving sexp]

  let prevent_default _ev = Effect.Prevent_default
  let with_prevent_default t ev = Effect.Many [ Effect.Prevent_default; t ev ]

  let handle_by_case ?prevent_default ts ev =
    match List.find_map ts ~f:(fun (cond, t) -> Option.some_if (cond ev) t) with
    | None -> Effect.Ignore
    | Some t ->
      let event = t ev in
      (match prevent_default with
       | None -> event
       | Some () -> Effect.Many [ event; Effect.Prevent_default ])
  ;;

  let only_handle_if ?prevent_default cond t = handle_by_case ?prevent_default [ cond, t ]
end

module Uid = Unique_id.Int ()

module Command = struct
  type t =
    { keys : Keystroke.t list
    ; description : string
    ; group : Grouped_help_text.Group_name.t option
    ; handler : Handler.t
    }
  [@@deriving sexp]

  let get_help_text { keys; description; _ } = { Help_text.Command.keys; description }
end

module Action = struct
  type t =
    | Command of Command.t
    | Disabled_key of Keystroke.t
  [@@deriving sexp, variants]

  let keys = function
    | Command command -> command.keys
    | Disabled_key key -> [ key ]
  ;;

  let handler = function
    | Command command -> command.handler
    | Disabled_key _ -> Handler.prevent_default
  ;;

  let get_help_text = function
    | Command command -> Command.get_help_text command
    | Disabled_key key -> { Help_text.Command.keys = [ key ]; description = "Disabled" }
  ;;

  let merge t1 t2 ~keys =
    match t1, t2 with
    | Disabled_key _, Disabled_key _ -> t1
    | Disabled_key _, Command command | Command command, Disabled_key _ ->
      let handler ev =
        Vdom.Effect.Many [ Vdom.Effect.Prevent_default; command.handler ev ]
      in
      Command { command with handler }
    | Command command1, Command command2 ->
      Command
        { keys
        ; description = sprintf "%s/%s" command1.description command2.description
        ; group = Option.first_some command1.group command2.group
        ; handler =
            (fun ev -> Vdom.Effect.Many [ command1.handler ev; command2.handler ev ])
        }
  ;;
end

type t = (Uid.t * Action.t) Keystroke.Map.t [@@deriving sexp_of]

let empty = Keystroke.Map.empty

let new_entries action =
  let data = Uid.create (), action in
  List.map (Action.keys action) ~f:(fun key -> key, data)
;;

let of_action_list_exn actions =
  List.concat_map actions ~f:new_entries |> Keystroke.Map.of_alist_exn
;;

let of_command_list_exn commands =
  of_action_list_exn (List.map commands ~f:Action.command)
;;

let add_action_core t action map_add =
  List.fold (new_entries action) ~init:t ~f:(fun t (key, data) -> map_add t ~key ~data)
;;

let set_action t action = add_action_core t action Map.set
let set_command t command = set_action t (Command command)
let set_disabled_key t key = set_action t (Disabled_key key)
let add_action_exn t action = add_action_core t action Map.add_exn
let add_command_exn t command = add_action_exn t (Command command)
let add_disabled_key_exn t key = add_action_exn t (Disabled_key key)
let merge_core = Map.merge_skewed
let merge_override_with_right = merge_core ~combine:(fun ~key:_ _id1 id2 -> id2)

let merge_exn =
  merge_core ~combine:(fun ~key _ _ -> failwithf !"Duplicate key %{Keystroke#hum}" key ())
;;

module Uid_pair = struct
  module T = struct
    type t = Uid.t * Uid.t [@@deriving sexp, hash, compare]
  end

  include T
  include Hashable.Make (T)
end

(* [merge_both] is complicated because we want to (a) combine all keys that appear in both
   t1 and t2, and (b) remove those keys from any other actions in t1 and t2. *)
let merge_both t1 t2 =
  let combined_keys_by_id_pair = Uid_pair.Table.create () in
  let add_combined_keys_by_id_pair ~id1 ~id2 ~key =
    Hashtbl.update combined_keys_by_id_pair (id1, id2) ~f:(function
      | None -> Uid.create (), [ key ]
      | Some (id, keys) -> id, keys @ [ key ])
  in
  let combined_keys_by_id = Uid.Table.create () in
  let add_combined_keys_by_id ~id ~key =
    Hashtbl.update combined_keys_by_id id ~f:(function
      | None -> Keystroke.Set.singleton key
      | Some keys -> Set.add keys key)
  in
  Map.iter2 t1 t2 ~f:(fun ~key ~data ->
    match data with
    | `Left _ | `Right _ -> ()
    | `Both ((id1, _), (id2, _)) ->
      add_combined_keys_by_id_pair ~id1 ~id2 ~key;
      add_combined_keys_by_id ~id:id1 ~key;
      add_combined_keys_by_id ~id:id2 ~key);
  Map.merge t1 t2 ~f:(fun ~key:_ -> function
    | `Both ((id1, action1), (id2, action2)) ->
      let new_id, keys = Hashtbl.find_exn combined_keys_by_id_pair (id1, id2) in
      let action = Action.merge action1 action2 ~keys in
      Some (new_id, action)
    | `Left (id, action) | `Right (id, action) ->
      (match Hashtbl.find combined_keys_by_id id, action with
       | None, _ -> Some (id, action)
       (* this case is tricky: if the id is in [combined_keys_by_id] and the action is
          disabling a key, then it *should* have appeared in the `Both case. *)
       | Some _, Action.Disabled_key key ->
         failwithf !"bug: [merge] failed on disabled key %{sexp: Keystroke.t}" key ()
       | Some combined_keys, Action.Command command ->
         let keys_left =
           List.filter command.keys ~f:(fun k -> not (Set.mem combined_keys k))
         in
         (match keys_left with
          (* if all the keys were combined, then this action can just go away *)
          | [] -> None
          | keys -> Some (id, Command { command with keys }))))
;;

let merge ~on_dup =
  match on_dup with
  | `Override_with_right -> merge_override_with_right
  | `Both -> merge_both
  | `Throw -> merge_exn
;;

let handle_event t ev =
  Option.map
    (Map.find t (Keystroke.of_event ev))
    ~f:(fun (_, action) -> Action.handler action ev)
;;

let handle_or_ignore_event t ev =
  Option.value ~default:Vdom.Effect.Ignore (handle_event t ev)
;;

let disabled_key_group_name = Grouped_help_text.Group_name.of_string "Disabled keys"

let get_help_text_commands ?include_disabled_keys t =
  let actions =
    Map.fold t ~init:Uid.Map.empty ~f:(fun ~key ~data:(id, action) actions_by_id ->
      Map.update actions_by_id id ~f:(fun prev ->
        let prev_keys =
          match prev with
          | None -> []
          | Some (_, prev_keys) -> prev_keys
        in
        action, prev_keys @ [ key ]))
    |> Map.data
  in
  List.filter_map actions ~f:(fun (action, keys) ->
    match (action : Action.t) with
    | Command command ->
      let command = { command with keys } in
      Some (command.group, Command.get_help_text command)
    | Disabled_key key ->
      Option.map include_disabled_keys ~f:(fun () ->
        Some disabled_key_group_name, Action.get_help_text (Disabled_key key)))
;;

let get_help_text ?include_disabled_keys t =
  let help_text_commands =
    get_help_text_commands ?include_disabled_keys t |> List.map ~f:snd
  in
  Help_text.of_command_list help_text_commands
;;

let get_grouped_help_text_core ?include_disabled_keys ?custom_group_order t ~get_group =
  let help_text_commands =
    get_help_text_commands ?include_disabled_keys t
    |> List.map ~f:(Tuple2.map_fst ~f:get_group)
  in
  Grouped_help_text.of_command_list ?custom_group_order help_text_commands
;;

let get_grouped_help_text ?include_disabled_keys ?custom_group_order t ~default_group =
  get_grouped_help_text_core
    ?include_disabled_keys
    ?custom_group_order
    t
    ~get_group:(Option.value ~default:default_group)
;;

let get_grouped_help_text_exn ?include_disabled_keys ?custom_group_order t =
  get_grouped_help_text_core
    ?include_disabled_keys
    ?custom_group_order
    t
    ~get_group:(Option.value_exn ~here:[%here])
;;

let get_action t keystroke =
  match Map.find t keystroke with
  | Some (_uid, action) -> Some action
  | None -> None
;;

let handled_keystrokes = Map.key_set
