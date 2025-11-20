open! Core
open Import
open Dom_html

type t = keyboardEvent Js.t

module Keyboard_code = Keyboard_code

let key e = Keyboard_code.of_event e

(* This function looks silly (and it is): why not just use [Js.to_bool]? [robust_to_bool]
   handles [bool Js.t] that _might_ actually be [bool Js.t Js.Optdef.t].

   This is an important distinction to make if someone lied when submitting a keyboard
   event and it doesn't have the properties that we expect. *)
let robust_to_bool b = phys_equal b Js._true

let modifier e ~states ~fallback =
  (* we don't trust that [fallback] is actually a [bool Js.t] because it was pulled out of
     an event that could be missing the fields that the DOM promises exists, so we use
     [robust_to_bool] instead.

     To my knowledge this can only happen if a user manually submits an event that is
     missing these fields, but it's better to be safe. *)
  match robust_to_bool fallback with
  | true -> true
  | false ->
    (* Some [KeyboardEvent]s don't have a getModifierState function for some reason. This
       issue was first noticed in a typeahead component, but it may be an issue for other
       input elements too. If we can't find this property, we bail. *)
    let has_get_modifier_state_function =
      Js.Unsafe.get e (Js.string "getModifierState") |> Js.Optdef.test
    in
    (match has_get_modifier_state_function with
     | false -> false
     | true ->
       List.exists states ~f:(fun state_name ->
         robust_to_bool (e##getModifierState (Js.string state_name))))
;;

let ctrl e = modifier e ~states:[ "Control"; "AltGraph" ] ~fallback:e##.ctrlKey
let alt e = modifier e ~states:[ "Alt"; "AltGraph" ] ~fallback:e##.altKey
let shift e = modifier e ~states:[ "Shift" ] ~fallback:e##.shiftKey
let meta e = modifier e ~states:[ "Meta" ] ~fallback:e##.metaKey

let match_modifiers ?ctrl:ctrl' ?alt:alt' ?shift:shift' ?meta:meta' e =
  List.for_all
    [ ctrl', ctrl e; alt', alt e; shift', shift e; meta', meta e ]
    ~f:(fun (cond, env) -> Option.value_map cond ~default:true ~f:(Bool.equal env))
;;

let no_modifiers e = match_modifiers ~ctrl:false ~alt:false ~shift:false ~meta:false e
let map e ~f = f (`Ctrl (ctrl e), `Alt (alt e), `Shift (shift e), `Meta (meta e), key e)
