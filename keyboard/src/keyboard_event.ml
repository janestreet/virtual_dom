open! Core
open Import
open Dom_html

type t = keyboardEvent Js.t

module Keyboard_code = Keyboard_code

let key e = Keyboard_code.of_event e

let modifier e ~states ~fallback =
  let from_state =
    List.exists states ~f:(fun state_name ->
      Js.to_bool (e##getModifierState (Js.string state_name)))
  in
  let from_fallback = Js.to_bool fallback in
  from_state || from_fallback
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
