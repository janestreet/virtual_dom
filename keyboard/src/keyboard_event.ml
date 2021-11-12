open! Core
open Import
open Dom_html

type t = keyboardEvent Js.t

module Keyboard_code = Keyboard_code

let key e = Keyboard_code.of_event e
let ctrl e = Js.to_bool e##.ctrlKey
let alt e = Js.to_bool e##.altKey
let shift e = Js.to_bool e##.shiftKey
let meta e = Js.to_bool e##.metaKey

let match_modifiers ?ctrl:ctrl' ?alt:alt' ?shift:shift' ?meta:meta' e =
  List.for_all
    [ ctrl', ctrl e; alt', alt e; shift', shift e; meta', meta e ]
    ~f:(fun (cond, env) -> Option.value_map cond ~default:true ~f:(Bool.equal env))
;;

let no_modifiers e = match_modifiers ~ctrl:false ~alt:false ~shift:false ~meta:false e
let map e ~f = f (`Ctrl (ctrl e), `Alt (alt e), `Shift (shift e), `Meta (meta e), key e)
