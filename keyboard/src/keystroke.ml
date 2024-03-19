open Core
open Js_of_ocaml

module Keyboard_code = struct
  module T = struct
    type t = Dom_html.Keyboard_code.t =
      | Unidentified
      | KeyA
      | KeyB
      | KeyC
      | KeyD
      | KeyE
      | KeyF
      | KeyG
      | KeyH
      | KeyI
      | KeyJ
      | KeyK
      | KeyL
      | KeyM
      | KeyN
      | KeyO
      | KeyP
      | KeyQ
      | KeyR
      | KeyS
      | KeyT
      | KeyU
      | KeyV
      | KeyW
      | KeyX
      | KeyY
      | KeyZ
      | Digit0
      | Digit1
      | Digit2
      | Digit3
      | Digit4
      | Digit5
      | Digit6
      | Digit7
      | Digit8
      | Digit9
      | Minus
      | Equal
      | Tab
      | Enter
      | Space
      | Escape
      | Backspace
      | Insert
      | Delete
      | CapsLock
      | BracketLeft
      | BracketRight
      | Semicolon
      | Quote
      | Backquote
      | Backslash
      | Comma
      | Period
      | Slash
      | F1
      | F2
      | F3
      | F4
      | F5
      | F6
      | F7
      | F8
      | F9
      | F10
      | F11
      | F12
      | Numpad0
      | Numpad1
      | Numpad2
      | Numpad3
      | Numpad4
      | Numpad5
      | Numpad6
      | Numpad7
      | Numpad8
      | Numpad9
      | NumpadMultiply
      | NumpadSubtract
      | NumpadAdd
      | NumpadDecimal
      | NumpadEqual
      | NumpadEnter
      | NumpadDivide
      | NumLock
      | ControlLeft
      | ControlRight
      | MetaLeft
      | MetaRight
      | ShiftLeft
      | ShiftRight
      | AltLeft
      | AltRight
      | ArrowLeft
      | ArrowRight
      | ArrowUp
      | ArrowDown
      | PageUp
      | PageDown
      | Home
      | End
      | VolumeMute
      | VolumeDown
      | VolumeUp
      | MediaTrackPrevious
      | MediaTrackNext
      | MediaPlayPause
      | MediaStop
      | ContextMenu
      | BrowserSearch
      | BrowserHome
      | BrowserFavorites
      | BrowserRefresh
      | BrowserStop
      | BrowserForward
      | BrowserBack
      | OSLeft
      | OSRight
      | ScrollLock
      | PrintScreen
      | IntlBackslash
      | IntlYen
      | Pause
    [@@deriving sexp, compare, bin_io, hash, enumerate, equal, sexp_grammar]
  end

  include T
  include Sexpable.To_stringable (T)

  let unidentified = 100000

  let to_key_code_left = function
    | ShiftLeft -> 16
    | ControlLeft -> 17
    | AltLeft -> 18
    | MetaLeft -> 91
    | _ -> unidentified
  ;;

  let to_key_code_right = function
    | ShiftRight -> 16
    | ControlRight -> 17
    | AltRight -> 18
    | MetaRight -> 91
    | _ -> unidentified
  ;;

  let to_key_code_numpad = function
    | NumpadDecimal -> 46
    | Numpad0 -> 45
    | Numpad1 -> 35
    | Numpad2 -> 40
    | Numpad3 -> 34
    | Numpad4 -> 37
    | Numpad5 -> 12
    | Numpad6 -> 39
    | Numpad7 -> 36
    | Numpad8 -> 38
    | Numpad9 -> 33
    | NumpadEnter -> 13
    | NumpadDivide -> 111
    | NumpadAdd -> 107
    | NumpadSubtract -> 109
    | NumpadMultiply -> 106
    | _ -> unidentified
  ;;

  let to_key_code_normal = function
    | Escape -> 27
    | F1 -> 112
    | F2 -> 113
    | F3 -> 114
    | F4 -> 115
    | F5 -> 116
    | F6 -> 117
    | F7 -> 118
    | F8 -> 119
    | F9 -> 120
    | F10 -> 121
    | F11 -> 122
    | F12 -> 123
    | PrintScreen -> 42
    | ScrollLock -> 145
    | Pause -> 19
    | Backquote -> 192
    | Digit1 -> 49
    | Digit2 -> 50
    | Digit3 -> 51
    | Digit4 -> 52
    | Digit5 -> 53
    | Digit6 -> 54
    | Digit7 -> 55
    | Digit8 -> 56
    | Digit9 -> 57
    | Digit0 -> 48
    | Minus -> 189
    | Equal -> 187
    | Backspace -> 8
    | Tab -> 9
    | KeyQ -> 81
    | KeyW -> 87
    | KeyE -> 69
    | KeyR -> 82
    | KeyT -> 84
    | KeyY -> 89
    | KeyU -> 85
    | KeyI -> 73
    | KeyO -> 79
    | KeyP -> 80
    | BracketLeft -> 219
    | BracketRight -> 221
    | Backslash -> 220
    | CapsLock -> 20
    | KeyA -> 65
    | KeyS -> 83
    | KeyD -> 68
    | KeyF -> 70
    | KeyG -> 71
    | KeyH -> 72
    | KeyJ -> 74
    | KeyK -> 75
    | KeyL -> 76
    | Semicolon -> 186
    | Quote -> 222
    | Enter -> 13
    | KeyZ -> 90
    | KeyX -> 88
    | KeyC -> 67
    | KeyV -> 86
    | KeyB -> 66
    | KeyN -> 78
    | KeyM -> 77
    | Comma -> 188
    | Period -> 190
    | Slash -> 191
    | Space -> 32
    | ContextMenu -> 93
    | Insert -> 45
    | Home -> 36
    | PageUp -> 33
    | Delete -> 46
    | End -> 35
    | PageDown -> 34
    | ArrowLeft -> 37
    | ArrowDown -> 40
    | ArrowRight -> 39
    | ArrowUp -> 38
    | _ -> unidentified
  ;;

  let to_location (t : t) =
    match t with
    | Unidentified
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ -> `Other
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | Minus
    | Equal
    | Tab
    | Enter
    | Space
    | Escape
    | Backspace
    | Insert
    | Delete
    | CapsLock
    | BracketLeft
    | BracketRight
    | Semicolon
    | Quote
    | Backquote
    | Backslash
    | Comma
    | Period
    | Slash
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | NumLock
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | PageUp
    | PageDown
    | Home
    | End
    | VolumeMute
    | VolumeDown
    | VolumeUp
    | MediaTrackPrevious
    | MediaTrackNext
    | MediaPlayPause
    | MediaStop
    | ContextMenu
    | BrowserSearch
    | BrowserHome
    | BrowserFavorites
    | BrowserRefresh
    | BrowserStop
    | BrowserForward
    | BrowserBack
    | OSLeft
    | OSRight
    | ScrollLock
    | PrintScreen
    | IntlBackslash
    | IntlYen
    | Pause -> `Other
    | Numpad0
    | Numpad1
    | Numpad2
    | Numpad3
    | Numpad4
    | Numpad5
    | Numpad6
    | Numpad7
    | Numpad8
    | Numpad9
    | NumpadMultiply
    | NumpadSubtract
    | NumpadAdd
    | NumpadDecimal
    | NumpadEqual
    | NumpadEnter
    | NumpadDivide -> `Numpad
    | ShiftLeft | MetaLeft | AltLeft | ControlLeft -> `Left
    | ControlRight | MetaRight | ShiftRight | AltRight -> `Right
  ;;

  let to_key_code t =
    match to_location t with
    | `Left -> to_key_code_left t
    | `Right -> to_key_code_right t
    | `Numpad -> to_key_code_numpad t
    | `Other -> to_key_code_normal t
  ;;

  let to_location t =
    match to_location t with
    | `Left -> 1
    | `Right -> 2
    | `Numpad -> 3
    | `Other -> 4
  ;;

  let of_event = Dom_html.Keyboard_code.of_event
end

module T = struct
  type t =
    { key : Keyboard_code.t
    ; ctrl : bool
    ; alt : bool
    ; shift : bool
    ; meta : bool
    }
  [@@deriving sexp, compare, bin_io, hash, fields ~getters, sexp_grammar]
end

include T
include Comparable.Make_binable (T)
include Hashable.Make_binable (T)

let create key ~ctrl ~alt ~shift ~meta = { key; ctrl; alt; shift; meta }

let create' ?ctrl ?alt ?shift ?meta key =
  { key
  ; ctrl = Option.is_some ctrl
  ; alt = Option.is_some alt
  ; shift = Option.is_some shift
  ; meta = Option.is_some meta
  }
;;

let of_event (ev : Keyboard_event.t) =
  let key = Keyboard_event.key ev in
  let ctrl = Keyboard_event.ctrl ev in
  let alt = Keyboard_event.alt ev in
  let shift = Keyboard_event.shift ev in
  let meta = Keyboard_event.meta ev in
  create key ~ctrl ~alt ~shift ~meta
;;

module With_prefix = struct
  type t =
    | Key of string
    | Digit of string
    | Numpad of string
    | Arrow of string
    | Other of string
  [@@deriving variants]

  let of_string str =
    List.find_map
      [ "Key", key; "Digit", digit; "Numpad", numpad; "Arrow", arrow ]
      ~f:(fun (prefix, f) ->
        if String.is_prefix str ~prefix
        then Some (f (String.drop_prefix str (String.length prefix)))
        else None)
    |> Option.value ~default:(Other str)
  ;;
end

let shift_combo_to_string (key : Keyboard_code.t) =
  match key with
  | Digit0 -> Some ")"
  | Digit1 -> Some "!"
  | Digit2 -> Some "@"
  | Digit3 -> Some "#"
  | Digit4 -> Some "$"
  | Digit5 -> Some "%"
  | Digit6 -> Some "^"
  | Digit7 -> Some "&"
  | Digit8 -> Some "*"
  | Digit9 -> Some "("
  | Minus -> Some "_"
  | Equal -> Some "+"
  | BracketLeft -> Some "{"
  | BracketRight -> Some "}"
  | Semicolon -> Some ":"
  | Quote -> Some "\""
  | Backquote -> Some "~"
  | Backslash -> Some "|"
  | Comma -> Some "<"
  | Period -> Some ">"
  | Slash -> Some "?"
  | _ -> None
;;

let keyboard_code_to_string (key : Keyboard_code.t) (key_with_prefix : With_prefix.t) =
  match key_with_prefix with
  | Key str -> String.lowercase str
  | Digit str | Arrow str -> str
  | Numpad str ->
    (match key with
     | NumpadMultiply -> "*"
     | NumpadSubtract -> "-"
     | NumpadAdd -> "+"
     | NumpadDecimal -> "."
     | NumpadEqual -> "="
     | NumpadDivide -> "/"
     | _ -> str)
  | Other str ->
    (match key with
     | Minus -> "-"
     | Equal -> "="
     | Escape -> "Esc"
     | Backspace -> "Bksp"
     | BracketLeft -> "["
     | BracketRight -> "]"
     | Semicolon -> ";"
     | Quote -> "'"
     | Backquote -> "`"
     | Backslash -> "\\"
     | Comma -> ","
     | Period -> "."
     | Slash -> "/"
     | _ -> str)
;;

let shift_string_and_keyboard_code_string (t : t) =
  let shift_str, shift_combo_str =
    match t.shift with
    | false -> "", None
    | true ->
      (match shift_combo_to_string t.key with
       | None -> "Shift+", None
       | Some str -> "", Some str)
  in
  let keyboard_code_str =
    match shift_combo_str with
    | Some str -> str
    | None ->
      let key_with_prefix =
        t.key |> Keyboard_code.sexp_of_t |> Sexp.to_string |> With_prefix.of_string
      in
      keyboard_code_to_string t.key key_with_prefix
  in
  shift_str, keyboard_code_str
;;

let to_string_hum t =
  let open Core in
  let ctrl_str = if t.ctrl then "Ctrl+" else "" in
  let alt_str = if t.alt then "Alt+" else "" in
  let meta_str = if t.meta then "Meta+" else "" in
  let shift_str, keyboard_code_str = shift_string_and_keyboard_code_string t in
  String.concat [ ctrl_str; alt_str; shift_str; meta_str; keyboard_code_str ]
;;

let%expect_test "" =
  let print ?ctrl ?alt ?shift ?meta key =
    printf !"%{to_string_hum}\n" (create' ?ctrl ?alt ?shift ?meta key)
  in
  print KeyA;
  [%expect {| a |}];
  print ~shift:() KeyA;
  [%expect {| Shift+a |}];
  print ~ctrl:() ~alt:() ~shift:() ~meta:() KeyA;
  [%expect {| Ctrl+Alt+Shift+Meta+a |}];
  print Digit1;
  [%expect {| 1 |}];
  print ~ctrl:() Digit1;
  [%expect {| Ctrl+1 |}];
  print Numpad1;
  [%expect {| 1 |}];
  print ~ctrl:() Numpad1;
  [%expect {| Ctrl+1 |}];
  print ~ctrl:() Numpad1;
  [%expect {| Ctrl+1 |}];
  print Comma;
  [%expect {| , |}];
  print ~shift:() Comma;
  [%expect {| < |}];
  print ~ctrl:() Comma;
  [%expect {| Ctrl+, |}];
  print ~ctrl:() ~shift:() Comma;
  [%expect {| Ctrl+< |}]
;;
