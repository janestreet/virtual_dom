open! Core
open Vdom_keyboard
module Keyboard_code = Keystroke.Keyboard_code
open Js_of_ocaml

let%expect_test _ =
  let (_ : unit list) =
    let%map.List keyboard_code = Keyboard_code.all in
    let key_code = Keyboard_code.to_key_code keyboard_code in
    let location = Keyboard_code.to_location keyboard_code in
    let roundtrip =
      Keyboard_code.of_event
        (Js.Unsafe.coerce
           (object%js
              val keyCode = key_code
              val location = location
           end))
    in
    match keyboard_code with
    | NumpadEqual
    | NumLock
    | VolumeMute
    | VolumeDown
    | VolumeUp
    | MediaTrackPrevious
    | MediaTrackNext
    | MediaPlayPause
    | MediaStop
    | BrowserSearch
    | BrowserHome
    | BrowserFavorites
    | BrowserRefresh
    | BrowserStop
    | BrowserForward
    | BrowserBack
    | OSLeft
    | OSRight
    | IntlBackslash
    | IntlYen -> ()
    | _ when Keyboard_code.equal keyboard_code roundtrip -> ()
    | _ ->
      raise_s
        [%message
          (key_code : int) (keyboard_code : Keyboard_code.t) (roundtrip : Keyboard_code.t)]
  in
  ()
;;
