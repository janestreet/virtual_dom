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

              (* We need to include [key] and [code], so that there's some value for our
                 [of_event] function to read. But we want to use values that wouldn't show
                 up in real events, because we're testing just the [key_code] (and
                 [location]) property. *)
              val key = Js.string "not a valid key"
              val code = Js.string "not a valid code"
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

let via_empty_object : Dom_html.keyboardEvent Js.t = Obj.magic (object%js end)

let via_getmodifierstate : Dom_html.keyboardEvent Js.t =
  Obj.magic
    (object%js
       val getModifierState = Js.wrap_callback (fun _ -> Js._true)
    end)
;;

let via_bad_getmodifierstate : Dom_html.keyboardEvent Js.t =
  Obj.magic
    (object%js
       val getModifierState = Js.wrap_callback (fun _ -> Js.string "foo")
    end)
;;

let%expect_test "ctrl" =
  let via_property : Dom_html.keyboardEvent Js.t =
    Obj.magic
      (object%js
         val ctrlKey = Js.bool true
      end)
  in
  print_s [%message (Keyboard_event.ctrl via_property : bool)];
  [%expect {| ("Keyboard_event.ctrl via_property" true) |}];
  print_s [%message (Keyboard_event.ctrl via_empty_object : bool)];
  [%expect {| ("Keyboard_event.ctrl via_empty_object" false) |}];
  print_s [%message (Keyboard_event.ctrl via_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.ctrl via_getmodifierstate" true) |}];
  print_s [%message (Keyboard_event.ctrl via_bad_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.ctrl via_bad_getmodifierstate" false) |}]
;;

let%expect_test "alt" =
  let via_property : Dom_html.keyboardEvent Js.t =
    Obj.magic
      (object%js
         val altKey = Js.bool true
      end)
  in
  print_s [%message (Keyboard_event.alt via_property : bool)];
  [%expect {| ("Keyboard_event.alt via_property" true) |}];
  print_s [%message (Keyboard_event.alt via_empty_object : bool)];
  [%expect {| ("Keyboard_event.alt via_empty_object" false) |}];
  print_s [%message (Keyboard_event.alt via_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.alt via_getmodifierstate" true) |}];
  print_s [%message (Keyboard_event.alt via_bad_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.alt via_bad_getmodifierstate" false) |}]
;;

let%expect_test "shift" =
  let via_property : Dom_html.keyboardEvent Js.t =
    Obj.magic
      (object%js
         val shiftKey = Js.bool true
      end)
  in
  print_s [%message (Keyboard_event.shift via_property : bool)];
  [%expect {| ("Keyboard_event.shift via_property" true) |}];
  print_s [%message (Keyboard_event.shift via_empty_object : bool)];
  [%expect {| ("Keyboard_event.shift via_empty_object" false) |}];
  print_s [%message (Keyboard_event.shift via_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.shift via_getmodifierstate" true) |}];
  print_s [%message (Keyboard_event.shift via_bad_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.shift via_bad_getmodifierstate" false) |}]
;;

let%expect_test "meta" =
  let via_property : Dom_html.keyboardEvent Js.t =
    Obj.magic
      (object%js
         val metaKey = Js.bool true
      end)
  in
  print_s [%message (Keyboard_event.meta via_property : bool)];
  [%expect {| ("Keyboard_event.meta via_property" true) |}];
  print_s [%message (Keyboard_event.meta via_empty_object : bool)];
  [%expect {| ("Keyboard_event.meta via_empty_object" false) |}];
  print_s [%message (Keyboard_event.meta via_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.meta via_getmodifierstate" true) |}];
  print_s [%message (Keyboard_event.meta via_bad_getmodifierstate : bool)];
  [%expect {| ("Keyboard_event.meta via_bad_getmodifierstate" false) |}]
;;
