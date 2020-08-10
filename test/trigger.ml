open! Core_kernel
open! Import

let%expect_test "empty div with triggered callback" =
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.div
         [ Attr.on_click (fun _ ->
             print_endline "inside handler";
             Event.Ignore)
         ]
         [])
  in
  Node_helpers.trigger node ~event_name:"onclick";
  [%expect {| inside handler |}]
;;

let%expect_test "empty div with triggered callback (failing)" =
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.div
         [ Attr.on_click (fun e ->
             print_s [%message (e##.screenX : int)];
             Event.Ignore)
         ]
         [])
  in
  Expect_test_helpers_core.require_does_raise [%here] (fun _ ->
    Node_helpers.trigger node ~event_name:"onclick");
  [%expect
    {|
    ((missing_field_path screenX)
     "The field was read on a fake DOM event.  You probably called [Handler.trigger] in a test, and the handler accessed a field of the event that was not provided to [~extra_fields].") |}]
;;

let%expect_test "empty input with on_change (success!)" =
  let value_element =
    Js_of_ocaml.Js.Unsafe.obj
      (* Due to a crazy hack in the Js_of_ocaml CoerceTo module, Node.js is accidentally
         pretending to be IE6, and by sticking "tagName" on the object, we can impersonate
         the <input> element, and bypass the coersion check! *)
      [| ("tagName", Js_of_ocaml.Js.(string "input" |> Unsafe.inject))
       ; ("value", Js_of_ocaml.Js.(string "hello" |> Unsafe.inject))
      |]
  in
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.input
         [ Attr.on_change (fun _e s ->
             print_endline s;
             Event.Ignore)
         ]
         [])
  in
  Node_helpers.trigger
    node
    ~extra_fields:[ "target", value_element ]
    ~event_name:"onchange";
  [%expect {| hello |}]
;;

let%expect_test "empty input with on_change (failing: forgot tagName!)" =
  let value_element =
    Js_of_ocaml.Js.Unsafe.obj
      [| ("value", Js_of_ocaml.Js.(string "hello" |> Unsafe.inject)) |]
  in
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.input
         [ Attr.on_change (fun _e s ->
             print_endline s;
             Event.Ignore)
         ]
         [])
  in
  Expect_test_helpers_core.require_does_raise [%here] (fun _ ->
    Node_helpers.trigger
      node
      ~extra_fields:[ "target", value_element ]
      ~event_name:"onchange");
  (* Check it out!  The error message knew about the whole path that the js_of_ocaml runtime
     took in order to find tagName.  The power of Proxies! *)
  [%expect
    {|
    ((missing_field_path target.tagName)
     "The field was read on a fake DOM event.  You probably called [Handler.trigger] in a test, and the handler accessed a field of the event that was not provided to [~extra_fields].") |}]
;;

(* [Node_helpers.unsafe_convert_exn] only treats an attribute as a handler if it is named
   like a handler and it is a function.  *)
let%expect_test "fake event handler" =
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.div [ Attr.create "on_foo" "not a function" ] [])
  in
  print_s [%message (node : Node_helpers.t)];
  [%expect {| (node (Element (tag_name div) (attributes ((on_foo "not a function"))))) |}]
;;
