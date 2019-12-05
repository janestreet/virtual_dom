open! Core_kernel
open! Import

let show node =
  let t = node |> Node_helpers.unsafe_convert_exn in
  t |> [%sexp_of: Node_helpers.t] |> print_s;
  print_endline "----------------------";
  t |> Node_helpers.to_string_html |> print_endline
;;

let%expect_test "basic text" =
  show (Node.text "hello");
  [%expect {|
    (Text hello)
    ----------------------
    hello |}]
;;

let%expect_test "empty div" =
  show (Node.div [] []);
  [%expect
    {|
    (Element (tag_name div))
    ----------------------
    <div> </div> |}]
;;

let%expect_test "div with some text" =
  show (Node.div [] [ Node.text "hello world" ]);
  [%expect
    {|
    (Element (tag_name div) (children ((Text "hello world"))))
    ----------------------
    <div> hello world </div> |}]
;;

let%expect_test "empty div with key" =
  show (Node.div ~key:"keykey" [] []);
  [%expect
    {|
    (Element (tag_name div) (key keykey))
    ----------------------
    <div @key=keykey> </div> |}]
;;

let%expect_test "nested div with span" =
  show (Node.div [] [ Node.span [] [] ]);
  [%expect
    {|
    (Element (tag_name div) (children ((Element (tag_name span)))))
    ----------------------
    <div>
      <span> </span>
    </div> |}]
;;

let%expect_test "empty div with string attribute" =
  show (Node.div [ Attr.create "key" "value" ] []);
  [%expect
    {|
    (Element (tag_name div) (attributes ((key value))))
    ----------------------
    <div key="value"> </div> |}]
;;

let%expect_test "empty div with float attribute" =
  show (Node.div [ Attr.create_float "some_attr" 1.2345 ] []);
  [%expect
    {|
    (Element (tag_name div) (attributes ((some_attr 1.2345))))
    ----------------------
    <div some_attr="1.2345"> </div> |}]
;;

let%expect_test "widget" =
  let widget =
    Node.widget
      ~id:(Type_equal.Id.create ~name:"name_goes_here" [%sexp_of: opaque])
      ~init:(fun _ -> failwith "unreachable")
      ()
  in
  show widget;
  [%expect
    {|
    (Widget name_goes_here)
    ----------------------
    <widget id=name_goes_here /> |}]
;;

let%expect_test "empty div with callback" =
  show (Node.div [ Attr.on_click (Fn.const Event.Ignore) ] []);
  [%expect
    {|
    (Element (tag_name div) (handlers ((onclick <handler>))))
    ----------------------
    <div onclick={handler}> </div> |}]
;;

let%expect_test "empty div with class list" =
  show (Node.div [ Attr.classes [ "a"; "b"; "c" ] ] []);
  [%expect
    {|
    (Element (tag_name div) (attributes ((class "a b c"))))
    ----------------------
    <div class="a b c"> </div> |}]
;;

let%expect_test "empty div with id" =
  show (Node.div [ Attr.id "my-id" ] []);
  [%expect
    {|
    (Element (tag_name div) (attributes ((id my-id))))
    ----------------------
    <div id="my-id"> </div> |}]
;;

let trigger ?extra_fields (node : Node_helpers.t) ~event_name =
  match node with
  | Element { handlers; tag_name = _; attributes = _; key = _; children = _ } ->
    let handler = List.Assoc.find_exn handlers event_name ~equal:String.equal in
    Handler.trigger handler ?extra_fields
  | _ -> raise_s [%message "expected Element node" (node : Node_helpers.t)]
;;

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
  trigger node ~event_name:"onclick";
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
  Expect_test_helpers_kernel.require_does_raise [%here] (fun _ ->
    trigger node ~event_name:"onclick");
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
  trigger node ~extra_fields:[ "target", value_element ] ~event_name:"onchange";
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
  Expect_test_helpers_kernel.require_does_raise [%here] (fun _ ->
    trigger node ~extra_fields:[ "target", value_element ] ~event_name:"onchange");
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
  [%expect
    {| (node (Element (tag_name div) (attributes ((on_foo "not a function"))))) |}]
;;
