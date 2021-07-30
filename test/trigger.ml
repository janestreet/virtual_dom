open! Core
open! Import

let%expect_test "empty div with triggered callback" =
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.div
         ~attr:
           (Attr.on_click (fun _ ->
              print_endline "inside handler";
              Effect.Ignore))
         [])
  in
  Node_helpers.trigger node ~event_name:"onclick";
  [%expect {| inside handler |}]
;;

let%expect_test "empty div with triggered callback (failing)" =
  let node =
    Node_helpers.unsafe_convert_exn
      (Node.div
         ~attr:
           (Attr.on_click (fun e ->
              print_s [%message (e##.screenX : int)];
              Effect.Ignore))
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
         ~attr:
           (Attr.on_change (fun _e s ->
              print_endline s;
              Effect.Ignore))
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
         ~attr:
           (Attr.on_change (fun _e s ->
              print_endline s;
              Effect.Ignore))
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
      (Node.div ~attr:(Attr.create "on_foo" "not a function") [])
  in
  print_s [%message (node : Node_helpers.t)];
  [%expect
    {| (node (Element ((tag_name div) (attributes ((on_foo "not a function")))))) |}]
;;

module Print_int_event = Ui_effect.Define (struct
    module Action = Int

    let handle = printf "%d "
  end)

module Print_string_event = Ui_effect.Define (struct
    module Action = String

    let handle = printf "%s "
  end)

module H = Attr.Hooks.Make (struct
    module Input = struct
      type t = int -> unit Ui_effect.t [@@deriving sexp]

      let combine left right i =
        (* adding 10 to [i] is silly, but it'll be obvious in the tests *)
        let i = i + 10 in
        Effect.sequence_as_sibling (left i) ~unless_stopped:(fun () -> right i)
      ;;
    end

    module State = Unit

    let init _input _element = ()
    let on_mount _input _state _element = ()
    let update ~old_input:_ ~new_input:_ _state _element = ()
    let destroy _input _state _element = ()
  end)

module String_h = Attr.Hooks.Make (struct
    module Input = struct
      type t = string -> unit Ui_effect.t [@@deriving sexp]

      let combine left right i =
        let i = i ^ "_combine" in
        Effect.sequence_as_sibling (left i) ~unless_stopped:(fun () -> right i)
      ;;
    end

    module State = Unit

    let init _input _element = ()
    let on_mount _input _state _element = ()
    let update ~old_input:_ ~new_input:_ _state _element = ()
    let destroy _input _state _element = ()
  end)

let%expect_test "fake event handler for hook" =
  let node =
    Node.input ~attr:(Attr.create_hook "unique-name" (H.create Print_int_event.inject)) []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.trigger_hook ~type_id:H.For_testing.type_id ~name:"unique-name" ~arg:5;
  [%expect {| 5 |}]
;;

let%expect_test "not merged " =
  let node =
    Node.input
      ~attr:
        Attr.(
          many_without_merge
            [ create_hook "not-so-unique-name" (H.create Print_int_event.inject)
            ; create_hook "not-so-unique-name" (H.create Print_int_event.inject)
            ])
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.trigger_hook
       ~type_id:H.For_testing.type_id
       ~name:"not-so-unique-name"
       ~arg:5;
  [%expect {|
    ("WARNING: not combining hooks" (name not-so-unique-name))
    5 |}]
;;

let%expect_test "bad merge" =
  let node =
    Node.input
      ~attr:
        (Attr.many
           [ Attr.create_hook "not-so-unique-name" (H.create Print_int_event.inject)
           ; Attr.create_hook "not-so-unique-name" (H.create Print_int_event.inject)
           ])
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.trigger_hook
       ~type_id:H.For_testing.type_id
       ~name:"not-so-unique-name"
       ~arg:5;
  [%expect {| 15 15 |}]
;;

let%expect_test "not merged " =
  let node =
    Node.input
      ~attr:
        (Attr.many
           [ Attr.create_hook "not-so-unique-name" (H.create Print_int_event.inject)
           ; Attr.create_hook
               "not-so-unique-name"
               (String_h.create Print_string_event.inject)
           ])
      []
  in
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    node
    |> Node_helpers.unsafe_convert_exn
    |> Node_helpers.trigger_hook
         ~type_id:H.For_testing.type_id
         ~name:"not-so-unique-name"
         ~arg:5);
  [%expect
    {|
    "hooks do not have the same type, so they cannot be combined; taking the second of the two"
    (Failure
     "get_hook_value: a hook for not-so-unique-name was found, but the type-ids were not the same; are you using the same type-id that you got from the For_testing module from your hook creator?") |}]
;;

let%expect_test "fake event handler for on_click" =
  let node =
    Node.div
      ~attr:
        Attr.(many_without_merge [ id "x"; on_click (fun _ -> Print_int_event.inject 5) ])
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select_first_exn ~selector:"#x"
  |> Node_helpers.User_actions.click_on;
  [%expect {| 5 |}]
;;

let%expect_test "not merged" =
  let node =
    Node.div
      ~attr:
        Attr.(
          many_without_merge
            [ id "x"
            ; on_click (fun _ -> Print_int_event.inject 5)
            ; on_click (fun _ -> Print_int_event.inject 6)
            ])
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select_first_exn ~selector:"#x"
  |> Node_helpers.User_actions.click_on;
  [%expect {|
    ("WARNING: not combining handlers" (name click))
    6 |}]
;;

let%expect_test "merged" =
  let node =
    Node.div
      ~attr:
        Attr.(
          many_without_merge
            [ id "x"
            ; many
                [ on_click (fun _ -> Print_int_event.inject 5)
                ; on_click (fun _ -> Print_int_event.inject 6)
                ]
            ])
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select_first_exn ~selector:"#x"
  |> Node_helpers.User_actions.click_on;
  [%expect {| 5 6 |}]
;;

let%expect_test "add class in the middle of a [many]" =
  let node =
    Node.div
      ~attr:
        (Attr.many
           ([ Attr.id "x"
            ; Attr.class_ "abc"
            ; Attr.on_click (fun _ -> Print_int_event.inject 1)
            ]
            @ Attr.Multi.add_class
                [ Attr.class_ "def"
                ; Attr.on_click (fun _ -> Print_int_event.inject 2)
                ; Attr.on_click (fun _ -> Print_int_event.inject 3)
                ]
                "ghi"
            @ [ Attr.class_ "jkl"; Attr.on_click (fun _ -> Print_int_event.inject 4) ]))
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select_first_exn ~selector:"#x"
  |> Node_helpers.User_actions.click_on;
  [%expect {|
    ("WARNING: not combining handlers" (name click))
    1 3 4 |}]
;;

let%expect_test "bug with attributes and properties with the same name" =
  (* There is currently a bug with selecting elements that have attributes and properties
     with the same name. This is fixed in a subfeature. *)
  let node =
    Node.input
      ~attr:
        Attr.(
          many_without_merge
            [ checked
            ; bool_property "checked" true
            ; on_click (fun _ev -> Print_int_event.inject 1)
            ; id "x"
            ])
      []
  in
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select_first_exn ~selector:"#x"
  |> Node_helpers.User_actions.click_on;
  [%expect {| 1 |}]
;;
