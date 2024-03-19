open! Core
open! Import

let show ?path_censoring_message ?hash_censoring_message ?filter_printed_attributes node =
  let t = node |> Node_helpers.unsafe_convert_exn in
  t |> [%sexp_of: Node_helpers.t] |> print_s;
  print_endline "----------------------";
  t
  |> Node_helpers.to_string_html
       ?path_censoring_message
       ?hash_censoring_message
       ?filter_printed_attributes
  |> print_endline
;;

let%expect_test "basic text" =
  show (Node.text "hello");
  [%expect {|
    (Text hello)
    ----------------------
    hello
    |}]
;;

let%expect_test "lazy text" =
  show (Node.lazy_ (lazy (Node.text "hello")));
  [%expect {|
    (Text hello)
    ----------------------
    hello
    |}]
;;

let%expect_test "lazy div" =
  show (Node.lazy_ (lazy (Node.div [])));
  [%expect
    {|
    (Element ((tag_name div)))
    ----------------------
    <div> </div>
    |}]
;;

let%expect_test "lazy lazy div" =
  show (Node.lazy_ (lazy (Node.lazy_ (lazy (Node.div [])))));
  [%expect
    {|
    (Element ((tag_name div)))
    ----------------------
    <div> </div>
    |}]
;;

let%expect_test "empty div" =
  show (Node.div []);
  [%expect
    {|
    (Element ((tag_name div)))
    ----------------------
    <div> </div>
    |}]
;;

let%expect_test "empty div" =
  show (Node.div []);
  [%expect
    {|
    (Element ((tag_name div)))
    ----------------------
    <div> </div>
    |}]
;;

let%expect_test "root level fragment converts into a div" =
  show (Node.fragment []);
  [%expect
    {|
    (Element ((tag_name div)))
    ----------------------
    <div> </div>
    |}]
;;

let%expect_test "non-root fragment flattens into parent" =
  show (Node.div [ Node.text "Hello, "; Node.fragment [ Node.text "world" ] ]);
  [%expect
    {|
    (Element ((tag_name div) (children ((Text "Hello, ") (Text world)))))
    ----------------------
    <div> Hello,  world </div>
    |}]
;;

let%expect_test "deeply nested fragments are completely flattened" =
  show
    (Node.div
       [ Node.text "Hello, "
       ; Node.fragment [ Node.fragment [ Node.fragment [ Node.text "world" ] ] ]
       ]);
  [%expect
    {|
    (Element ((tag_name div) (children ((Text "Hello, ") (Text world)))))
    ----------------------
    <div> Hello,  world </div>
    |}]
;;

let%expect_test "inner_html" =
  show
    (Node.inner_html
       ()
       ~tag:"div"
       ~attrs:[]
       ~this_html_is_sanitized_and_is_totally_safe_trust_me:"<b>hi</b>");
  [%expect
    {|
    (Element ((tag_name div) (children ((Text <b>hi</b>)))))
    ----------------------
    <div> <b>hi</b> </div>
    |}]
;;

let%expect_test "inner_html with custom renderer" =
  show
    (Node.inner_html
       ()
       ~tag:"div"
       ~attrs:[]
       ~override_vdom_for_testing:(lazy (Node.text "overridden!"))
       ~this_html_is_sanitized_and_is_totally_safe_trust_me:"<b>hi</b>");
  [%expect {|
    (Text overridden!)
    ----------------------
    overridden!
    |}]
;;

let%expect_test "div with some text" =
  show (Node.div [ Node.text "hello world" ]);
  [%expect
    {|
    (Element ((tag_name div) (children ((Text "hello world")))))
    ----------------------
    <div> hello world </div>
    |}]
;;

let%expect_test "empty div with key" =
  show (Node.div ~key:"keykey" []);
  [%expect
    {|
    (Element ((tag_name div) (key keykey)))
    ----------------------
    <div @key=keykey> </div>
    |}]
;;

let%expect_test "empty div with string property" =
  show (Node.div ~attrs:[ Attr.string_property "foo" "bar" ] []);
  [%expect
    {|
    (Element ((tag_name div) (string_properties ((foo bar)))))
    ----------------------
    <div #foo="bar"> </div>
    |}]
;;

let%expect_test "empty div with bool property" =
  show (Node.div ~attrs:[ Attr.bool_property "foo" true ] []);
  [%expect
    {|
    (Element ((tag_name div) (bool_properties ((foo true)))))
    ----------------------
    <div #foo="true"> </div>
    |}]
;;

let%expect_test "nested div with span" =
  show (Node.div [ Node.span [] ]);
  [%expect
    {|
    (Element ((tag_name div) (children ((Element ((tag_name span)))))))
    ----------------------
    <div>
      <span> </span>
    </div>
    |}]
;;

let%expect_test "empty div with string attribute" =
  show (Node.div ~attrs:[ Attr.create "key" "value" ] []);
  [%expect
    {|
    (Element ((tag_name div) (attributes ((key value)))))
    ----------------------
    <div key="value"> </div>
    |}]
;;

let%expect_test "empty div with float attribute" =
  show (Node.div ~attrs:[ Attr.create_float "some_attr" 1.2345 ] []);
  [%expect
    {|
    (Element ((tag_name div) (attributes ((some_attr 1.2345)))))
    ----------------------
    <div some_attr="1.2345"> </div>
    |}]
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
    (Element ((tag_name name_goes_here-widget)))
    ----------------------
    <name_goes_here-widget> </name_goes_here-widget>
    |}]
;;

let%expect_test "widget with a vdom_for_testing" =
  let widget =
    Node.widget
      ~id:(Type_equal.Id.create ~name:"name_goes_here" [%sexp_of: opaque])
      ~vdom_for_testing:
        (lazy
          (Node.div
             ~attrs:
               [ Attr.on_click (fun _ ->
                   print_endline "inside handler";
                   Effect.Ignore)
               ]
             []))
      ~init:(fun _ -> failwith "unreachable")
      ()
  in
  show widget;
  [%expect
    {|
    (Element ((tag_name div) (handlers ((onclick <handler>)))))
    ----------------------
    <div onclick> </div>
    |}]
;;

let%expect_test "widget inside of something else" =
  let widget =
    Node.div
      [ Node.widget
          ~id:(Type_equal.Id.create ~name:"name_goes_here" [%sexp_of: opaque])
          ~init:(fun _ -> failwith "unreachable")
          ()
      ]
  in
  show widget;
  [%expect
    {|
    (Element
     ((tag_name div) (children ((Element ((tag_name name_goes_here-widget)))))))
    ----------------------
    <div>
      <name_goes_here-widget> </name_goes_here-widget>
    </div>
    |}]
;;

let%expect_test "widget with info" =
  let widget =
    Node.widget
      ~vdom_for_testing:(lazy (Node.create "widget" [ Node.text "info name" ]))
      ~id:(Type_equal.Id.create ~name:"name_goes_here" [%sexp_of: opaque])
      ~init:(fun _ -> failwith "unreachable")
      ()
  in
  show widget;
  [%expect
    {|
    (Element ((tag_name widget) (children ((Text "info name")))))
    ----------------------
    <widget> info name </widget>
    |}]
;;

let%expect_test "opaque_widget" =
  let widget =
    Node.widget
      ~id:(Type_equal.Id.create ~name:"name_goes_here" [%sexp_of: opaque])
      ~init:(fun _ -> failwith "unreachable")
      ()
  in
  show widget;
  [%expect
    {|
    (Element ((tag_name name_goes_here-widget)))
    ----------------------
    <name_goes_here-widget> </name_goes_here-widget>
    |}]
;;

let%expect_test "empty div with callback" =
  show (Node.div ~attrs:[ Attr.on_click (Fn.const Effect.Ignore) ] []);
  [%expect
    {|
    (Element ((tag_name div) (handlers ((onclick <handler>)))))
    ----------------------
    <div onclick> </div>
    |}]
;;

let%expect_test "empty div with class list" =
  show (Node.div ~attrs:[ Attr.classes [ "a"; "b"; "c" ] ] []);
  [%expect
    {|
    (Element ((tag_name div) (attributes ((class "a b c")))))
    ----------------------
    <div class="a b c"> </div>
    |}]
;;

let%expect_test "empty div with id" =
  show (Node.div ~attrs:[ Attr.id "my-id" ] []);
  [%expect
    {|
    (Element ((tag_name div) (attributes ((id my-id)))))
    ----------------------
    <div id="my-id"> </div>
    |}]
;;

let%expect_test "later attributes override earlier ones" =
  show
    (Node.div
       ~attrs:[ Attr.(many_without_merge [ id "overwritten-id"; id "final-id" ]) ]
       []);
  [%expect
    {|
    ("WARNING: not combining attributes" (name id))
    (Element ((tag_name div) (attributes ((id final-id)))))
    ----------------------
    <div id="final-id"> </div>
    |}]
;;

let%expect_test "later properties override earlier ones" =
  show
    (Node.div
       ~attrs:
         [ Attr.(
             many_without_merge
               [ string_property "prop" "overwritten-prop"
               ; string_property "prop" "final-prop"
               ])
         ]
       []);
  [%expect
    {|
    ("WARNING: not combining properties" (name prop))
    (Element ((tag_name div) (string_properties ((prop final-prop)))))
    ----------------------
    <div #prop="final-prop"> </div>
    |}]
;;

let%expect_test "no merging without [many]" =
  show
    (Node.div
       ~attrs:[ Attr.(many_without_merge [ class_ "a"; class_ "b"; class_ "c" ]) ]
       []);
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b)))
    ("WARNING: not combining classes" (first (b)) (second (c)))
    (Element ((tag_name div) (attributes ((class c)))))
    ----------------------
    <div class="c"> </div>
    |}]
;;

let%expect_test "merging only within [many]" =
  show
    (Node.div
       ~attrs:
         [ Attr.(many_without_merge [ class_ "a"; class_ "b"; many [ class_ "c" ] ]) ]
       []);
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b)))
    ("WARNING: not combining classes" (first (b)) (second (c)))
    (Element ((tag_name div) (attributes ((class c)))))
    ----------------------
    <div class="c"> </div>
    |}]
;;

let%expect_test "empty div with [many] classes" =
  show
    (Node.div
       ~attrs:[ Attr.many [ Attr.class_ "a"; Attr.class_ "b"; Attr.class_ "c" ] ]
       []);
  [%expect
    {|
    (Element ((tag_name div) (attributes ((class "a b c")))))
    ----------------------
    <div class="a b c"> </div>
    |}]
;;

let%expect_test "empty div with [many] different attributes" =
  show
    (Node.div
       ~attrs:
         [ Attr.class_ "a"
         ; Attr.on_click (fun _ -> Ui_effect.Ignore)
         ; Attr.style Css_gen.bold
         ; Attr.class_ "b"
         ; Attr.style (Css_gen.z_index 42)
         ; Attr.id "my-id"
         ; Attr.class_ "c"
         ; Attr.Always_focus_hook.attr `Read_the_docs__this_hook_is_unpredictable
         ; Attr.style (Css_gen.display `Table)
         ; Attr.class_ "d"
         ; Attr.on_click (fun _ -> Ui_effect.Ignore)
         ; Attr.Always_focus_hook.attr `Read_the_docs__this_hook_is_unpredictable
         ]
       []);
  [%expect
    {|
    (Element
     ((tag_name div) (attributes ((id my-id) (class "a b c d")))
      (styles ((font-weight bold) (z-index 42) (display table)))
      (handlers ((onclick <handler>))) (hooks ((always-focus-hook ())))))
    ----------------------
    <div id="my-id"
         class="a b c d"
         always-focus-hook=()
         onclick
         style={
           font-weight: bold;
           z-index: 42;
           display: table;
         }> </div>
    |}]
;;

let%expect_test "empty div with tree of nested [many]" =
  show
    (Node.div
       ~attrs:
         [ Attr.many [ Attr.class_ "a"; Attr.style (Css_gen.text_align `Center) ]
         ; Attr.many [ Attr.many [ Attr.class_ "b"; Attr.class_ "c" ] ]
         ; Attr.many
             [ Attr.many
                 [ Attr.many
                     [ Attr.id "my-id"; Attr.style (Css_gen.box_sizing `Border_box) ]
                 ]
             ]
         ]
       []);
  [%expect
    {|
    (Element
     ((tag_name div) (attributes ((id my-id) (class "a b c")))
      (styles ((text-align center) (box-sizing border-box)))))
    ----------------------
    <div id="my-id" class="a b c" style={ text-align: center; box-sizing: border-box; }> </div>
    |}]
;;

let%expect_test "empty div with [many] different attributes" =
  let view =
    Node.div
      ~attrs:
        [ Attr.(
            many_without_merge
              [ class_ "a"
              ; on_click (fun _ -> Ui_effect.Ignore)
              ; on_blur (fun _ -> Ui_effect.Ignore)
              ; style Css_gen.bold
              ; id "my-id"
              ; checked
              ])
        ]
      []
  in
  show
    ~filter_printed_attributes:(fun ~key ~data:_ -> String.is_prefix ~prefix:"on" key)
    view;
  [%expect
    {|
    (Element
     ((tag_name div) (attributes ((id my-id) (checked "") (class a)))
      (styles ((font-weight bold)))
      (handlers ((onblur <handler>) (onclick <handler>)))))
    ----------------------
    <div onblur onclick> </div>
    |}];
  show
    ~filter_printed_attributes:(fun ~key ~data:_ ->
      not (String.is_prefix ~prefix:"on" key))
    view;
  [%expect
    {|
    (Element
     ((tag_name div) (attributes ((id my-id) (checked "") (class a)))
      (styles ((font-weight bold)))
      (handlers ((onblur <handler>) (onclick <handler>)))))
    ----------------------
    <div id="my-id" checked="" class="a" style={ font-weight: bold; }> </div>
    |}]
;;

let%expect_test "[many_without_merge] inside merge" =
  let view =
    Node.div
      ~attrs:
        [ Attr.(class_ "a" @ many_without_merge [ class_ "b"; class_ "c" ] @ class_ "d") ]
      []
  in
  show view;
  [%expect
    {|
    ("WARNING: not combining classes" (first (b)) (second (c)))
    (Element ((tag_name div) (attributes ((class "a c d")))))
    ----------------------
    <div class="a c d"> </div>
    |}];
  let view =
    Node.div
      ~attrs:
        [ Attr.(
            many_without_merge
              (Multi.merge_classes_and_styles
                 [ class_ "a"; many_without_merge [ class_ "b"; class_ "c" ]; class_ "d" ]))
        ]
      []
  in
  show view;
  [%expect
    {|
    ("WARNING: not combining classes" (first (b)) (second (c)))
    (Element ((tag_name div) (attributes ((class "a c d")))))
    ----------------------
    <div class="a c d"> </div>
    |}];
  let view =
    Node.div
      ~attrs:
        [ Attr.(
            class_ "a"
            @ many_without_merge
                [ class_ "b"
                ; class_ "ba" @ class_ "bb"
                ; class_ "c" @ many_without_merge [ class_ "baa"; class_ "bab" ]
                ]
            @ class_ "d")
        ]
      []
  in
  show view;
  [%expect
    {|
    ("WARNING: not combining classes" (first (b)) (second (ba bb)))
    ("WARNING: not combining classes" (first (baa)) (second (bab)))
    ("WARNING: not combining classes" (first (ba bb)) (second (bab c)))
    (Element ((tag_name div) (attributes ((class "a bab c d")))))
    ----------------------
    <div class="a bab c d"> </div>
    |}]
;;

let%expect_test "combining hooks" =
  let module H =
    Virtual_dom.Vdom.Attr.Hooks.Make (struct
      module State = Unit

      module Input = struct
        type t = string list [@@deriving sexp_of]

        let combine a b =
          print_endline "in combine";
          a @ b
        ;;
      end

      let init _ _ = ()
      let on_mount = `Do_nothing
      let update ~old_input:_ ~new_input:_ () _ = ()
      let destroy _ () _ = ()
    end)
  in
  let make s = Attr.create_hook "my-hook" (H.create [ s ]) in
  show (Node.div ~attrs:[ Attr.class_ "x"; make "hello"; make "world" ] []);
  [%expect
    {|
    in combine
    (Element
     ((tag_name div) (attributes ((class x))) (hooks ((my-hook (hello world))))))
    ----------------------
    <div class="x" my-hook=(hello world)> </div>
    |}]
;;

let%expect_test "hash censoring" =
  let node = Node.div ~attrs:[ Attr.class_ "foo_hash_abc123" ] [] in
  show node;
  let before = [%expect.output] in
  show ~hash_censoring_message:"#" node;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  [%expect
    {|
    -1,3 +1,3
      (Element ((tag_name div) (attributes ((class foo_hash_abc123)))))
      ----------------------
    -|<div class="foo_hash_replaced_in_test"> </div>
    +|<div class="foo#"> </div>
    |}]
;;

let%expect_test "path censoring" =
  let node = Node.div ~attrs:[ Attr.class_ "bonsai_path_abcdefg" ] [] in
  show node;
  let before = [%expect.output] in
  show ~path_censoring_message:"<path>" node;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  [%expect
    {|
    -1,3 +1,3
      (Element ((tag_name div) (attributes ((class bonsai_path_abcdefg)))))
      ----------------------
    -|<div class="bonsai_path_replaced_in_test"> </div>
    +|<div class="<path>"> </div>
    |}]
;;
