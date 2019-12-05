open! Core_kernel
open! Import

let show selector node =
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select ~selector
  |> [%sexp_of: Node_helpers.t list]
  |> print_s
;;

let%expect_test "select empty div with * selector" =
  show "*" (Node.div [] []);
  [%expect {| ((Element (tag_name div))) |}]
;;

let%expect_test "wrong id selector" =
  show "#wrong" (Node.div [ Attr.id "correct" ] []);
  [%expect {| () |}]
;;

let%expect_test "correct id selector" =
  show "#correct" (Node.div [ Attr.id "correct" ] []);
  [%expect {| ((Element (tag_name div) (attributes ((id correct))))) |}]
;;

let%expect_test "multiple classes selector" =
  show ".a.b" (Node.div [ Attr.classes [ "a"; "b" ] ] []);
  [%expect {| ((Element (tag_name div) (attributes ((class "a b"))))) |}]
;;

let%expect_test "select finds multiple items" =
  show
    ".a"
    (Node.div
       []
       [ Node.span [ Attr.class_ "a"; Attr.id "1" ] []
       ; Node.span [ Attr.class_ "a"; Attr.id "2" ] []
       ]);
  [%expect
    {|
    ((Element (tag_name span) (attributes ((class a) (id 1))))
     (Element (tag_name span) (attributes ((class a) (id 2))))) |}]
;;

let%expect_test "select nth-child" =
  let t =
    Node.div
      []
      [ Node.span [ Attr.class_ "a"; Attr.id "1" ] []
      ; Node.span [ Attr.class_ "a"; Attr.id "2" ] []
      ]
  in
  show "span:nth-child(1)" t;
  [%expect {|
    ((Element (tag_name span) (attributes ((class a) (id 1))))) |}];
  show "span:nth-child(2)" t;
  [%expect {|
    ((Element (tag_name span) (attributes ((class a) (id 2))))) |}]
;;

let%expect_test "select on node-name" =
  let t =
    Node.div
      []
      [ Node.span [ Attr.class_ "a"; Attr.id "1" ] []
      ; Node.span [ Attr.class_ "a"; Attr.id "2" ] []
      ]
  in
  show "span" t;
  [%expect
    {|
    ((Element (tag_name span) (attributes ((class a) (id 1))))
     (Element (tag_name span) (attributes ((class a) (id 2))))) |}]
;;
