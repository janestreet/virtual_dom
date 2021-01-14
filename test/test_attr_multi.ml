open! Core_kernel
open! Import

let show node =
  let t = node |> Node_helpers.unsafe_convert_exn in
  t |> [%sexp_of: Node_helpers.t] |> print_s;
  print_endline "----------------------";
  t |> Node_helpers.to_string_html |> print_endline
;;

let%expect_test "combining classes and styles" =
  show
    (Node.div
       (Attr.Multi.merge_classes_and_styles
          [ Attr.class_ "abc"
          ; Attr.class_ "123"
          ; Attr.style (Css_gen.margin ~top:(`Px 10) ())
          ; Attr.style (Css_gen.margin ~bottom:(`Px 20) ())
          ])
       []);
  [%expect
    {|
    (Element
     ((tag_name div) (attributes ((class "123 abc")))
      (styles ((margin-top 10px) (margin-bottom 20px)))))
    ----------------------
    <div class="123 abc" style={ margin-top: 10px; margin-bottom: 20px; }> </div> |}]
;;

let%expect_test "map node styles" =
  show
    (Node.div
       (Attr.Multi.map_style
          ~f:(fun styles -> Css_gen.combine styles (Css_gen.margin ~left:(`Px 30) ()))
          [ Attr.style (Css_gen.margin ~top:(`Px 10) ())
          ; Attr.style (Css_gen.margin ~bottom:(`Px 20) ())
          ])
       []);
  [%expect
    {|
    (Element
     ((tag_name div)
      (styles ((margin-top 10px) (margin-bottom 20px) (margin-left 30px)))))
    ----------------------
    <div style={ margin-top: 10px; margin-bottom: 20px; margin-left: 30px; }> </div> |}]
;;

let%expect_test "add class" =
  show
    (Node.div (Attr.Multi.add_class [ Attr.autofocus true; Attr.class_ "def" ] "abc") []);
  [%expect
    {|
    (Element ((tag_name div) (attributes ((autofocus true) (class "abc def")))))
    ----------------------
    <div autofocus="true" class="abc def"> </div> |}]
;;
