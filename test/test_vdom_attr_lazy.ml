open! Core
open! Import
open Virtual_dom

let show node =
  let t = node |> Node_helpers.unsafe_convert_exn in
  t |> Node_helpers.to_string_html |> print_endline
;;

let%expect_test "vdom.attr.lazy_" =
  let lazy_attr =
    Vdom.Attr.lazy_
      (lazy
        (print_endline "forced!";
         Vdom.Attr.class_ "my-class"))
  in
  (* not forced yet! *)
  [%expect {| |}];
  let element = Node.div ~attrs:[ lazy_attr ] [] in
  (* still not forced... *)
  [%expect {| |}];
  show element;
  (* now they're forced *)
  [%expect
    {|
    forced!
    <div class="my-class"> </div>
    |}]
;;

let%expect_test "multiple vdom.attr.lazy_" =
  let lazy_attr_1 =
    Vdom.Attr.lazy_
      (lazy
        (print_endline "forced 1!";
         Vdom.Attr.class_ "my-class-1"))
  in
  let lazy_attr_2 =
    Vdom.Attr.lazy_
      (lazy
        (print_endline "forced 2!";
         Vdom.Attr.class_ "my-class-2"))
  in
  (* not forced yet! *)
  [%expect {| |}];
  let element = Node.div ~attrs:[ lazy_attr_1; lazy_attr_2 ] [] in
  (* still not forced... *)
  [%expect {| |}];
  show element;
  (* now they're forced *)
  [%expect
    {|
    forced 1!
    forced 2!
    <div class="my-class-1 my-class-2"> </div>
    |}]
;;
