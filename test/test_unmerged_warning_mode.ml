open! Core
open! Import

let show node =
  let t = node |> Node_helpers.unsafe_convert_exn in
  t |> Node_helpers.to_string_html |> print_endline
;;

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = Expect_test_helpers_core.hide_positions_in_string (sanitize s)

  let run f =
    Virtual_dom.Vdom.Attr.Expert.set_explicitly_print_locations true;
    f ();
    Virtual_dom.Vdom.Attr.Expert.set_explicitly_print_locations false
  ;;
end

let%expect_test "stop warning after message quota" =
  Attr.Unmerged_warning_mode.For_testing.reset_warning_count ();
  Attr.Unmerged_warning_mode.current := Stop_after_quota 1;
  show
    (Node.div
       ~attrs:
         [ Attr.(many_without_merge [ class_ "a"; class_ "b"; many [ class_ "c" ] ]) ]
       []);
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    ("WARNING: reached warning message quota; no more messages will be printed"
     (quota 1))
    <div class="c"> </div>
    |}]
;;

let%expect_test "No_warnings prints no warnings" =
  Attr.Unmerged_warning_mode.For_testing.reset_warning_count ();
  Attr.Unmerged_warning_mode.current := No_warnings;
  show
    (Node.div
       ~attrs:
         [ Attr.(many_without_merge [ class_ "a"; class_ "b"; many [ class_ "c" ] ]) ]
       []);
  [%expect {| <div class="c"> </div> |}]
;;

let%expect_test "All_warnings prints warnings" =
  Attr.Unmerged_warning_mode.For_testing.reset_warning_count ();
  Attr.Unmerged_warning_mode.current := All_warnings;
  show
    (Node.div
       ~attrs:
         [ Attr.(many_without_merge [ class_ "a"; class_ "b"; many [ class_ "c" ] ]) ]
       []);
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    ("WARNING: not combining classes" (first (b)) (second (c))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    <div class="c"> </div>
    |}]
;;

let%expect_test "mode transitions are predictable" =
  let node () =
    Node.div
      ~attrs:[ Attr.(many_without_merge [ class_ "a"; class_ "b"; many [ class_ "c" ] ]) ]
      []
  in
  Attr.Unmerged_warning_mode.For_testing.reset_warning_count ();
  Attr.Unmerged_warning_mode.current := No_warnings;
  show (node ());
  [%expect {| <div class="c"> </div> |}];
  Attr.Unmerged_warning_mode.current := Stop_after_quota 3;
  show (node ());
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    ("WARNING: reached warning message quota; no more messages will be printed"
     (quota 3))
    <div class="c"> </div>
    |}];
  Attr.Unmerged_warning_mode.current := Stop_after_quota 5;
  show (node ());
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    ("WARNING: reached warning message quota; no more messages will be printed"
     (quota 5))
    <div class="c"> </div>
    |}];
  Attr.Unmerged_warning_mode.current := All_warnings;
  show (node ());
  [%expect
    {|
    ("WARNING: not combining classes" (first (a)) (second (b))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    ("WARNING: not combining classes" (first (b)) (second (c))
     (here lib/virtual_dom/test/test_unmerged_warning_mode.ml:LINE:COL))
    <div class="c"> </div>
    |}]
;;
