open! Core
open Import
open! Virtual_dom.Vdom

(* This test doesn't do anything apart from testing that we can actually run tests for
   virtual_dom *)
let%test _ =
  let previous = Node.div [] in
  let current = Node.div [] in
  let _patch = Node.Patch.create ~previous ~current in
  true
;;

let show node =
  node |> Node_helpers.unsafe_convert_exn |> [%sexp_of: Node_helpers.t] |> print_s
;;

let%expect_test "Node.of_opt" =
  show (Some (Node.div []) |> Node.of_opt);
  [%expect {| (Element ((tag_name div))) |}];
  show (None |> Node.of_opt);
  [%expect {| (Element ((tag_name Vdom.Node.none-widget))) |}]
;;

let%expect_test "Node.dfn" =
  show (Node.dfn [ Node.text "hi" ]);
  [%expect {| (Element ((tag_name dfn) (children ((Text hi))))) |}]
;;
