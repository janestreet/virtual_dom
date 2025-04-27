open! Core
open! Import

let%test "empty patch succeeds" =
  let previous = Node.div [] in
  let current = Node.div [] in
  let patch = Node.Patch.create ~previous ~current in
  Node.Patch.is_empty patch
;;

let%test "non-empty patch fails" =
  let previous = Node.div [ Node.text "Hello" ] in
  let current = Node.div [ Node.text "World" ] in
  let patch = Node.Patch.create ~previous ~current in
  not (Node.Patch.is_empty patch)
;;

let%expect_test {|regression: elements with the same event handler produces a patch |} =
  let attrs = [ Attr.on_click (fun _ -> Effect.Ignore) ] in
  let previous = Node.div ~attrs [] in
  let current = Node.div ~attrs [] in
  let patch = Node.Patch.create ~previous ~current in
  print_s [%message "" ~patch_is_empty:(Node.Patch.is_empty patch : bool)];
  [%expect {| (patch_is_empty true) |}]
;;
