open! Core_kernel
open! Import

let%test "empty patch succeeds" =
  let previous = Node.div [] [] in
  let current = Node.div [] [] in
  let patch = Node.Patch.create ~previous ~current in
  Node.Patch.is_empty patch
;;

let%test "non-empty patch fails" =
  let previous = Node.div [] [ Node.text "Hello" ] in
  let current = Node.div [] [ Node.text "World" ] in
  let patch = Node.Patch.create ~previous ~current in
  not (Node.Patch.is_empty patch)
;;
