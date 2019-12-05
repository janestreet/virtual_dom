open! Core_kernel
open! Virtual_dom.Vdom

(* This test doesn't do anything apart from testing that we can actually run tests for
   virtual_dom *)
let%test _ =
  let previous = Node.div [] [] in
  let current = Node.div [] [] in
  let _patch = Node.Patch.create ~previous ~current in
  true
;;

