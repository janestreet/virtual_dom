open! Core
open! Import

let show selector node =
  node
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.select ~selector
  |> [%sexp_of: Node_helpers.t list]
  |> print_s
;;

let%expect_test "select empty div with * selector" =
  show "*" (Node.div []);
  [%expect {| ((Element ((tag_name div)))) |}]
;;

let%expect_test "wrong id selector" =
  show "#wrong" (Node.div ~attrs:[ Attr.id "correct" ] []);
  [%expect {| () |}]
;;

let%expect_test "correct id selector" =
  show "#correct" (Node.div ~attrs:[ Attr.id "correct" ] []);
  [%expect {| ((Element ((tag_name div) (attributes ((id correct)))))) |}]
;;

let%expect_test "multiple classes selector" =
  show ".a.b" (Node.div ~attrs:[ Attr.classes [ "a"; "b" ] ] []);
  [%expect {| ((Element ((tag_name div) (attributes ((class "a b")))))) |}]
;;

let%expect_test "select finds multiple items" =
  show
    ".a"
    (Node.div
       [ Node.span ~attrs:[ Attr.(many_without_merge [ class_ "a"; id "1" ]) ] []
       ; Node.span ~attrs:[ Attr.(many_without_merge [ class_ "a"; id "2" ]) ] []
       ]);
  [%expect
    {|
    ((Element ((tag_name span) (attributes ((id 1) (class a)))))
     (Element ((tag_name span) (attributes ((id 2) (class a))))))
    |}]
;;

let%expect_test "select nth-child" =
  let t =
    Node.div
      [ Node.span ~attrs:[ Attr.(many_without_merge [ class_ "a"; id "1" ]) ] []
      ; Node.span ~attrs:[ Attr.(many_without_merge [ class_ "a"; id "2" ]) ] []
      ]
  in
  show "span:nth-child(1)" t;
  [%expect {| ((Element ((tag_name span) (attributes ((id 1) (class a)))))) |}];
  show "span:nth-child(2)" t;
  [%expect {| ((Element ((tag_name span) (attributes ((id 2) (class a)))))) |}]
;;

let%expect_test "select on node-name" =
  let t =
    Node.div
      [ Node.span ~attrs:[ Attr.(many_without_merge [ class_ "a"; id "1" ]) ] []
      ; Node.span ~attrs:[ Attr.(many_without_merge [ class_ "a"; id "2" ]) ] []
      ]
  in
  show "span" t;
  [%expect
    {|
    ((Element ((tag_name span) (attributes ((id 1) (class a)))))
     (Element ((tag_name span) (attributes ((id 2) (class a))))))
    |}]
;;

module Person = struct
  type t =
    { age : int
    ; name : string
    }
  [@@deriving sexp_of]

  let combine _ right = right
end

module H = Attr.Hooks.Make (struct
    module State = Unit
    module Input = Person

    let init _input _element = ()
    let on_mount = `Do_nothing
    let update ~old_input:_ ~new_input:_ _state _element = ()
    let destroy _input _state _element = ()
  end)

let%expect_test "print element with a hook" =
  show
    "*"
    (Node.div
       ~attrs:
         [ Attr.create_hook "unique-name" (H.create { Person.age = 20; name = "person" })
         ]
       []);
  [%expect
    {| ((Element ((tag_name div) (hooks ((unique-name ((age 20) (name person)))))))) |}]
;;

let%expect_test "get value out of a hook in a test" =
  Node.div
    ~attrs:
      [ Attr.create_hook "unique-name" (H.create { Person.age = 20; name = "person" }) ]
    []
  |> Node_helpers.unsafe_convert_exn
  |> Node_helpers.get_hook_value ~type_id:H.For_testing.type_id ~name:"unique-name"
  |> Person.sexp_of_t
  |> print_s;
  [%expect {| ((age 20) (name person)) |}]
;;

let%expect_test "try to find hook that doesn't exist" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    let (_ : _) =
      Node.div []
      |> Node_helpers.unsafe_convert_exn
      |> Node_helpers.get_hook_value ~type_id:H.For_testing.type_id ~name:"unique-name"
    in
    ());
  [%expect {| (Failure "get_hook_value: no hook found with name unique-name") |}]
;;

let%expect_test "try to find hook on a text node" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    let (_ : _) =
      Node.text ""
      |> Node_helpers.unsafe_convert_exn
      |> Node_helpers.get_hook_value ~type_id:H.For_testing.type_id ~name:"unique-name"
    in
    ());
  [%expect {| (Failure "get_hook_value: expected Element, found Text") |}]
;;

let%expect_test "try to find hook with a bad type_id" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    let (_ : _) =
      Node.div
        ~attrs:
          [ Attr.create_hook "unique-name" (H.create { Person.age = 20; name = "person" })
          ]
        []
      |> Node_helpers.unsafe_convert_exn
      |> Node_helpers.get_hook_value
           ~type_id:(Type_equal.Id.create ~name:"" sexp_of_opaque)
           ~name:"unique-name"
    in
    ());
  [%expect
    {|
    (Failure
     "get_hook_value: a hook for unique-name was found, but the type-ids were not the same; are you using the same type-id that you got from the For_testing module from your hook creator?")
    |}]
;;
