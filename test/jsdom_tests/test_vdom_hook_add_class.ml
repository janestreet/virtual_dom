open! Core
include Virtual_dom.Vdom
include Virtual_dom_test_helpers
open Js_of_ocaml
open Html_syntax

let show node =
  let t = node |> Node_helpers.unsafe_convert_exn in
  t |> [%sexp_of: Node_helpers.t] |> print_s;
  print_endline "";
  print_endline "---------- Vdom ----------";
  print_endline "";
  t |> Node_helpers.to_string_html |> print_endline;
  print_endline "";
  print_endline "---------- Actual DOM ----------";
  print_endline "";
  let node = Node.to_dom node in
  Jsdom.Expert_for_custom_test_handles.Dom_serialization.dom_to_string ~node ()
  |> Jsdom.Expert_for_custom_test_handles.Dom_serialization.pretty_print_html
;;

module Add_class_on_init_hook = Attr.Hooks.Make (struct
    module State = struct
      type t = unit
    end

    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine () () = ()
    end

    let init () (elem : Dom_html.element Js.t) =
      let () = (Js.Unsafe.coerce elem)##.classList##add (Js.string "hello") in
      ()
    ;;

    let update ~old_input:() ~new_input:() () _elem = ()
    let destroy () () _elem = ()
    let on_mount = `Do_nothing
  end)

let create_hook () =
  Add_class_on_init_hook.create () |> Attr.create_hook "add_class_on_init_hook"
;;

let%expect_test "hook class shows up" =
  let node = {%html|<div %{create_hook()}></div>|} in
  show node;
  {%expect|
    (Element ((tag_name div) (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="hello"> </div>
      </body>
    </html>
    |}
;;

let%expect_test "hook class is erased when class attrs are applied regardless of the \
                 order"
  =
  let node = {%html|<div %{Attr.class_ "uh-oh"} %{create_hook()}></div>|} in
  show node;
  {%expect|
    (Element
     ((tag_name div) (attributes ((class uh-oh)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="uh-oh" add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="uh-oh"> </div>
      </body>
    </html>
    |};
  (* Standard class attr *)
  let node = {%html|<div %{create_hook()} %{Attr.class_ "uh-oh"}></div>|} in
  show node;
  {%expect|
    (Element
     ((tag_name div) (attributes ((class uh-oh)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="uh-oh" add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="uh-oh"> </div>
      </body>
    </html>
    |};
  let node = {%html|<div %{create_hook()} %{Attr.class_ "uh-oh"}></div>|} in
  show node;
  [%expect
    {|
    (Element
     ((tag_name div) (attributes ((class uh-oh)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="uh-oh" add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="uh-oh"> </div>
      </body>
    </html>
    |}];
  (* PPX_CSS *)
  let node = {%html|<div style="display: flex" %{create_hook()}></div>|} in
  show node;
  {%expect|
    (Element
     ((tag_name div)
      (attributes
       ((class test_vdom_hook_add_class__inline_class_hash_0ccd3487a8)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="test_vdom_hook_add_class__inline_class_hash_replaced_in_test"
         add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="test_vdom_hook_add_class__inline_class_hash_replaced_in_test"> </div>
      </body>
    </html>
    |};
  let node = {%html|<div %{create_hook()} style="display: flex"></div>|} in
  show node;
  {%expect|
    (Element
     ((tag_name div)
      (attributes
       ((class test_vdom_hook_add_class__inline_class_hash_553e50da47)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="test_vdom_hook_add_class__inline_class_hash_replaced_in_test"
         add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="test_vdom_hook_add_class__inline_class_hash_replaced_in_test"> </div>
      </body>
    </html>
    |};
  (* PPX tailwind *)
  let node = {%html|<div tailwind="flex" %{create_hook()}></div>|} in
  show node;
  {%expect|
    (Element
     ((tag_name div) (attributes ((class tw_hash_e01f1d87fe-flex)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="tw_hash_replaced_in_test-flex" add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="tw_hash_replaced_in_test-flex"> </div>
      </body>
    </html>
    |};
  let node = {%html|<div %{create_hook()} tailwind="flex"></div>|} in
  show node;
  {%expect|
    (Element
     ((tag_name div) (attributes ((class tw_hash_e01f1d87fe-flex)))
      (hooks ((add_class_on_init_hook ())))))

    ---------- Vdom ----------

    <div class="tw_hash_replaced_in_test-flex" add_class_on_init_hook=()> </div>

    ---------- Actual DOM ----------

    <html>
      <head> </head>
      <body>
        <div class="tw_hash_replaced_in_test-flex"> </div>
      </body>
    </html>
    |}
;;
