open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Jsdom
module Handle = Handle_experimental

(** This test verifies that the [Value_normalizing_hook] for time inputs does not
    unnecessarily set the DOM element's value when the new value is semantically equal to
    the current value but has a different string representation (e.g., "13:37:00.000" vs
    "13:37").

    Note: jsdom does not normalize time input values the way real browsers do. Real
    browsers normalize "13:37:00.000" to "13:37" in the DOM. To simulate this, we manually
    set the DOM element's value to the browser-normalized form before triggering the hook
    update. *)

let get_input_value handle ~selector =
  let open Js_of_ocaml in
  let element = Handle.query_selector_exn handle ~selector ~here:[%here] in
  let input : Dom_html.inputElement Js.t = Js.Unsafe.coerce element in
  Js.to_string input##.value
;;

let set_input_value_directly handle ~selector ~value =
  let open Js_of_ocaml in
  let element = Handle.query_selector_exn handle ~selector ~here:[%here] in
  let input : Dom_html.inputElement Js.t = Js.Unsafe.coerce element in
  input##.value := Js.string value
;;

let time_component time_value _graph =
  let value = Bonsai.Expert.Var.value time_value in
  let on_input = Bonsai.return (fun (_ : Time_ns.Ofday.t option) -> Effect.Ignore) in
  let%arr value and on_input in
  Vdom_input_widgets.Entry.time
    ~extra_attrs:[ Vdom.Attr.id "time-input" ]
    ~value
    ~on_input
    ()
;;

let%expect_test "time input: semantically equal values with different string formats do \
                 not cause unnecessary DOM updates"
  =
  let time_value = Bonsai.Expert.Var.create (Some (Time_ns.Ofday.of_string "13:37")) in
  let%with handle =
    Handle.with_ ~here:[%here] ~get_vdom:Fn.id (time_component time_value)
  in
  let selector = "#time-input" in
  (* After the first frame, the hook sets the DOM value to "13:37:00.000"
     (Time_ns.Ofday.to_string format). *)
  Handle.one_frame handle;
  let dom_value = get_input_value handle ~selector in
  print_s [%message "after initial render" (dom_value : string)];
  [%expect {| ("after initial render" (dom_value 13:37:00.000)) |}];
  (* Simulate what a real browser does: it normalizes the value to "13:37". *)
  set_input_value_directly handle ~selector ~value:"13:37";
  (* Focus the input (as if the user is about to type). *)
  Handle.focus handle ~selector ~here:[%here];
  (* Trigger a re-render. The Bonsai value hasn't changed, so the hook's [update] will be
     called with the same "13:37:00.000" string, but the DOM now has "13:37". Without the
     fix, the hook would see "13:37:00.000" != "13:37" and clobber the DOM value. With the
     fix, [equal_time_value] recognizes them as the same time. *)
  Bonsai.Expert.Var.set time_value (Some (Time_ns.Ofday.of_string "13:37:00.000"));
  Handle.one_frame handle;
  let dom_value_after = get_input_value handle ~selector in
  print_s
    [%message "after setting equivalent value while focused" (dom_value_after : string)];
  [%expect {| ("after setting equivalent value while focused" (dom_value_after 13:37)) |}]
;;

let%expect_test "time input: genuinely different values still update the DOM" =
  let time_value = Bonsai.Expert.Var.create (Some (Time_ns.Ofday.of_string "13:37")) in
  let%with handle =
    Handle.with_ ~here:[%here] ~get_vdom:Fn.id (time_component time_value)
  in
  let selector = "#time-input" in
  Handle.one_frame handle;
  let dom_value = get_input_value handle ~selector in
  print_s [%message "initial" (dom_value : string)];
  [%expect {| (initial (dom_value 13:37:00.000)) |}];
  (* Change to a genuinely different time. *)
  Bonsai.Expert.Var.set time_value (Some (Time_ns.Ofday.of_string "14:00"));
  Handle.one_frame handle;
  let dom_value = get_input_value handle ~selector in
  print_s [%message "after changing to 14:00" (dom_value : string)];
  [%expect {| ("after changing to 14:00" (dom_value 14:00:00.000)) |}];
  (* Change to None. *)
  Bonsai.Expert.Var.set time_value None;
  Handle.one_frame handle;
  let dom_value = get_input_value handle ~selector in
  print_s [%message "after setting None" (dom_value : string)];
  [%expect {| ("after setting None" (dom_value "")) |}]
;;

let stateful_time_component graph =
  let time_value, set_time_value =
    Bonsai.state (Some (Time_ns.Ofday.of_string "13:37")) graph
  in
  let%arr value = time_value
  and set_time_value in
  ( Vdom_input_widgets.Entry.time
      ~extra_attrs:[ Vdom.Attr.id "time-input" ]
      ~value
      ~on_input:(fun v -> set_time_value v)
      ()
  , set_time_value )
;;

let%expect_test "time input: user edits while focused do not get clobbered by model \
                 round-trip"
  =
  (* This test simulates the real-world scenario where: a user focuses the time input and
     types a new value. The [on_input] callback fires and updates the Bonsai state. On the
     next frame, the hook's [update] is called with [Time_ns.Ofday.to_string] of the new
     value (e.g., "14:30:00.000"), but the DOM element holds the browser-normalized form
     (e.g., "14:30"). Without the [equal_time_value] guard, the hook would set the DOM
     value on every frame, which in a real browser breaks the segment-based digit-entry UI
     and makes two-digit values impossible to type. *)
  let%with handle = Handle.with_ ~here:[%here] ~get_vdom:fst stateful_time_component in
  let selector = "#time-input" in
  Handle.one_frame handle;
  let dom_value = get_input_value handle ~selector in
  print_s [%message "initial" (dom_value : string)];
  [%expect {| (initial (dom_value 13:37:00.000)) |}];
  (* Focus the input (user starts editing). *)
  Handle.focus handle ~selector ~here:[%here];
  (* The user types "14:30" into the input. In a real browser, this fires the input event
     and the DOM normalizes to "14:30". We simulate both: set the DOM value directly
     (simulating normalization) and fire the on_input callback via
     [set_input_element_value]. *)
  Handle.set_input_element_value handle ~selector ~value:"14:30" ~here:[%here];
  (* After this frame, Bonsai processes the on_input effect and updates state to
     [Some (Time_ns.Ofday.of_string "14:30")]. The hook will be called with new_value =
     "14:30:00.000" but the DOM has "14:30". *)
  Handle.one_frame handle;
  let dom_value = get_input_value handle ~selector in
  print_s [%message "after user types 14:30" (dom_value : string)];
  (* With [equal_time_value], the DOM value stays as-is because it recognizes "14:30" and
     "14:30:00.000" as the same time. Without it, this would be "14:30:00.000", which in a
     real browser would disrupt the segment-based entry. *)
  [%expect {| ("after user types 14:30" (dom_value 14:30)) |}]
;;
