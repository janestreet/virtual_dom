open! Core
open Vdom_input_widgets

let%expect_test "check that datetime_local parses input time correctly" =
  let time = Time_ns.of_string "2025-01-15 13:30:10Z" in
  let last_input_value = ref (Some time) in
  let on_input value =
    last_input_value := value;
    Virtual_dom.Vdom.Effect.Ignore
  in
  let node = Entry.datetime_local ~value:(Some time) ~on_input () in
  let node_helper = Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn node in
  print_s [%message (last_input_value : Time_ns.t option ref)];
  [%expect {| (last_input_value ((2025-01-15 08:30:10.000000000-05:00))) |}];
  let update_time_and_show ~text =
    Virtual_dom_test_helpers.Node_helpers.User_actions.input_text node_helper ~text;
    print_s [%message (last_input_value : Time_ns.t option ref)]
  in
  update_time_and_show ~text:"2025-01-15T13:31";
  [%expect {| (last_input_value ((2025-01-15 08:31:00.000000000-05:00))) |}];
  update_time_and_show ~text:"invalid input";
  [%expect {| (last_input_value ()) |}];
  update_time_and_show ~text:"2025-01-15T13:30:20";
  [%expect {| (last_input_value ((2025-01-15 08:30:20.000000000-05:00))) |}];
  update_time_and_show ~text:"2025-01-15T13:30:20.123";
  [%expect {| (last_input_value ((2025-01-15 08:30:20.123000000-05:00))) |}];
  update_time_and_show ~text:"2025-01-15T13:30:20.123.123";
  [%expect {| (last_input_value ()) |}]
;;
