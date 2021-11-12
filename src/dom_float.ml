open Js_of_ocaml

let to_js_string value = (Js.number_of_float value)##toString
let to_js_string_fixed digits value = (Js.number_of_float value)##toFixed digits
let to_js_string_precision digits value = (Js.number_of_float value)##toPrecision digits
let to_js_string_exponential value = (Js.number_of_float value)##toExponential
let to_string value = to_js_string value |> Js.to_string
let to_string_fixed digits value = to_js_string_fixed digits value |> Js.to_string
let to_string_precision digits value = to_js_string_precision digits value |> Js.to_string
let to_string_exponential value = to_js_string_exponential value |> Js.to_string

let%expect_test _ =
  let open Core in
  let print f = printf "%s" (to_string f) in
  print 1.;
  [%expect {| 1 |}];
  print Float.nan;
  [%expect {| NaN |}];
  print Float.infinity;
  [%expect {| Infinity |}];
  print Float.neg_infinity;
  [%expect {| -Infinity |}];
  print 0.00000001;
  [%expect {| 1e-8 |}];
  print (-1.);
  [%expect {| -1 |}];
  print 1.0000001;
  [%expect {| 1.0000001 |}]
;;
