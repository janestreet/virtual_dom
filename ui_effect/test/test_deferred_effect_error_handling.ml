open! Core
open Async
module Effect = Ui_effect

let print_exn exn = print_endline (Exn.to_string exn)

let%expect_test "Raising synchronously inside deferred function" =
  let raise_effect =
    Ui_effect_of_deferred.of_deferred_thunk (fun () ->
      raise_s [%message "this is a raised exn!"])
  in
  Effect.Expert.handle raise_effect ~on_exn:print_exn;
  let%map () = Async_kernel.after (Time_ns.Span.of_sec 0.01) in
  [%expect {| "this is a raised exn!" |}]
;;

let%expect_test "Raising inside deferred function" =
  let raise_effect =
    Ui_effect_of_deferred.of_deferred_thunk (fun () ->
      let raise_once () =
        let%bind () = Deferred.return () in
        raise_s [%message "this is a raised exn!"]
      in
      Deferred.all_unit (List.init 5 ~f:(fun _ -> raise_once ())))
  in
  Effect.Expert.handle raise_effect ~on_exn:print_exn;
  let%map () = Async_kernel.after (Time_ns.Span.of_sec 0.01) in
  [%expect
    {|
    "this is a raised exn!"
    "this is a raised exn!"
    "this is a raised exn!"
    "this is a raised exn!"
    "this is a raised exn!"
    |}]
;;

let%expect_test "Calling [on_exn] inside deferred function" =
  let raise_effect =
    Ui_effect_of_deferred.of_deferred_thunk' (fun () ~on_exn ->
      let%map () = Deferred.return () in
      on_exn (Exn.create_s [%message "this is an exn!"]))
  in
  Effect.Expert.handle raise_effect ~on_exn:print_exn;
  let%map () = Async_kernel.after (Time_ns.Span.of_sec 0.01) in
  [%expect {| "this is an exn!" |}]
;;
