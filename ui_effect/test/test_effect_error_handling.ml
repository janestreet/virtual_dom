open! Core
module Effect = Ui_effect
open Effect.Let_syntax

let on_exn exn = print_s [%message "handled by on_exn" (exn : Exn.t)]
let on_further_exns exn = print_s [%message "handled by on_further_exns" (exn : Exn.t)]
let test_exn = Exn.create_s [%message "this is an exn!"]
let test_error = Error.create_s [%message "this is an error!"]
let raise_effect = Effect.raise test_exn

let sequential_raise_effect =
  Effect.all_unit
    (List.init 5 ~f:(fun which_exn -> Effect.raise_s [%message (which_exn : int)]))
;;

let parallel_raise_effect =
  Effect.all_parallel_unit
    (List.init 5 ~f:(fun which_exn -> Effect.raise_s [%message (which_exn : int)]))
;;

module%test [@name "Effects that raise"] _ = struct
  let%expect_test "Effect.raise" =
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;

  let%expect_test "Effect.raise_s" =
    let raise_effect = Effect.raise_s [%message "a b c" (5 : int)] in
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn ("a b c" (5 5)))) |}]
  ;;

  let%expect_test "Effect.raise_error" =
    let raise_effect = Effect.raise_error test_error in
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an error!")) |}]
  ;;

  let%expect_test "Effect that calls [on_exn]" =
    let raise_effect =
      Effect.Expert.of_fun ~f:(fun ~callback:_ ~on_exn -> on_exn test_exn)
    in
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;

  let%expect_test "Effect that actually just raises" =
    let raise_effect =
      Effect.Expert.of_fun ~f:(fun ~callback:_ ~on_exn:_ ->
        failwith "this is a raised exn!")
    in
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "this is a raised exn!"))) |}]
  ;;

  let%expect_test "Raising sequentially" =
    Effect.Expert.handle sequential_raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (which_exn 0))) |}]
  ;;

  let%expect_test "Raising in parallel" =
    Effect.Expert.handle parallel_raise_effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("handled by on_exn" (exn (which_exn 0)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "Nested raising in parallel" =
    Effect.Expert.handle
      (Effect.all_parallel_unit [ parallel_raise_effect; parallel_raise_effect ])
      ~on_exn
      ~on_further_exns;
    [%expect
      {|
      ("handled by on_exn" (exn (which_exn 0)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      ("handled by on_further_exns" (exn (which_exn 0)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "Raising in Many effect" =
    let effects =
      [ Effect.Ignore
      ; Effect.raise_s [%message "second in many raised"]
      ; Effect.Ignore
      ; Effect.raise_s [%message "fourth in many raised"]
      ]
    in
    let effect = Effect.Many effects in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("handled by on_exn" (exn "second in many raised"))
      ("handled by on_further_exns" (exn "fourth in many raised"))
      |}]
  ;;
end

module%test [@name "Effect.lower_result and Effect.lower_or_error"] _ = struct
  let%expect_test "Effect.lower_result (non-raising)" =
    let result_effect = Effect.Result.return () in
    let normal_effect = Effect.lower_result result_effect in
    Effect.Expert.handle normal_effect ~on_exn ~on_further_exns;
    [%expect {| |}]
  ;;

  let%expect_test "Effect.lower_result (raising)" =
    let result_effect = Effect.Result.fail test_exn in
    let raise_effect = Effect.lower_result result_effect in
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;

  let%expect_test "Effect.lower_or_error (non-raising)" =
    let or_error_effect = Effect.Or_error.return () in
    let normal_effect = Effect.lower_or_error or_error_effect in
    Effect.Expert.handle normal_effect ~on_exn ~on_further_exns;
    [%expect {| |}]
  ;;

  let%expect_test "Effect.lower_or_error (raising)" =
    let or_error_effect = Effect.Or_error.fail test_error in
    let raise_effect = Effect.lower_or_error or_error_effect in
    Effect.Expert.handle raise_effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an error!")) |}]
  ;;
end

module%test [@name "Effect.try_with and Effect.try_with_or_error"] _ = struct
  type generic_try_with =
    { f :
        'exn.
        (?rest:[ `Raise | `Call of Exn.t -> unit ]
         -> unit Effect.t
         -> (unit, 'exn) result Effect.t)
        -> unit
    }

  let bisimulate_both_try_withs { f } =
    f Effect.try_with;
    f Effect.try_with_or_error
  ;;

  let () =
    bisimulate_both_try_withs
      { f =
          (fun try_with ->
            let module _ = struct
              let%expect_test "try_with on non-raising effect" =
                let normal_effect = Effect.Ignore in
                let handled_effect =
                  match%map try_with normal_effect with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                Effect.Expert.handle handled_effect ~on_exn ~on_further_exns;
                [%expect {| Ok! |}]
              ;;

              let%expect_test "try_with on raising effect" =
                let handled_effect =
                  match%map try_with raise_effect with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                Effect.Expert.handle handled_effect ~on_exn ~on_further_exns;
                [%expect {| Error! |}]
              ;;

              let%expect_test "try_with on sequential raising effect" =
                let handled_effect =
                  match%map try_with sequential_raise_effect with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                Effect.Expert.handle handled_effect ~on_exn ~on_further_exns;
                [%expect {| Error! |}]
              ;;

              let%expect_test "try_with on parallel raising effect" =
                let handled_effect =
                  match%map try_with parallel_raise_effect with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                Effect.Expert.handle handled_effect ~on_exn ~on_further_exns;
                [%expect
                  {|
                  Error!
                  ("handled by on_further_exns" (exn (which_exn 1)))
                  ("handled by on_further_exns" (exn (which_exn 2)))
                  ("handled by on_further_exns" (exn (which_exn 3)))
                  ("handled by on_further_exns" (exn (which_exn 4)))
                  |}]
              ;;

              let%expect_test "custom handler for further exns on try_with" =
                let custom_on_further_exns exn =
                  print_s [%message "this is different!" (exn : Exn.t)]
                in
                let custom_handled_effect =
                  match%map
                    try_with parallel_raise_effect ~rest:(`Call custom_on_further_exns)
                  with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                let default_handled_effect =
                  match%map try_with parallel_raise_effect ~rest:`Raise with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                Effect.Expert.handle
                  (Effect.all_parallel_unit
                     [ custom_handled_effect; default_handled_effect ])
                  ~on_exn
                  ~on_further_exns;
                [%expect
                  {|
                  Error!
                  ("this is different!" (exn (which_exn 1)))
                  ("this is different!" (exn (which_exn 2)))
                  ("this is different!" (exn (which_exn 3)))
                  ("this is different!" (exn (which_exn 4)))
                  Error!
                  ("handled by on_further_exns" (exn (which_exn 1)))
                  ("handled by on_further_exns" (exn (which_exn 2)))
                  ("handled by on_further_exns" (exn (which_exn 3)))
                  ("handled by on_further_exns" (exn (which_exn 4)))
                  |}]
              ;;

              let%expect_test "try_with and never" =
                let handled_effect =
                  match%map try_with Effect.never with
                  | Ok () -> print_endline "Ok!"
                  | Error _ -> print_endline "Error!"
                in
                Effect.Expert.handle handled_effect ~on_exn ~on_further_exns;
                [%expect {| |}]
              ;;
            end
            in
            ())
      }
  ;;

  let%expect_test "try_with and lower_result roundtrips" =
    let won't_raise = Effect.try_with raise_effect in
    let will_raise = Effect.lower_result won't_raise in
    Effect.Expert.handle will_raise ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;

  let%expect_test "try_with_or_error and lower_or_error roundtrips" =
    let won't_raise = Effect.try_with_or_error raise_effect in
    let will_raise = Effect.lower_or_error won't_raise in
    Effect.Expert.handle will_raise ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;
end

module%test [@name "Effect.iter_errors"] _ = struct
  let print_iter_errors_str str exn = Effect.print_s [%message str (exn : Exn.t)]
  let print_iter_errors = print_iter_errors_str "seen by iter_errors"

  let%expect_test "iter_errors with no errors" =
    let effect = Effect.iter_errors Effect.Ignore ~f:print_iter_errors in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| |}]
  ;;

  let%expect_test "iter_errors with one error" =
    let effect = Effect.iter_errors raise_effect ~f:print_iter_errors in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("seen by iter_errors" (exn "this is an exn!"))
      ("handled by on_exn" (exn "this is an exn!"))
      |}]
  ;;

  let%expect_test "iter_errors with sequential errors" =
    let effect = Effect.iter_errors sequential_raise_effect ~f:print_iter_errors in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("seen by iter_errors" (exn (which_exn 0)))
      ("handled by on_exn" (exn (which_exn 0)))
      |}]
  ;;

  let%expect_test "nested iter_errors" =
    let effect_1 = Effect.iter_errors raise_effect ~f:(print_iter_errors_str "1") in
    let effect_2 = Effect.iter_errors effect_1 ~f:(print_iter_errors_str "2") in
    let effect_3 = Effect.iter_errors effect_2 ~f:(print_iter_errors_str "3") in
    Effect.Expert.handle effect_3 ~on_exn ~on_further_exns;
    [%expect
      {|
      (1 (exn "this is an exn!"))
      (2 (exn "this is an exn!"))
      (3 (exn "this is an exn!"))
      ("handled by on_exn" (exn "this is an exn!"))
      |}]
  ;;

  let%expect_test "nested iter_errors within [f]" =
    let effect_1 =
      Effect.iter_errors (Effect.raise_s [%message "inner error"]) ~f:print_iter_errors
    in
    let effect_2 =
      Effect.iter_errors
        (Effect.raise_s [%message "outer error"])
        ~f:(fun exn -> Effect.all_unit [ print_iter_errors exn; effect_1 ])
    in
    Effect.Expert.handle effect_2 ~on_exn ~on_further_exns;
    [%expect
      {|
      ("seen by iter_errors" (exn "outer error"))
      ("seen by iter_errors" (exn "inner error"))
      ("handled by on_exn" (exn "outer error"))
      ("handled by on_further_exns" (exn "inner error"))
      |}]
  ;;

  let%expect_test "on_exn raises" =
    (try
       let effect = Effect.iter_errors raise_effect ~f:print_iter_errors in
       Effect.Expert.handle effect ~on_exn:Base.raise
     with
     | exn -> print_s [%message "raised" (exn : Exn.t)]);
    [%expect
      {|
      ("seen by iter_errors" (exn "this is an exn!"))
      (raised (exn "this is an exn!"))
      |}]
  ;;

  let%expect_test "iter_errors with parallel errors" =
    let effect = Effect.iter_errors parallel_raise_effect ~f:print_iter_errors in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    (* Notice that the [iter_errors] handlers are not all called together - they are
       intermixed with their respective exn handlers. Since effects are implemented via
       callbacks (and not all effects are synchronous), I don't think there's a good way
       to change this. *)
    [%expect
      {|
      ("seen by iter_errors" (exn (which_exn 0)))
      ("handled by on_exn" (exn (which_exn 0)))
      ("seen by iter_errors" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("seen by iter_errors" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("seen by iter_errors" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("seen by iter_errors" (exn (which_exn 4)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "try_with hides any [on_exn] exn from [iter_errors]" =
    let tried_effect =
      Effect.try_with parallel_raise_effect ~rest:`Raise |> Effect.ignore_m
    in
    let effect = Effect.iter_errors tried_effect ~f:print_iter_errors in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("seen by iter_errors" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("seen by iter_errors" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("seen by iter_errors" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("seen by iter_errors" (exn (which_exn 4)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "try_with can also hide _all_ exns from iter_errors with the power of \
                   [rest]"
    =
    let custom_on_further_exns exn =
      print_s [%message "handled by try_with" (exn : Exn.t)]
    in
    let tried_effect =
      Effect.try_with parallel_raise_effect ~rest:(`Call custom_on_further_exns)
      |> Effect.ignore_m
    in
    let effect = Effect.iter_errors tried_effect ~f:print_iter_errors in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("handled by try_with" (exn (which_exn 1)))
      ("handled by try_with" (exn (which_exn 2)))
      ("handled by try_with" (exn (which_exn 3)))
      ("handled by try_with" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "iter_errors waits for [f]'s output to complete" =
    let effect = Effect.iter_errors raise_effect ~f:(fun _ -> Effect.never) in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| |}]
  ;;
end

module%test [@name "Effect.protect"] _ = struct
  let print_finally_str str = Effect.print_s [%message str]
  let print_finally = print_finally_str "finally executed"

  let%expect_test "protect with successful effect" =
    let effect = Effect.protect Effect.Ignore ~finally:print_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| "finally executed" |}]
  ;;

  let%expect_test "protect with raising effect" =
    let effect = Effect.protect raise_effect ~finally:print_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      "finally executed"
      ("handled by on_exn" (exn "this is an exn!"))
      |}]
  ;;

  let%expect_test "protect with parallel raising effect" =
    let effect = Effect.protect parallel_raise_effect ~finally:print_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      "finally executed"
      ("handled by on_exn" (exn (which_exn 0)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "protect with finally effect that raises" =
    let raising_finally = Effect.raise_s [%message "finally raised!"] in
    let effect = Effect.protect Effect.Ignore ~finally:raising_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn "finally raised!")) |}]
  ;;

  let%expect_test "protect with both main and finally effects raising" =
    let raising_finally = Effect.raise_s [%message "finally raised!"] in
    let effect = Effect.protect raise_effect ~finally:raising_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("handled by on_exn" (exn "this is an exn!"))
      ("handled by on_further_exns" (exn "finally raised!"))
      |}]
  ;;

  let%expect_test "nested protect" =
    let inner_finally = print_finally_str "inner finally" in
    let outer_finally = print_finally_str "outer finally" in
    let inner_effect = Effect.protect Effect.Ignore ~finally:inner_finally in
    let outer_effect = Effect.protect inner_effect ~finally:outer_finally in
    Effect.Expert.handle outer_effect ~on_exn ~on_further_exns;
    [%expect
      {|
      "inner finally"
      "outer finally"
      |}]
  ;;

  let%expect_test "nested protect with inner raising" =
    let inner_finally = print_finally_str "inner finally" in
    let outer_finally = print_finally_str "outer finally" in
    let inner_effect = Effect.protect raise_effect ~finally:inner_finally in
    let outer_effect = Effect.protect inner_effect ~finally:outer_finally in
    Effect.Expert.handle outer_effect ~on_exn ~on_further_exns;
    [%expect
      {|
      "inner finally"
      "outer finally"
      ("handled by on_exn" (exn "this is an exn!"))
      |}]
  ;;

  let%expect_test "nested protect with everything raising" =
    let main_effect = Effect.raise_s [%message "main raised"] in
    let inner_finally = Effect.raise_s [%message "inner finally raised"] in
    let outer_finally = Effect.raise_s [%message "outer finally raised"] in
    let inner_effect = Effect.protect main_effect ~finally:inner_finally in
    let effect = Effect.protect inner_effect ~finally:outer_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    (* Note that the outer raise exn handler is called before the inner raise. We could
       call [on_further_exns] on the inner exn before the outer one to prevent this, but
       it would change the order of other things in unintuitive ways (e.g. [iter_errors]
       would see the exn from [finally] before [main_effect], and effects could raise
       with on_further_exns' exns rather than the first exn that's actually raised). *)
    [%expect
      {|
      ("handled by on_exn" (exn "main raised"))
      ("handled by on_further_exns" (exn "outer finally raised"))
      ("handled by on_further_exns" (exn "inner finally raised"))
      |}]
  ;;

  let%expect_test "protect with try_with" =
    let effect =
      Effect.protect
        (Effect.try_with raise_effect |> Effect.ignore_m)
        ~finally:print_finally
    in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| "finally executed" |}]
  ;;

  let%expect_test "protect with iter_errors" =
    let print_iter_errors exn =
      print_s [%message "seen by iter_errors" (exn : Exn.t)];
      Effect.Ignore
    in
    let effect =
      Effect.protect
        (Effect.iter_errors parallel_raise_effect ~f:print_iter_errors)
        ~finally:print_finally
    in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("seen by iter_errors" (exn (which_exn 0)))
      "finally executed"
      ("handled by on_exn" (exn (which_exn 0)))
      ("seen by iter_errors" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("seen by iter_errors" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("seen by iter_errors" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("seen by iter_errors" (exn (which_exn 4)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "protect with never effect" =
    let effect = Effect.protect Effect.never ~finally:print_finally in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    (* The never effect doesn't complete, so finally should not execute *)
    [%expect {| |}]
  ;;

  let%expect_test "protect with never in finally" =
    let inner_effect = Effect.protect Effect.Ignore ~finally:Effect.never in
    let outer_effect =
      let%bind () = inner_effect in
      print_finally
    in
    Effect.Expert.handle outer_effect ~on_exn ~on_further_exns;
    [%expect {| |}]
  ;;

  let%expect_test "protect with multiple finally effects in parallel" =
    let effect = Effect.protect Effect.Ignore ~finally:parallel_raise_effect in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("handled by on_exn" (exn (which_exn 0)))
      ("handled by on_further_exns" (exn (which_exn 1)))
      ("handled by on_further_exns" (exn (which_exn 2)))
      ("handled by on_further_exns" (exn (which_exn 3)))
      ("handled by on_further_exns" (exn (which_exn 4)))
      |}]
  ;;

  let%expect_test "Complex nested raising scenario" =
    let inner_effect =
      Effect.protect
        (Effect.both_parallel
           (Effect.raise_s [%message "inner left"])
           (Effect.raise_s [%message "inner right"])
         |> Effect.ignore_m)
        ~finally:
          (Effect.all_unit
             [ Effect.print_s [%message "inner finally"]
             ; Effect.raise_s [%message "inner finally raised"]
             ])
    in
    let middle_effect =
      Effect.iter_errors inner_effect ~f:(fun exn ->
        print_s [%message "seen by iter_errors" (exn : Exn.t)];
        if Sexp.equal (Exn.sexp_of_t exn) [%sexp "inner right"]
        then Effect.raise_s [%message "iter_errors callback raised"]
        else Effect.Ignore)
    in
    let outer_effect =
      Effect.protect
        middle_effect
        ~finally:
          (Effect.all_unit
             [ Effect.print_s [%message "outer finally"]
             ; Effect.raise_s [%message "outer finally raised"]
             ])
    in
    Effect.Expert.handle outer_effect ~on_exn ~on_further_exns;
    [%expect
      {|
      "inner finally"
      ("seen by iter_errors" (exn "inner left"))
      "outer finally"
      ("handled by on_exn" (exn "inner left"))
      ("handled by on_further_exns" (exn "outer finally raised"))
      ("seen by iter_errors" (exn "inner finally raised"))
      ("handled by on_further_exns" (exn "inner finally raised"))
      ("seen by iter_errors" (exn "inner right"))
      ("handled by on_further_exns" (exn "inner right"))
      ("handled by on_further_exns" (exn "iter_errors callback raised"))
      |}]
  ;;
end

module%test [@name "reraising should only wrap exns once"] _ = struct
  let handle effect =
    try
      Effect.Expert.handle effect ~on_exn:(fun exn -> Exn.reraise exn "reraising...")
    with
    | exn -> print_s (Exn.sexp_of_t exn)
  ;;

  let%expect_test "single raise" =
    handle raise_effect;
    [%expect {| (exn.ml.Reraised reraising... "this is an exn!") |}]
  ;;

  let%expect_test "multiple raises/Effect.Expert.of_fun" =
    handle parallel_raise_effect;
    [%expect {| (exn.ml.Reraised reraising... (which_exn 0)) |}]
  ;;

  let%expect_test "Complex nested raising scenario" =
    let inner_effect =
      Effect.protect
        (Effect.both_parallel
           (Effect.raise_s [%message "inner left"])
           (Effect.raise_s [%message "inner right"])
         |> Effect.ignore_m)
        ~finally:(Effect.raise_s [%message "inner finally raised"])
    in
    let middle_effect =
      Effect.iter_errors inner_effect ~f:(fun exn ->
        if Sexp.equal (Exn.sexp_of_t exn) [%sexp "inner left"]
        then Effect.raise_s [%message "iter_errors callback raised"]
        else Effect.Ignore)
    in
    let outer_effect =
      Effect.protect
        middle_effect
        ~finally:(Effect.raise_s [%message "outer finally raised"])
    in
    handle outer_effect;
    [%expect {| (exn.ml.Reraised reraising... "inner left") |}]
  ;;
end

(* just to make sure we don't miss adding [try...with] anywhere *)
module%test [@name "Raising at various places"] _ = struct
  let%expect_test "Raising inside bind function" =
    let effect =
      let%bind () = Effect.Ignore in
      failwith "this is a raised exn!"
    in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "this is a raised exn!"))) |}]
  ;;

  let%expect_test "Raising inside map function" =
    let effect =
      let%map () = Effect.Ignore in
      failwith "this is a raised exn!"
    in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "this is a raised exn!"))) |}]
  ;;

  let%expect_test "Raising inside lazy" =
    let effect = Effect.lazy_ (lazy (failwith "this is a raised exn!")) in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "this is a raised exn!"))) |}]
  ;;

  let%expect_test "Raising in eval's [f]" =
    Effect.Expert.eval
      Effect.Ignore
      ~f:(fun () -> failwith "this is a raised exn!")
      ~on_exn
      ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "this is a raised exn!"))) |}]
  ;;

  let%expect_test "Handle inside eval" =
    Effect.Expert.eval
      Effect.Ignore
      ~f:(fun () -> Effect.Expert.handle raise_effect ~on_exn ~on_further_exns)
      ~on_exn:(fun _ -> print_endline "I should not be called!");
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;

  let%expect_test "Raising inside of_sync_fun" =
    let effect = Effect.of_sync_fun (fun () -> failwith "sync fun raised!") () in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "sync fun raised!"))) |}]
  ;;

  let%expect_test "Raising inside of_thunk" =
    let effect = Effect.of_thunk (fun () -> failwith "thunk raised!") in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "thunk raised!"))) |}]
  ;;

  let%expect_test "Raising inside Expert.of_fun" =
    let effect =
      Effect.Expert.of_fun ~f:(fun ~callback:_ ~on_exn:_ -> failwith "callback raised!")
    in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect {| ("handled by on_exn" (exn (Failure "callback raised!"))) |}]
  ;;

  let%expect_test "Raising in iter_errors callback function" =
    let effect =
      Effect.iter_errors raise_effect ~f:(fun _ ->
        failwith "iter_errors callback raised!")
    in
    Effect.Expert.handle effect ~on_exn ~on_further_exns;
    [%expect
      {|
      ("handled by on_exn" (exn "this is an exn!"))
      ("handled by on_further_exns" (exn (Failure "iter_errors callback raised!")))
      |}]
  ;;
end

module%test
  [@name
    "it's possible to propogate exns without raising from a call to [handle] to an \
     enclosing call to [of_thunk'] and co., but not [of_thunk] and co."] _ =
struct
  let%expect_test "[handle] inside [of_thunk]" =
    let effect =
      Effect.of_thunk (fun () ->
        Effect.Expert.handle raise_effect ~on_exn:(fun exn ->
          Exn.reraise exn "No access to [on_exn], must raise!"))
    in
    (try Effect.Expert.handle effect ~on_exn ~on_further_exns with
     | e -> print_s [%message (e : Exn.t)]);
    [%expect
      {|
      ("handled by on_exn"
       (exn
        (exn.ml.Reraised "No access to [on_exn], must raise!" "this is an exn!")))
      |}]
  ;;

  let%expect_test "[handle] inside [of_thunk']" =
    let effect =
      Effect.of_thunk' (fun () ~on_exn -> Effect.Expert.handle raise_effect ~on_exn)
    in
    (try Effect.Expert.handle effect ~on_exn ~on_further_exns with
     | e -> print_s [%message (e : Exn.t)]);
    [%expect {| ("handled by on_exn" (exn "this is an exn!")) |}]
  ;;
end
