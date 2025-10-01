open! Core
module Effect = Ui_effect

module%test [@name "Effect.Result"] _ = struct
  open Effect.Result.Let_syntax

  let test_effect effect =
    Effect.Expert.handle
      ~on_exn:raise
      (let%bind.Effect effect in
       Effect.print_s [%message (effect : (string, string) Result.t)])
  ;;

  let%expect_test "monad bind works as expected" =
    let effect =
      let%bind () = return () in
      let%bind () = Effect.Result.fail "I am printing but I'm the failure type" in
      return "I should not print since the previous one fails"
    in
    test_effect effect;
    [%expect {| (effect (Error "I am printing but I'm the failure type")) |}]
  ;;

  let%expect_test "monadic map works as expected" =
    let effect =
      let%bind () = return () in
      let%map () = Effect.Result.fail "I am printing but I'm the failure type" in
      "I should not print since the previous one fails"
    in
    test_effect effect;
    [%expect {| (effect (Error "I am printing but I'm the failure type")) |}]
  ;;
end

module%test [@name "Combining Effect.Result"] _ = struct
  open Effect.Result.Let_syntax

  module C = struct
    type t =
      | A of string
      | Failure of string
    [@@deriving sexp]

    let from_a_b a b =
      match a with
      | `A -> A b
      | _ -> Failure b
    ;;
  end

  let test_effect effect =
    Effect.Expert.handle
      ~on_exn:raise
      (let%bind.Effect effect in
       Effect.print_s [%message (effect : (C.t, Error.t) Result.t)])
  ;;

  let combine_errors err_a err_b =
    Error.create_s [%message "combining two errors" (err_a : Error.t) (err_b : Error.t)]
  ;;

  let%expect_test "Both OK" =
    let a = return `A in
    let b = return "Inner B" in
    Effect.Result.combine a b ~ok:C.from_a_b ~err:combine_errors |> test_effect;
    [%expect {| (effect (Ok (A "Inner B"))) |}]
  ;;

  let%expect_test "Fail A, B ok" =
    let b = return "B" in
    let a = Effect.Result.fail (Error.create_s [%message "fail a"]) in
    Effect.Result.combine a b ~ok:C.from_a_b ~err:combine_errors |> test_effect;
    [%expect {| (effect (Error "fail a")) |}]
  ;;

  let%expect_test "A ok, Fail B" =
    let a = return `A in
    let b = Effect.Result.fail (Error.create_s [%message "fail b"]) in
    Effect.Result.combine a b ~ok:C.from_a_b ~err:combine_errors |> test_effect;
    [%expect {| (effect (Error "fail b")) |}]
  ;;

  let%expect_test "Fail both" =
    let a = Effect.Result.fail (Error.create_s [%message "fail a"]) in
    let b = Effect.Result.fail (Error.create_s [%message "fail b"]) in
    Effect.Result.combine a b ~ok:C.from_a_b ~err:combine_errors |> test_effect;
    [%expect
      {| (effect (Error ("combining two errors" (err_a "fail a") (err_b "fail b")))) |}]
  ;;
end

module%test [@name "Effect.Or_error"] _ = struct
  open Effect.Or_error.Let_syntax

  let test_effect effect =
    Effect.Expert.handle
      ~on_exn:raise
      (let%bind.Effect effect in
       Effect.print_s [%message (effect : string Or_error.t)])
  ;;

  let%expect_test "monad bind works as expected" =
    let effect =
      let%bind () = return () in
      let%bind () =
        Effect.Or_error.error_string "I am printing but I'm the failure type"
      in
      return "I should not print since the previous one fails"
    in
    test_effect effect;
    [%expect {| (effect (Error "I am printing but I'm the failure type")) |}]
  ;;

  let%expect_test "monadic map works as expected" =
    let effect =
      let%bind () = return () in
      let%map () =
        Effect.Or_error.error_s [%message "I am printing but I'm the failure type"]
      in
      "I should not print since the previous one fails"
    in
    test_effect effect;
    [%expect {| (effect (Error "I am printing but I'm the failure type")) |}]
  ;;
end
