open! Core
module Effect = Ui_effect
open Effect.Let_syntax

module Svar = struct
  let create () =
    let var = Effect.For_testing.Svar.create () in
    ( Effect.For_testing.Svar.fill_if_empty var
    , Effect.For_testing.of_svar_fun (fun () -> var) () )
  ;;
end

module Degenerate_svar = struct
  (* the degenerate svar allows multiple resumptions of an effect *)

  let create () =
    let setter = ref (fun _ -> ()) in
    let eff = Effect.Expert.of_fun ~f:(fun ~callback ~on_exn:_ -> setter := callback) in
    (fun v -> !setter v), eff
  ;;
end

let both_not_parallel ~(here : [%call_pos]) a b =
  let%map ret = Effect.all [ a; b ] in
  match ret with
  | [ a; b ] -> a, b
  | _ -> raise_s [%message "TESTING BUG" (here : Source_code_position.t)]
;;

let bisimulate ~parallel ~normal ~f =
  f parallel ~expect_diff:(fun ~parallel ~normal:_ -> parallel ());
  f normal ~expect_diff:(fun ~parallel:_ ~normal -> normal ())
;;

let bisimulate_both_boths ~f =
  bisimulate ~parallel:Effect.both_parallel ~normal:(fun a b -> both_not_parallel a b) ~f
;;

let bisimulate_both_alls ~f =
  bisimulate ~parallel:Effect.all_parallel ~normal:Effect.all ~f
;;

let bisimulate_both_all_units ~f =
  bisimulate ~parallel:Effect.all_parallel_unit ~normal:Effect.all_unit ~f
;;

let%expect_test "Effect.both_parallel" =
  bisimulate_both_boths ~f:(fun both ~expect_diff:_ ->
    let fill_eff1, eff1 = Svar.create () in
    let fill_eff2, eff2 = Svar.create () in
    let effect =
      let%map a, b =
        both
          (let%map () = eff1 in
           print_endline "eff1 done";
           1)
          (let%map () = eff2 in
           print_endline "eff2 done";
           2)
      in
      printf "effect done: %d %d" a b
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff1 ();
    [%expect {| eff1 done |}];
    fill_eff2 ();
    [%expect
      {|
      eff2 done
      effect done: 1 2
      |}])
;;

let%expect_test "Effect.both_parallel using let%map syntax" =
  let fill_eff1, eff1 = Svar.create () in
  let fill_eff2, eff2 = Svar.create () in
  let effect =
    let%map.Effect.Par a =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    and b =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    printf "effect done: %d %d" a b
  in
  Effect.Expert.handle effect ~on_exn:Base.raise;
  fill_eff1 ();
  [%expect {| eff1 done |}];
  fill_eff2 ();
  [%expect
    {|
    eff2 done
    effect done: 1 2
    |}]
;;

let%expect_test "Effect.both_parallel using let%bind syntax" =
  let fill_eff1, eff1 = Svar.create () in
  let fill_eff2, eff2 = Svar.create () in
  let effect =
    let%bind.Effect.Par a =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    and b =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    return (printf "effect done: %d %d" a b)
  in
  Effect.Expert.handle effect ~on_exn:Base.raise;
  fill_eff1 ();
  [%expect {| eff1 done |}];
  fill_eff2 ();
  [%expect
    {|
    eff2 done
    effect done: 1 2
    |}]
;;

let%expect_test "Effect.both_parallel using let%map syntax - filled in reverse" =
  let fill_eff1, eff1 = Svar.create () in
  let fill_eff2, eff2 = Svar.create () in
  let effect =
    let%map.Effect.Par a =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    and b =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    printf "effect done: %d %d" a b
  in
  Effect.Expert.handle effect ~on_exn:Base.raise;
  fill_eff2 ();
  [%expect {| eff2 done |}];
  fill_eff1 ();
  [%expect
    {|
    eff1 done
    effect done: 1 2
    |}]
;;

let%expect_test "Effect.both_parallel using let%bind syntax - filled in reverse" =
  let fill_eff1, eff1 = Svar.create () in
  let fill_eff2, eff2 = Svar.create () in
  let effect =
    let%bind.Effect.Par a =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    and b =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    return (printf "effect done: %d %d" a b)
  in
  Effect.Expert.handle effect ~on_exn:Base.raise;
  fill_eff2 ();
  [%expect {| eff2 done |}];
  fill_eff1 ();
  [%expect
    {|
    eff1 done
    effect done: 1 2
    |}]
;;

let%expect_test "Effect.both_parallel degenerate" =
  bisimulate_both_boths ~f:(fun both ~expect_diff ->
    let fill_eff1, eff1 = Degenerate_svar.create () in
    let fill_eff2, eff2 = Degenerate_svar.create () in
    let effect =
      let%map a, b =
        both
          (let%map () = eff1 in
           print_endline "eff1 done";
           1)
          (let%map () = eff2 in
           print_endline "eff2 done";
           2)
      in
      printf "effect done: %d %d" a b
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff1 ();
    [%expect {| eff1 done |}];
    fill_eff1 ();
    [%expect {| |}];
    fill_eff2 ();
    [%expect
      {|
      eff2 done
      effect done: 1 2
      |}];
    fill_eff2 ();
    expect_diff ~parallel:(fun () -> [%expect {| |}]) ~normal:(fun () -> [%expect {| |}]);
    fill_eff1 ();
    [%expect {| |}])
;;

let%expect_test "Effect.both_parallel already filled" =
  bisimulate_both_boths ~f:(fun both ~expect_diff:_ ->
    let fill_eff1, eff1 = Svar.create () in
    fill_eff1 ();
    let fill_eff2, eff2 = Svar.create () in
    fill_eff2 ();
    let effect =
      let%map a, b =
        both
          (let%map () = eff1 in
           print_endline "eff1 done";
           1)
          (let%map () = eff2 in
           print_endline "eff2 done";
           2)
      in
      printf "effect done: %d %d" a b
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    [%expect
      {|
      eff1 done
      eff2 done
      effect done: 1 2
      |}])
;;

let%expect_test "Effect.both_parallel out of order" =
  bisimulate_both_boths ~f:(fun both ~expect_diff ->
    let fill_eff1, eff1 = Svar.create () in
    let fill_eff2, eff2 = Svar.create () in
    let effect =
      let%map a, b =
        both
          (let%map () = eff1 in
           print_endline "eff1 done";
           1)
          (let%map () = eff2 in
           print_endline "eff2 done";
           2)
      in
      printf "effect done: %d %d" a b
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff2 ();
    (* NOTE: This test shows that the effects in [all] are only filled in order, while
       both_parallel allows out-of-order effects to be filled earlier. *)
    expect_diff
      ~parallel:(fun () -> [%expect {| eff2 done |}])
      ~normal:(fun () -> [%expect {| |}]);
    fill_eff1 ();
    expect_diff
      ~parallel:(fun () ->
        [%expect
          {|
          eff1 done
          effect done: 1 2
          |}])
      ~normal:(fun () ->
        [%expect
          {|
          eff1 done
          eff2 done
          effect done: 1 2
          |}]))
;;

let%expect_test "Effect.all_parallel" =
  bisimulate_both_alls ~f:(fun all ~expect_diff:_ ->
    let fill_eff1, eff1 = Svar.create () in
    let fill_eff2, eff2 = Svar.create () in
    let fill_eff3, eff3 = Svar.create () in
    let eff1 =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    in
    let eff2 =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    let eff3 =
      let%map () = eff3 in
      print_endline "eff3 done";
      3
    in
    let effect =
      let%map all = all [ eff1; eff2; eff3 ] in
      print_s [%message (all : int list)]
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff1 ();
    [%expect {| eff1 done |}];
    fill_eff2 ();
    [%expect {| eff2 done |}];
    fill_eff3 ();
    [%expect
      {|
      eff3 done
      (all (1 2 3))
      |}])
;;

let%expect_test "Effect.all_parallel degenerate" =
  bisimulate_both_alls ~f:(fun all ~expect_diff ->
    let fill_eff1, eff1 = Degenerate_svar.create () in
    let fill_eff2, eff2 = Degenerate_svar.create () in
    let fill_eff3, eff3 = Degenerate_svar.create () in
    let eff1 =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    in
    let eff2 =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    let eff3 =
      let%map () = eff3 in
      print_endline "eff3 done";
      3
    in
    let effect =
      let%map all = all [ eff1; eff2; eff3 ] in
      print_s [%message (all : int list)]
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff1 ();
    [%expect {| eff1 done |}];
    fill_eff1 ();
    [%expect {| |}];
    fill_eff2 ();
    [%expect {| eff2 done |}];
    fill_eff2 ();
    [%expect {| |}];
    fill_eff3 ();
    [%expect
      {|
      eff3 done
      (all (1 2 3))
      |}];
    fill_eff3 ();
    expect_diff ~parallel:(fun () -> [%expect {| |}]) ~normal:(fun () -> [%expect {| |}]))
;;

let%expect_test "Effect.all_parallel already filled" =
  bisimulate_both_alls ~f:(fun all ~expect_diff:_ ->
    let fill_eff1, eff1 = Svar.create () in
    fill_eff1 ();
    let fill_eff2, eff2 = Svar.create () in
    fill_eff2 ();
    let fill_eff3, eff3 = Svar.create () in
    fill_eff3 ();
    let eff1 =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    in
    let eff2 =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    let eff3 =
      let%map () = eff3 in
      print_endline "eff3 done";
      3
    in
    let effect =
      let%map all = all [ eff1; eff2; eff3 ] in
      print_s [%message (all : int list)]
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    [%expect
      {|
      eff1 done
      eff2 done
      eff3 done
      (all (1 2 3))
      |}])
;;

let%expect_test "Effect.all_parallel out of order" =
  bisimulate_both_alls ~f:(fun all ~expect_diff ->
    let fill_eff1, eff1 = Svar.create () in
    let fill_eff2, eff2 = Svar.create () in
    let fill_eff3, eff3 = Svar.create () in
    let eff1 =
      let%map () = eff1 in
      print_endline "eff1 done";
      1
    in
    let eff2 =
      let%map () = eff2 in
      print_endline "eff2 done";
      2
    in
    let eff3 =
      let%map () = eff3 in
      print_endline "eff3 done";
      3
    in
    let effect =
      let%map all = all [ eff1; eff2; eff3 ] in
      print_s [%message (all : int list)]
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff2 ();
    expect_diff
      ~parallel:(fun () -> [%expect {| eff2 done |}])
      ~normal:(fun () -> [%expect {| |}]);
    fill_eff3 ();
    expect_diff
      ~parallel:(fun () -> [%expect {| eff3 done |}])
      ~normal:(fun () -> [%expect {| |}]);
    fill_eff1 ();
    (* NOTE: In the "normal"/non-parallel case, none of the events occurs unless they its
       previous effect in the list has occurred. *)
    expect_diff
      ~parallel:(fun () ->
        [%expect
          {|
          eff1 done
          (all (1 2 3))
          |}])
      ~normal:(fun () ->
        [%expect
          {|
          eff1 done
          eff2 done
          eff3 done
          (all (1 2 3))
          |}]))
;;

let%expect_test "Effect.all_parallel_unit sanity check" =
  bisimulate_both_all_units ~f:(fun all_unit ~expect_diff ->
    let fill_eff1, eff1 = Svar.create () in
    let fill_eff2, eff2 = Svar.create () in
    let fill_eff3, eff3 = Svar.create () in
    let eff1 =
      let%bind () = eff1 in
      Effect.print_s [%message "eff1 done"]
    in
    let eff2 =
      let%bind () = eff2 in
      Effect.print_s [%message "eff2 done"]
    in
    let eff3 =
      let%bind () = eff3 in
      Effect.print_s [%message "eff3 done"]
    in
    let effect =
      let%bind () = all_unit [ eff1; eff2; eff3 ] in
      Effect.print_s [%message "all done!"]
    in
    Effect.Expert.handle effect ~on_exn:Base.raise;
    fill_eff2 ();
    expect_diff
      ~parallel:(fun () -> [%expect {| "eff2 done" |}])
      ~normal:(fun () -> [%expect {| |}]);
    fill_eff3 ();
    expect_diff
      ~parallel:(fun () -> [%expect {| "eff3 done" |}])
      ~normal:(fun () -> [%expect {| |}]);
    fill_eff1 ();
    (* NOTE: In the "normal"/non-parallel case, none of the events occurs unless they its
       previous effect in the list has occurred. *)
    expect_diff
      ~parallel:(fun () ->
        [%expect
          {|
          "eff1 done"
          "all done!"
          |}])
      ~normal:(fun () ->
        [%expect
          {|
          "eff1 done"
          "eff2 done"
          "eff3 done"
          "all done!"
          |}]))
;;
