open! Core
open Virtual_dom
open Js_of_ocaml

module Hook = struct
  module T = struct
    module Input = struct
      type t = unit -> unit

      let sexp_of_t = sexp_of_opaque

      let combine a b () =
        a ();
        b ()
      ;;
    end

    module State = Unit

    let init _ _ = print_endline "init"
    let on_mount f () _ = f ()
    let on_mount = `Schedule_immediately_after_this_dom_patch_completes on_mount
    let update ~old_input:_ ~new_input:_ () _ = ()
    let destroy _ _ _ = print_endline "destroy"
  end

  include T
  include Vdom.Attr.Hooks.Make (T)
end

let hook ~f = Vdom.Attr.create_hook "print_on_init_and_mount" (Hook.create f)
let print_hook s = hook ~f:(fun () -> print_endline s)

module Non_handle_expect_test_config = struct
  module Expect_test_config = struct
    include Expect_test_config

    let run f =
      run (fun () ->
        Jsdom.Expert_for_custom_test_handles.reset_global_state_for_startup ();
        f ();
        Jsdom.Expert_for_custom_test_handles.reset_global_state_for_shutdown ())
    ;;
  end
end

module%test [@name "patch + diff"] _ = struct
  open Non_handle_expect_test_config

  let%expect_test "on_mount callbacks won't run if outside of [with_on_mount_at_end]" =
    let node = Vdom.Node.div ~attrs:[ print_hook "hi!" ] [] in
    let patch =
      Vdom.Node.Patch.create
        ~previous:(Vdom.Node.none_deprecated [@alert "-deprecated"])
        ~current:node
    in
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}];
    Vdom.Node.Patch.apply patch Dom_html.document##.body |> Fn.ignore;
    [%expect
      {|
      init
      You attempted to schedule an [on_mount] task somewhere other than within a [with_on_mount_at_end] call, which is not allowed, so the task will not be scheduled. This is usually caused by running [Vdom.Node.to_dom] or [Vdom.Node.Patch.apply] outside of a widget or hook.
      |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;

  let%expect_test "on_mount callbacks are registered during patch, not diff." =
    Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      let node = Vdom.Node.div ~attrs:[ print_hook "hi!" ] [] in
      let patch =
        Vdom.Node.Patch.create
          ~previous:(Vdom.Node.none_deprecated [@alert "-deprecated"])
          ~current:node
      in
      print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
      [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}];
      Vdom.Node.Patch.apply patch Dom_html.document##.body |> Fn.ignore);
    [%expect
      {|
      init
      hi!
      |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;

  let%expect_test "on_mount callbacks run even if disconnected, but print an error." =
    Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      let node = Vdom.Node.div ~attrs:[ print_hook "hi!" ] [] in
      let patch =
        Vdom.Node.Patch.create
          ~previous:(Vdom.Node.none_deprecated [@alert "-deprecated"])
          ~current:node
      in
      print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
      [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}];
      let fake_root = Dom_html.document##createElement (Js.string "div") in
      Vdom.Node.Patch.apply patch fake_root |> Fn.ignore);
    [%expect
      {|
      init
      WARN: [on_mount] hook ran, but element was not connected.[object HTMLDivElement]
      hi!
      |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;

  let%expect_test "patching in hook will trigger further [on_mount]s" =
    Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      let node =
        Vdom.Node.div
          [ Vdom.Node.div ~attrs:[ print_hook "Hook 1" ] []
          ; Vdom.Node.div ~attrs:[ print_hook "Hook 2"; print_hook "Hook 3" ] []
          ; Vdom.Node.div
              ~attrs:
                [ hook ~f:(fun () ->
                    let portalled = Dom_html.createDiv Dom_html.document in
                    Dom.appendChild Dom_html.document##.documentElement portalled;
                    let patch =
                      Vdom.Node.Patch.create
                        ~previous:(Vdom.Node.none_deprecated [@alert "-deprecated"])
                        ~current:(Vdom.Node.div ~attrs:[ print_hook "Inner Hook" ] [])
                    in
                    print_endline "start inner patch";
                    Fn.ignore (Vdom.Node.Patch.apply patch portalled);
                    print_endline "end inner patch")
                ]
              []
          ; Vdom.Node.div ~attrs:[ print_hook "Hook 4" ] []
          ]
      in
      let patch =
        Vdom.Node.Patch.create
          ~previous:(Vdom.Node.none_deprecated [@alert "-deprecated"])
          ~current:node
      in
      print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
      [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}];
      Vdom.Node.Patch.apply patch Js_of_ocaml.Dom_html.document##.body |> Fn.ignore);
    (* If tasks trigger other tasks, we run them in BFS order.*)
    [%expect
      {|
      init
      init
      init
      init
      Hook 1
      Hook 2
      Hook 3
      start inner patch
      init
      end inner patch
      Hook 4
      Inner Hook
      |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;
end

module%test [@name "Vdom.Node.to_dom"] _ = struct
  open Non_handle_expect_test_config

  let%expect_test "on_mount callbacks won't run if outside of [with_on_mount_at_end]" =
    let node = Vdom.Node.div ~attrs:[ print_hook "hi!" ] [] in
    let dom_node = Vdom.Node.to_dom node in
    [%expect
      {|
      init
      You attempted to schedule an [on_mount] task somewhere other than within a [with_on_mount_at_end] call, which is not allowed, so the task will not be scheduled. This is usually caused by running [Vdom.Node.to_dom] or [Vdom.Node.Patch.apply] outside of a widget or hook.
      |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}];
    Dom_html.document##.body##appendChild (dom_node :> Dom.node Js.t) |> Fn.ignore;
    [%expect {| |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;

  let%expect_test "on_mount callbacks run via [with_on_mount_at_end]" =
    Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      let node = Vdom.Node.div ~attrs:[ print_hook "hi!" ] [] in
      let dom_node = Vdom.Node.to_dom node in
      [%expect {| init |}];
      print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
      [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 1) |}];
      Dom_html.document##.body##appendChild (dom_node :> Dom.node Js.t) |> Fn.ignore);
    [%expect {| hi! |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;

  let%expect_test "on_mount callbacks run via [with_on_mount_at_end] even if not \
                   actually mounted, but print an error."
    =
    Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      let node = Vdom.Node.div ~attrs:[ print_hook "hi!" ] [] in
      Vdom.Node.to_dom node |> Fn.ignore;
      [%expect {| init |}];
      print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
      [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 1) |}]);
    [%expect
      {|
      WARN: [on_mount] hook ran, but element was not connected.[object HTMLDivElement]
      hi!
      |}];
    print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
    [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
  ;;
end

module%test [@name "Bonsai event loop"] _ = struct
  open Bonsai_web
  open Jsdom
  module Handle = Handle_experimental

  let nested_hook =
    hook ~f:(fun () ->
      print_endline "outer";
      let portalled = Dom_html.createDiv Dom_html.document in
      Dom.appendChild Dom_html.document##.documentElement portalled;
      let patch =
        Vdom.Node.Patch.create
          ~previous:(Vdom.Node.none_deprecated [@alert "-deprecated"])
          ~current:(Vdom.Node.div ~attrs:[ print_hook "Inner Hook" ] [])
      in
      print_endline "start inner patch";
      Fn.ignore (Vdom.Node.Patch.apply patch portalled);
      print_endline "end inner patch")
  ;;

  let%expect_test "basic hook, present from the start" =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:Fn.id (fun (local_ _graph) ->
        return {%html|<div %{print_hook "hi!"}></div>|})
    in
    [%expect
      {|
      init
      hi!
      |}];
    Handle.one_frame handle;
    [%expect {| |}]
  ;;

  let%expect_test "nested hook, present from the start" =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:Fn.id (fun (local_ _graph) ->
        return {%html|<div %{nested_hook}></div>|})
    in
    [%expect
      {|
      init
      outer
      start inner patch
      init
      end inner patch
      Inner Hook
      |}];
    Handle.one_frame handle;
    [%expect {| |}]
  ;;

  let%expect_test "nested hook, appears after mount, within a single frame." =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:Fn.id (fun (local_ graph) ->
        let show, toggle_show = Bonsai.toggle ~default_model:false graph in
        let subview =
          match%sub show with
          | true -> return {%html|<div %{nested_hook}></div>|}
          | false -> return Vdom.Node.none
        in
        let%arr subview and toggle_show in
        {%html|
          <div>
            <button on_click=%{fun _ ->toggle_show} id="toggle">Toggle</button>
            %{subview}
          </div>
        |})
    in
    [%expect {| |}];
    Handle.click_on handle ~selector:"#toggle";
    [%expect {| |}];
    Handle.one_frame handle;
    [%expect
      {|
      init
      outer
      start inner patch
      init
      end inner patch
      Inner Hook
      |}];
    Handle.click_on handle ~selector:"#toggle";
    [%expect {| |}];
    Handle.one_frame handle;
    [%expect {| destroy |}];
    (* Hook [on_mount] re-runs whenever re-mounted. *)
    Handle.click_on handle ~selector:"#toggle";
    [%expect {| |}];
    Handle.one_frame handle;
    [%expect
      {|
      init
      outer
      start inner patch
      init
      end inner patch
      Inner Hook
      |}]
  ;;

  let widget =
    Vdom.Node.widget
      ~id:(Type_equal.Id.create ~name:"runs_to_dom" (const [%sexp "runs_to_dom"]))
      ~init:(fun () ->
        Vdom.Node.span ~attrs:[ print_hook "on_mount: won't actually get mounted" ] []
        |> Vdom.Node.to_dom
        |> Fn.ignore;
        let main =
          Vdom.Node.div ~attrs:[ print_hook "on_mount: actual widget" ] []
          |> Vdom.Node.to_dom
        in
        (), main)
      ()
  ;;

  let%expect_test "Running [to_dom] inside widget: present from start" =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:Fn.id (fun (local_ _graph) -> return widget)
    in
    [%expect
      {|
      init
      init
      WARN: [on_mount] hook ran, but element was not connected.[object HTMLSpanElement]
      on_mount: won't actually get mounted
      on_mount: actual widget
      |}];
    Handle.one_frame handle;
    [%expect {| |}]
  ;;

  let%expect_test "Running [to_dom] inside widget: appears after mount, within a single \
                   frame."
    =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:Fn.id (fun (local_ graph) ->
        let show, toggle_show = Bonsai.toggle ~default_model:false graph in
        let subview =
          match%sub show with
          | true -> return widget
          | false -> return Vdom.Node.none
        in
        let%arr subview and toggle_show in
        {%html|
          <div>
            <button on_click=%{fun _ ->toggle_show} id="toggle">Toggle</button>
            %{subview}
          </div>
        |})
    in
    [%expect {| |}];
    Handle.click_on handle ~selector:"#toggle";
    [%expect {| |}];
    Handle.one_frame handle;
    [%expect
      {|
      init
      init
      WARN: [on_mount] hook ran, but element was not connected.[object HTMLSpanElement]
      on_mount: won't actually get mounted
      on_mount: actual widget
      |}];
    Handle.click_on handle ~selector:"#toggle";
    [%expect {| |}];
    Handle.one_frame handle;
    [%expect {| |}];
    (* Hook [on_mount] re-runs whenever re-mounted. *)
    Handle.click_on handle ~selector:"#toggle";
    [%expect {| |}];
    Handle.one_frame handle;
    [%expect
      {|
      init
      init
      WARN: [on_mount] hook ran, but element was not connected.[object HTMLSpanElement]
      on_mount: won't actually get mounted
      on_mount: actual widget
      |}]
  ;;
end

module%test [@name "destroy"] _ = struct
  let hook_that_creates_and_immediately_destroys =
    hook ~f:(fun () ->
      print_endline "outer";
      let portalled = Dom_html.createDiv Dom_html.document in
      Dom.appendChild Dom_html.document##.documentElement portalled;
      let vdom =
        Vdom.Node.div
          ~attrs:
            [ print_hook "Immediately destroyed after created: I SHOULD NEVER BE PRINTED"
            ]
          []
      in
      let add_patch = Vdom.Node.Patch.create ~previous:(Vdom.Node.div []) ~current:vdom in
      print_endline "start inner patch: add";
      Fn.ignore (Vdom.Node.Patch.apply add_patch portalled);
      print_endline "end inner patch: add";
      let remove_patch =
        Vdom.Node.Patch.create
          ~previous:vdom
          ~current:(Vdom.Node.none_deprecated [@alert "-deprecated"])
      in
      print_endline "start inner patch: remove";
      Fn.ignore (Vdom.Node.Patch.apply remove_patch portalled);
      print_endline "end inner patch: remove")
  ;;

  module%test [@name "non-handle"] _ = struct
    open Non_handle_expect_test_config

    let%expect_test "if an element is created and then immediately destroyed, its hook's \
                     on_mounts will not run"
      =
      Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
        let node =
          Vdom.Node.div ~attrs:[ hook_that_creates_and_immediately_destroys ] []
        in
        let dom_node = Vdom.Node.to_dom node in
        [%expect {| init |}];
        print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
        [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 1) |}];
        Dom_html.document##.body##appendChild (dom_node :> Dom.node Js.t) |> Fn.ignore);
      [%expect
        {|
        outer
        start inner patch: add
        init
        end inner patch: add
        start inner patch: remove
        destroy
        end inner patch: remove
        |}];
      print_s [%message (For_testing.num_queued_on_mount_tasks () : int)];
      [%expect {| ("For_testing.num_queued_on_mount_tasks ()" 0) |}]
    ;;
  end

  open Bonsai_web
  open Jsdom
  module Handle = Handle_experimental

  let%expect_test "if an element is created and then immediately destroyed, its hook's \
                   [on_mount]s will not run"
    =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:Fn.id (fun (local_ _graph) ->
        return {%html|<div %{hook_that_creates_and_immediately_destroys}>oof</div>|})
    in
    [%expect
      {|
      init
      outer
      start inner patch: add
      init
      end inner patch: add
      start inner patch: remove
      destroy
      end inner patch: remove
      |}];
    Handle.print_dom handle;
    [%expect
      {|
      <html>
        <head>
          <meta charset="UTF-8"/>
        </head>
        <body>
          <div tabindex="0" style="outline: none;"> oof </div>
        </body>
      </html>
      |}]
  ;;
end

module%test [@name "via requestAnimationFrame"] _ = struct
  open! Bonsai_web
  open Jsdom
  module Handle = Handle_experimental

  module Hook = struct
    module T = struct
      module Input = struct
        type t = unit [@@deriving sexp_of]

        let combine () () = ()
      end

      module State = Unit

      let init () _ = print_endline "init"
      let on_mount () () _ = print_endline "on_mount"
      let on_mount = `Schedule_animation_frame on_mount
      let update ~old_input:() ~new_input:() () _ = ()
      let destroy () _ _ = ()
    end

    include T
    include Vdom.Attr.Hooks.Make (T)
  end

  let hook = Vdom.Attr.create_hook "print_on_init_and_mount" (Hook.create ())

  module Layout = struct
    type t =
      | Standalone
      | Has_prev_sibling
      | Has_prev_none
      | Has_parent
      | Parent_has_prev_sibling
  end

  let one_frame_with_raf handle =
    Handle.run_request_animation_frame_tasks handle;
    Handle.one_frame handle
  ;;

  let%expect_test "on_mount runs when previous sibling is toggled" =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~get_vdom:fst (fun (local_ graph) ->
        let layout, set_layout = Bonsai.state Layout.Standalone graph in
        let elem = {%html|<div %{hook}>Has hook</div>|} in
        let view =
          match%arr layout with
          | Standalone -> Vdom.Node.div [ elem ]
          | Has_prev_sibling -> Vdom.Node.div [ Vdom.Node.text "hi"; elem ]
          | Has_prev_none -> Vdom.Node.div [ Vdom.Node.none; elem ]
          | Has_parent -> Vdom.Node.div [ Vdom.Node.div [ elem ] ]
          | Parent_has_prev_sibling ->
            Vdom.Node.div [ Vdom.Node.text "hi"; Vdom.Node.div [ elem ] ]
        in
        Bonsai.both view set_layout)
    in
    let inject layout = Handle.inject handle (fun (_, inject) -> inject layout) in
    [%expect {| init |}];
    Handle.print_dom handle;
    ignore ([%expect.output] : string);
    one_frame_with_raf handle;
    [%expect {| on_mount |}];
    Handle.print_dom_diff ~context:1 handle;
    [%expect {| |}];
    inject Has_prev_sibling;
    one_frame_with_raf handle;
    Handle.print_dom_diff ~context:1 handle;
    (* No raf scheduled warning is expected. *)
    [%expect
      {|
      WARNING: no requestAnimationFrame tasks queued!
      init
      === DIFF HUNK ===
            <div tabindex="0" style="outline: none;">
      +|      hi
              <div> Has hook </div>
      |}];
    (* We don't change any previous siblings, so a new batch of [init] -> [on_mount] doesn't
     run again. *)
    inject Has_prev_none;
    one_frame_with_raf handle;
    Handle.print_dom_diff ~context:1 handle;
    [%expect
      {|
      on_mount
      === DIFF HUNK ===
            <div tabindex="0" style="outline: none;">
      -|      hi
              <div> Has hook </div>
      |}];
    inject Standalone;
    one_frame_with_raf handle;
    Handle.print_dom_diff ~context:1 handle;
    (* No raf scheduled warning is expected. *)
    [%expect
      {|
      WARNING: no requestAnimationFrame tasks queued!
      init
      |}];
    inject Has_parent;
    one_frame_with_raf handle;
    Handle.print_dom_diff ~context:1 handle;
    [%expect
      {|
      on_mount
      init
      === DIFF HUNK ===
            <div tabindex="0" style="outline: none;">
      +|      <div>
                <div> Has hook </div>
              </div>
      +|    </div>
          </body>
      |}];
    (* This is particularly sad; if any ancestor gets a new previous sibling, hook [on_mounts]
     will run again.*)
    inject Parent_has_prev_sibling;
    one_frame_with_raf handle;
    Handle.print_dom_diff ~context:1 handle;
    [%expect
      {|
      on_mount
      init
      === DIFF HUNK ===
            <div tabindex="0" style="outline: none;">
      +|      hi
              <div>
      |}];
    Handle.run_request_animation_frame_tasks handle;
    [%expect {| on_mount |}]
  ;;
end
