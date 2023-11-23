open Core
open Incr_dom
open Vdom_input_widgets
open Incr.Let_syntax
open Vdom

module Example = struct
  module T = struct
    type t =
      | Foo
      | Bar
      | Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz
    [@@deriving sexp, compare, enumerate]
  end

  include T
  include Sexpable.To_stringable (T)
  include Comparable.Make (T)
end

module Time_ns = struct
  include Time_ns
  include Time_ns.Alternate_sexp
end

module App = struct
  module Model = struct
    type t =
      { example : Example.t
      ; example_opt : Example.t option
      ; examples : Example.Set.t
      ; string_opt : string option
      ; int_opt : int option
      ; float_opt : float option
      ; time_ns_opt : Time_ns.t option
      ; time_of_day_opt : Time_ns.Ofday.t option
      ; time_span : Time_ns.Span.t Validated.t
      ; selected_files : string list
      ; color : [ `Hex of string ]
      }
    [@@deriving sexp_of]

    let cutoff = phys_equal

    let toggle t example =
      let examples =
        if Set.mem t.examples example
        then Set.remove t.examples example
        else Set.add t.examples example
      in
      { t with examples }
    ;;
  end

  module State = struct
    type t = unit
  end

  module Action = struct
    type t = Set of Model.t [@@deriving sexp_of]
  end

  let initial_model : Model.t =
    { example = Foo
    ; example_opt = None
    ; examples = Example.Set.empty
    ; string_opt = None
    ; int_opt = None
    ; float_opt = None
    ; time_ns_opt = None
    ; time_of_day_opt = None
    ; time_span = Validated.initial_empty
    ; selected_files = []
    ; color = `Hex "#0000ff"
    }
  ;;

  let on_startup ~schedule_action:_ _model = Async_kernel.Deferred.unit

  let barebones_button_like ~checked =
    if checked
    then
      [ Vdom.Attr.style
          Css_gen.(
            border ~width:(`Px 1) ~color:(`Hex "#D0D0D0") ~style:`Solid ()
            @> background_color (`Hex "#404040")
            @> color (`Hex "#F7F7F7"))
      ]
    else
      [ Vdom.Attr.style
          Css_gen.(
            border ~width:(`Px 1) ~color:(`Hex "#D0D0D0") ~style:`Solid ()
            @> background_color (`Hex "#EFEFEF"))
      ]
  ;;

  let view (model : Model.t Incr.t) ~(inject : Action.t -> unit Effect.t) =
    let%map model = model in
    let widgets =
      [ ( "Dropdown.of_values"
        , Dropdown.of_values
            ~merge_behavior:Legacy_dont_merge
            (module Example)
            [ Foo; Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz ]
            ~selected:model.example
            ~on_change:(fun example -> inject (Set { model with example })) )
      ; ( "Dropdown.of_values_opt"
        , Dropdown.of_values_opt
            (module Example)
            [ Foo; Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz ]
            ~selected:model.example_opt
            ~on_change:(fun example_opt -> inject (Set { model with example_opt }))
            ~merge_behavior:Legacy_dont_merge )
      ; ( "Dropdown.of_enum"
        , Dropdown.of_enum
            (module Example)
            ~selected:model.example
            ~on_change:(fun example -> inject (Set { model with example }))
            ~merge_behavior:Legacy_dont_merge )
      ; ( "Dropdown.of_enum_opt"
        , Dropdown.of_enum_opt
            (module Example)
            ~selected:model.example_opt
            ~on_change:(fun example_opt -> inject (Set { model with example_opt }))
            ~merge_behavior:Legacy_dont_merge )
      ; ( "Radio_buttons.of_values"
        , Radio_buttons.of_values
            (module Example)
            ~name:"radio_buttons_of_values"
            ~on_click:(fun example -> inject (Set { model with example }))
            ~selected:(Some model.example)
            [ Foo; Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz ]
            ~merge_behavior:Legacy_dont_merge )
      ; ( "Radio_buttons.of_values_horizontal"
        , Radio_buttons.of_values_horizontal
            (module Example)
            ~name:"radio_buttons_of_values_horizontal"
            ~on_click:(fun example -> inject (Set { model with example }))
            ~selected:(Some model.example)
            [ Foo; Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz ]
            ~merge_behavior:Legacy_dont_merge )
      ; ( "Radio_buttons.of_values_horizontal with button-like style"
        , Radio_buttons.of_values_horizontal
            (module Example)
            ~style:Selectable_style.Button_like
            ~extra_button_attrs:barebones_button_like
            ~name:"radio_buttons_of_values_horizontal_button_like"
            ~on_click:(fun example -> inject (Set { model with example }))
            ~selected:(Some model.example)
            ~merge_behavior:Legacy_dont_merge
            [ Foo; Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz ] )
      ; ( "Checkbox.checkbox"
        , Checkbox.simple
            ~is_checked:(Set.mem model.examples Foo)
            ~label:"Foo"
            ~on_toggle:(inject (Set (Model.toggle model Foo)))
            ~merge_behavior:Legacy_dont_merge
            () )
      ; ( "Checklist.of_values"
        , Checklist.of_values
            (module Example)
            [ Foo; Bar ]
            ~is_checked:(Set.mem model.examples)
            ~on_toggle:(fun elt -> inject (Set (Model.toggle model elt)))
            ~merge_behavior:Legacy_dont_merge )
      ; ( "Checklist.of_values with button-like style"
        , Checklist.of_values
            ~merge_behavior:Legacy_dont_merge
            (module Example)
            [ Foo; Bar ]
            ~style:Selectable_style.Button_like
            ~extra_checkbox_attrs:barebones_button_like
            ~extra_container_attrs:[ Vdom.Attr.style (Css_gen.flex_container ()) ]
            ~is_checked:(Set.mem model.examples)
            ~on_toggle:(fun elt -> inject (Set (Model.toggle model elt))) )
      ; ( "Checklist.of_enum"
        , Checklist.of_enum
            ~merge_behavior:Legacy_dont_merge
            (module Example)
            ~is_checked:(Set.mem model.examples)
            ~on_toggle:(fun elt -> inject (Set (Model.toggle model elt))) )
      ; ( "Multi_select.of_values"
        , Multi_select.of_values
            ~merge_behavior:Legacy_dont_merge
            (module Example)
            [ Foo; Bar ]
            ~selected:model.examples
            ~on_change:(fun examples -> inject (Set { model with examples })) )
      ; ( "Multi_select.of_enum ~repeated_click_behavior:Select_all"
        , Multi_select.of_enum
            ~merge_behavior:Legacy_dont_merge
            (module Example)
            ~repeated_click_behavior:Select_all
            ~selected:model.examples
            ~on_change:(fun examples -> inject (Set { model with examples })) )
      ; ( "Entry.of_stringable"
        , Entry.of_stringable
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            (module Example)
            ~value:model.example_opt
            ~on_input:(fun example_opt -> inject (Set { model with example_opt })) )
      ; ( "Entry.validated (Time_ns.Span.t, press Enter = clear)"
        , Entry.validated
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            (module Time_ns.Span)
            ~value:model.time_span
            ~on_input:(fun time_span ->
              inject
                (Set { model with time_span = Validated.update model.time_span time_span }))
            ~on_return:(fun () ->
              inject (Set { model with time_span = Validated.initial_empty })) )
      ; ( "Entry.raw (press Enter = clear)"
        , Entry.raw
            ~merge_behavior:Legacy_dont_merge
            ~value:(Option.value model.string_opt ~default:"")
            ~on_input:(fun string -> inject (Set { model with string_opt = Some string }))
            ~on_return:(fun () -> inject (Set { model with string_opt = None }))
            () )
      ; ( "Entry.text"
        , Entry.text
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            ()
            ~value:model.string_opt
            ~on_input:(fun string_opt -> inject (Set { model with string_opt })) )
      ; ( "Entry.password"
        , Entry.password
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            ()
            ~value:model.string_opt
            ~on_input:(fun string_opt -> inject (Set { model with string_opt })) )
      ; ( "Entry.number (int)"
        , Entry.number
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            (module Int)
            ~step:1.0
            ~value:model.int_opt
            ~on_input:(fun int_opt -> inject (Set { model with int_opt })) )
      ; ( "Entry.number (decimal)"
        , Entry.number
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            (module Decimal)
            ~step:1.0
            ~value:model.float_opt
            ~on_input:(fun float_opt -> inject (Set { model with float_opt })) )
      ; ( "Entry.range (int)"
        , Entry.range
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            (module Int)
            ~step:1.0
            ~value:model.int_opt
            ~on_input:(fun int_opt -> inject (Set { model with int_opt })) )
      ; ( "Entry.range (decimal)"
        , Entry.range
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            (module Decimal)
            ~step:0.1
            ~value:model.float_opt
            ~on_input:(fun float_opt -> inject (Set { model with float_opt })) )
      ; ( "Entry.datetime_local"
        , Entry.datetime_local
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            ()
            ~value:model.time_ns_opt
            ~on_input:(fun time_ns_opt -> inject (Set { model with time_ns_opt })) )
      ; ( "Entry.time (of_day)"
        , Entry.time
            ~allow_updates_when_focused:`Never
            ~merge_behavior:Legacy_dont_merge
            ()
            ~value:model.time_of_day_opt
            ~on_input:(fun time_of_day_opt -> inject (Set { model with time_of_day_opt }))
        )
      ; ( "Entry.color_picker"
        , Entry.color_picker
            ~merge_behavior:Legacy_dont_merge
            ()
            ~value:model.color
            ~on_input:(fun color -> inject (Set { model with color })) )
      ; ( "Button.reset_date"
        , Button.simple
            ~merge_behavior:Legacy_dont_merge
            "Set date to now"
            ~on_click:(fun () ->
            inject (Set { model with time_ns_opt = Some (Time_ns.now ()) })) )
      ; ( "Button.submit"
        , Button.with_validation
            ~merge_behavior:Legacy_dont_merge
            "Submit"
            ~validation:
              (Result.of_option
                 model.string_opt
                 ~error:"You must type something in [Entry.text]")
            ~on_click:(fun _input -> inject (Set { model with string_opt = None })) )
      ; ( "File_select.single"
        , File_select.single
            ~merge_behavior:Legacy_dont_merge
            ~accept:[ `Extension ".png"; `Mimetype "image/jpeg" ]
            ~on_input:(fun file ->
              let selected_files =
                Option.map file ~f:(fun file -> Js_of_ocaml.Js.to_string file##.name)
                |> Option.to_list
              in
              inject (Set { model with selected_files }))
            () )
      ; ( "File_select.list"
        , File_select.list
            ~merge_behavior:Legacy_dont_merge
            ~on_input:(fun files ->
              let selected_files =
                List.map files ~f:(fun file -> Js_of_ocaml.Js.to_string file##.name)
              in
              inject (Set { model with selected_files }))
            () )
      ]
    in
    let title text =
      Node.h2
        ~attrs:[ Attr.style (Css_gen.font_family [ "monospace" ]) ]
        [ Node.text text ]
    in
    Node.div
      (List.map widgets ~f:(fun (name, node) -> Node.section [ title name; node ])
       @ [ Node.section
             [ title "Model"; Node.pre [ Node.text (sprintf !"%{sexp:Model.t}" model) ] ]
         ])
  ;;

  let create model ~old_model:_ ~inject =
    let%map model = model
    and view = view model ~inject
    and apply_action =
      let%map _model = model in
      fun (Action.Set model') _state ~schedule_action:_ -> model'
    in
    Component.create ~apply_action model view
  ;;
end

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:App.initial_model
;;
