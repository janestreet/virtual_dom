open Core
open Virtual_dom.Vdom
include Vdom_input_widgets_intf

module Decimal = struct
  type t = float

  let invariant t =
    if not (Float.is_finite t)
    then failwithf "Cannot represent non-finite float as decimal: %f" t ()
  ;;

  let of_string s =
    let t = Float.of_string s in
    invariant t;
    t
  ;;

  let to_string t =
    invariant t;
    sprintf "%.12g" t
  ;;
end

module Validated = struct
  type 'a t =
    | Initial
    (* This is used to avoid marking as invalid a field that hasn't ever been
       touched by the user, to improve UX. *)
    | Valid of
        { input : string option
        ; value : 'a
        }
    | Invalid of
        { input : string
        ; last_valid : 'a option
        ; error : string
        }
  [@@deriving equal, sexp, bin_io, compare]

  type 'a update = 'a t [@@deriving equal, sexp, bin_io, compare]

  let lift (type a) (module M : Stringable.S with type t = a) =
    (module struct
      type nonrec t = a t

      let to_string = function
        | Initial -> ""
        | Invalid { input; last_valid = _; error = _ } -> input
        | Valid { input; value } ->
          (match input with
           | Some input -> input
           | None -> M.to_string value)
      ;;

      let of_string s =
        try Valid { input = Some s; value = M.of_string s } with
        | exn -> Invalid { input = s; last_valid = None; error = Exn.to_string exn }
      ;;
    end : Stringable.S
      with type t = a t)
  ;;

  let initial_empty = Initial
  let return value = Valid { input = None; value }

  let get_current = function
    | Valid { input = _; value } -> Some value
    | Invalid _ | Initial -> None
  ;;

  let get_last = function
    | Valid { input = _; value } -> Some value
    | Invalid { input = _; last_valid; error = _ } -> last_valid
    | Initial -> None
  ;;

  let get_error = function
    | Initial | Valid _ -> None
    | Invalid { input = _; last_valid = _; error } -> Some error
  ;;

  let is_invalid = function
    | Invalid _ -> true
    | Valid _ | Initial -> false
  ;;

  let is_initial_empty = function
    | Initial -> true
    | _ -> false
  ;;

  let update old new_ =
    match old, new_ with
    | Initial, _ -> new_
    | _, Valid _ -> new_
    | Valid { input = _; value = old }, Invalid { input; last_valid = _; error } ->
      Invalid { input; last_valid = Some old; error }
    | ( Invalid { input = _; last_valid; error = _ }
      , Invalid { input; last_valid = None; error } ) ->
      Invalid { input; last_valid; error }
    | Invalid _, Invalid { input = _; last_valid = Some _; error = _ } -> new_
    | _, Initial -> old
  ;;
end

let maybe_invalid validated attrs =
  if Validated.is_invalid validated
  then Attr.create "aria-invalid" "true" :: attrs
  else attrs
;;

module Time_compat = struct
  module Ofday = struct
    type t = Time_ns.Ofday.t

    let of_string = Time_ns.Ofday.of_string

    (* The browser expects a HH:mm format with optional trailing ":ss" or ":ss.SSS";
       [Time_ns.Ofday.to_string] provides precision in nanoseconds, which is too much. *)
    let to_string = Time_ns.Ofday.to_millisecond_string
  end

  let zoned zone : (module Stringable.S with type t = Time_ns.t) =
    (module struct
      type t = Time_ns.t

      (* Format from the browser: yyyy-MM-ddThh:mm *)
      let of_string s =
        let parts = String.split_on_chars s ~on:[ 'T'; ':' ] in
        let date = List.nth_exn parts 0 |> Date.of_string in
        let hr = List.nth_exn parts 1 |> Int.of_string in
        let min = List.nth_exn parts 2 |> Int.of_string in
        let ofday = Time_ns.Ofday.create ~hr ~min () in
        Time_ns.of_date_ofday ~zone date ofday
      ;;

      let to_string t =
        let s = Time_ns.to_string_iso8601_basic ~zone t in
        (* The browser expect a yyyy-MM-ddThh:mm format and it allows
           trailing ":ss" or ":ss.SSS".

           to_string_iso8601_basic format: 2019-01-30T01:00:00.000000000+01:00

           desired format after cutting:   2019-01-30T01:00:00
        *)
        String.lsplit2_exn ~on:'.' s |> Tuple2.get1
      ;;
    end)
  ;;
end

let maybe_disabled ~disabled attrs = if disabled then Attr.disabled :: attrs else attrs
let add_attrs attrs' attrs = attrs @ attrs' |> Attrs.merge_classes_and_styles

let structural_list ?(orientation = `Vertical) attrs children =
  let layout_style =
    match orientation with
    | `Vertical -> Css_gen.(display `Block)
    | `Horizontal -> Css_gen.(display `Inline_block)
  in
  Node.ul
    ~attr:
      (Attr.many_without_merge
         ([ Attr.style
              Css_gen.(create ~field:"list-style" ~value:"none" @> margin_left (`Px 0))
          ]
          |> add_attrs attrs))
    (List.map children ~f:(fun child -> Node.li ~attr:(Attr.style layout_style) [ child ]))
;;

module Value_normalizing_hook = struct
  module Unsafe = Js_of_ocaml.Js.Unsafe
  open Js_of_ocaml
  open Js_of_ocaml.Dom_html

  let is_active element =
    let document_active_element = Unsafe.get document (Js.string "activeElement") in
    phys_equal element document_active_element
  ;;

  let value_property = Js.string "value"
  let get_value element : 'a Js.t = Unsafe.get element value_property
  let set_value element value = Unsafe.set element value_property value

  let install_event_handler element ~f =
    (* This event handler normalizes the value on the input element on the [change] event.
       For a text entry, this means when the user presses enter, and when the user blurs
       the element. Why don't we simply [to_string] the value in the model? Because for
       some input elements, you can have a change event that fires after the value changes
       but before [Incr_dom] can update the model. For example, this happens when you
       press the up arrow on a number input. This leads to a bug where the value in the
       model swaps back and forth with the value in the element. *)
    let change_handler _ =
      let value = Js.to_string (get_value element) in
      Option.iter (f value) ~f:(fun normalized ->
        set_value element (Js.string normalized));
      Js._true
    in
    let change_handler = Dom.handler change_handler in
    addEventListener element Event.change change_handler Js._false
  ;;

  module M = struct
    module State = struct
      type t = { mutable event_id : event_listener_id }
    end

    module Input = struct
      type t =
        { value : string
        ; f : string -> string option
        }

      let sexp_of_t { value; _ } = Sexp.Atom value
      let combine _left right = right
    end

    let init { Input.value; f } element =
      if not (is_active element) then set_value element (Js.string value);
      let event_id = install_event_handler element ~f in
      { State.event_id }
    ;;

    let on_mount _input _state _element = ()
    let destroy _input { State.event_id } _element = removeEventListener event_id

    let update ~old_input ~new_input state element =
      destroy old_input state element;
      let { State.event_id } = init new_input element in
      state.State.event_id <- event_id
    ;;
  end

  include Attr.Hooks.Make (M)

  (* [create value ~f] will set the "value" property to [value] if the element is not
     focused and on each change, run the current value through [f] to re-set it. Again,
     this only happens if the element is not focused. If [f] returns [None], no change
     takes place. *)
  let create value ~f = Attr.create_hook "value:normalized" (create { value; f })
end

module Dropdown = struct
  let impl
        ?(extra_attrs = [])
        ?(disabled = false)
        values
        ~equal
        ~selected
        ~to_string
        ~on_change
    =
    Node.select
      ~attr:
        (Attr.many_without_merge
           ([ Attr.class_ "widget-dropdown"
            ; Attr.on_change (fun _ value ->
                on_change (Int.of_string value |> List.nth_exn values))
            ]
            |> maybe_disabled ~disabled
            |> add_attrs extra_attrs))
      (List.mapi values ~f:(fun index value ->
         Node.option
           ~attr:
             (Attr.many_without_merge
                [ Attr.value (Int.to_string index)
                ; Attr.bool_property "selected" (equal value selected)
                ])
           [ Node.text (to_string value) ]))
  ;;

  let of_values
        (type t)
        ?extra_attrs
        ?disabled
        (module M : Equal with type t = t)
        values
        ~selected
        ~on_change
    =
    impl
      ?extra_attrs
      ?disabled
      values
      ~equal:M.equal
      ~selected
      ~to_string:M.to_string
      ~on_change
  ;;

  let of_values_opt
        (type t)
        ?extra_attrs
        ?disabled
        (module M : Equal with type t = t)
        values
        ~selected
        ~on_change
    =
    let values = None :: List.map values ~f:Option.some in
    let to_string = Option.value_map ~default:"" ~f:M.to_string in
    impl
      ?extra_attrs
      ?disabled
      values
      ~equal:[%equal: M.t option]
      ~selected
      ~to_string
      ~on_change
  ;;

  let of_enum
        (type t)
        ?extra_attrs
        ?disabled
        (module M : Enum with type t = t)
        ~selected
        ~on_change
    =
    impl
      ?extra_attrs
      ?disabled
      M.all
      ~equal:M.equal
      ~selected
      ~to_string:M.to_string
      ~on_change
  ;;

  let of_enum_opt
        (type t)
        ?extra_attrs
        ?disabled
        (module M : Enum with type t = t)
        ~selected
        ~on_change
    =
    let values = None :: List.map M.all ~f:Option.some in
    let to_string = Option.value_map ~default:"" ~f:M.to_string in
    impl
      ?extra_attrs
      ?disabled
      values
      ~equal:[%equal: M.t option]
      ~selected
      ~to_string
      ~on_change
  ;;
end

module Checkbox = struct
  let impl ?(extra_attrs = []) ?(disabled = false) ~is_checked ~label ~on_toggle () =
    Node.label
      ~attr:(Attr.many_without_merge extra_attrs)
      [ Node.input
          ~attr:
            (Attr.many_without_merge
               ([ Attr.type_ "checkbox"
                ; Attr.on_click (fun _ev -> on_toggle ())
                ; Attr.bool_property "checked" is_checked
                ]
                |> maybe_disabled ~disabled))
          []
      ; Node.text label
      ]
  ;;

  let simple ?extra_attrs ?disabled ~is_checked ~label ~on_toggle () =
    Node.div
      ~attr:(Attr.class_ "checkbox-container")
      [ impl ?extra_attrs ?disabled ~is_checked ~label ~on_toggle () ]
  ;;
end

module Checklist = struct
  let impl
        ?(extra_attrs = [])
        ?(disabled = false)
        values
        ~is_checked
        ~on_toggle
        ~to_string
    =
    structural_list
      ([ Attr.classes [ "widget-checklist"; "checkbox-container" ] ]
       |> add_attrs extra_attrs)
      (List.map values ~f:(fun item ->
         Checkbox.impl
           ~extra_attrs
           ~disabled
           ~is_checked:(is_checked item)
           ~label:(to_string item)
           ~on_toggle:(fun () -> on_toggle item)
           ()))
  ;;

  let of_values
        (type t)
        ?extra_attrs
        ?disabled
        (module M : Display with type t = t)
        values
        ~is_checked
        ~on_toggle
    =
    impl ?extra_attrs ?disabled values ~is_checked ~on_toggle ~to_string:M.to_string
  ;;

  let of_enum
        (type t)
        ?extra_attrs
        ?disabled
        (module M : Enum with type t = t)
        ~is_checked
        ~on_toggle
    =
    impl ?extra_attrs ?disabled M.all ~is_checked ~on_toggle ~to_string:M.to_string
  ;;
end

module Multi_select = struct
  module Repeated_click_behavior = struct
    type t =
      | No_action
      | Clear_all
      | Select_all
  end

  let impl
        (type t cmp)
        ?(repeated_click_behavior = Repeated_click_behavior.No_action)
        ?(extra_attrs = [])
        ?(disabled = false)
        ?size
        (module M : Set with type t = t and type comparator_witness = cmp)
        values
        ~selected
        ~on_change
    =
    let open Js_of_ocaml in
    let size = Option.value size ~default:(List.length values) in
    let attrs =
      [ Attr.create "multiple" ""
      ; Attr.create "size" (Int.to_string size)
      ; Attr.on_change (fun evt (_ : string) ->
          let target =
            match
              Js.Opt.to_option (Js.Opt.bind evt##.target Dom_html.CoerceTo.select)
            with
            | Some target -> target
            | None ->
              failwith
                "Multi_select [on_change] event fired with a missing target or target \
                 that was not a select element."
          in
          let collection_to_list collection =
            List.init collection##.length ~f:(fun i ->
              Js.Opt.get (collection##item i) (fun () -> assert false))
          in
          let options = collection_to_list target##.options in
          let selected_values =
            List.filter_map (List.zip_exn values options) ~f:(fun (value, option) ->
              Option.some_if (Js.to_bool option##.selected) value)
          in
          on_change (Set.of_list (module M) selected_values))
      ]
      @ extra_attrs
      |> maybe_disabled ~disabled
    in
    let options =
      List.map values ~f:(fun value ->
        let is_selected = Set.mem selected value in
        Node.option
          (* [Attr.bool_property] keeps the state of the option in sync by setting the JS
             property. [Attr.selected] modifies the DOM attribute so that selected options
             can be styled with CSS. [Attr.selected] alone does not update the state
             properly if the model changes, so both are needed. *)
          ~attr:
            (Attr.many_without_merge
               ([ Some (Attr.bool_property "selected" is_selected)
                ; Some
                    (Attr.on_click (fun evt ->
                       let was_repeated_click =
                         (not (Js.to_bool evt##.ctrlKey))
                         && Set.equal selected (Set.singleton (module M) value)
                       in
                       match was_repeated_click, repeated_click_behavior with
                       | false, _ | true, No_action -> Effect.Ignore
                       | true, Clear_all -> on_change (Set.empty (module M))
                       | true, Select_all -> on_change (Set.of_list (module M) values)))
                ]
                |> List.filter_opt))
          [ Node.text (M.to_string value) ])
    in
    Node.select ~attr:(Attr.many_without_merge attrs) options
  ;;

  let of_values
        (type t cmp)
        ?extra_attrs
        ?repeated_click_behavior
        ?disabled
        ?size
        (module M : Set with type t = t and type comparator_witness = cmp)
        values
        ~selected
        ~on_change
    =
    impl
      ?extra_attrs
      ?repeated_click_behavior
      ?disabled
      ?size
      (module M)
      values
      ~selected
      ~on_change
  ;;

  let of_enum
        (type t cmp)
        ?extra_attrs
        ?repeated_click_behavior
        ?disabled
        ?size
        (module M : Enum_set with type t = t and type comparator_witness = cmp)
        ~selected
        ~on_change
    =
    impl
      ?extra_attrs
      ?repeated_click_behavior
      ?disabled
      ?size
      (module M)
      M.all
      ~selected
      ~on_change
  ;;
end

module Entry = struct
  module Call_on_input_when = struct
    type t =
      | Text_changed
      | Enter_key_pressed_or_focus_lost

    let listener = function
      | Text_changed -> Attr.on_input
      | Enter_key_pressed_or_focus_lost -> Attr.on_change
    ;;
  end

  let normalize (module M : Stringable.S) s =
    match M.to_string (M.of_string s) with
    | exception _ -> Some ""
    | v -> Some v
  ;;

  let maybe_on_return on_return attrs =
    match on_return with
    | None -> attrs
    | Some on_return ->
      Attr.on_keydown (fun ev ->
        if ev##.keyCode = 13 then on_return () else Effect.Ignore)
      :: attrs
  ;;

  let input_node ?(extra_attrs = []) ?(disabled = false) ?(placeholder = "") attrs =
    Node.input
      ~attr:
        (Attr.many_without_merge
           (attrs
            |> add_attrs [ Attr.placeholder placeholder; Attr.create "spellcheck" "false" ]
            |> maybe_disabled ~disabled
            |> add_attrs extra_attrs))
      []
  ;;

  let raw ?extra_attrs ?disabled ?placeholder ?on_return ~value ~on_input () =
    [ Attr.string_property "value" value; Attr.on_input (fun _ev -> on_input) ]
    |> maybe_on_return on_return
    |> input_node ?extra_attrs ?disabled ?placeholder
  ;;

  let stringable_input_opt
        (type t)
        ?extra_attrs
        ?(call_on_input_when = Call_on_input_when.Text_changed)
        ?disabled
        ?placeholder
        ?(should_normalize = true)
        (module M : Stringable.S with type t = t)
        ~type_attrs
        ~value
        ~on_input
    =
    let value =
      let value = Option.value_map ~f:M.to_string value ~default:"" in
      if should_normalize
      then Value_normalizing_hook.create value ~f:(normalize (module M))
      else Value_normalizing_hook.create value ~f:(const None)
    in
    [ Call_on_input_when.listener call_on_input_when (fun _ev -> function
        | "" -> on_input None
        | s -> on_input (Option.try_with (fun () -> M.of_string s)))
    ; value
    ]
    |> add_attrs type_attrs
    |> input_node ?extra_attrs ?disabled ?placeholder
  ;;

  let of_stringable
        (type t)
        ?extra_attrs
        ?call_on_input_when
        ?disabled
        ?placeholder
        (module M : Stringable.S with type t = t)
        ~value
        ~on_input
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module M)
      ~type_attrs:[ Attr.type_ "text" ]
      ~value
      ~on_input
  ;;

  let validated
        (type t)
        ?extra_attrs
        ?(call_on_input_when = Call_on_input_when.Text_changed)
        ?disabled
        ?placeholder
        ?on_return
        (module M : Stringable.S with type t = t)
        ~value
        ~on_input
    =
    let (module V) = Validated.lift (module M) in
    let value_attr =
      match (value : V.t) with
      | Initial -> Attr.string_property "value" ""
      | _ -> Value_normalizing_hook.create (V.to_string value) ~f:(normalize (module V))
    in
    [ Call_on_input_when.listener call_on_input_when (fun _ev s ->
        on_input (V.of_string s))
    ; value_attr
    ; Attr.type_ "text"
    ]
    |> maybe_on_return on_return
    |> maybe_invalid value
    |> input_node ?extra_attrs ?disabled ?placeholder
  ;;

  let text ?extra_attrs ?call_on_input_when ?disabled ?placeholder ~value ~on_input () =
    of_stringable
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module String)
      ~value
      ~on_input
  ;;

  let number
        (type t)
        ?extra_attrs
        ?call_on_input_when
        ?disabled
        ?placeholder
        (module M : Stringable.S with type t = t)
        ~value
        ~step
        ~on_input
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module M)
      ~type_attrs:[ Attr.type_ "number"; Attr.create_float "step" step ]
      ~value
      ~on_input
  ;;

  let range
        (type t)
        ?extra_attrs
        ?call_on_input_when
        ?disabled
        ?placeholder
        (module M : Stringable.S with type t = t)
        ~value
        ~step
        ~on_input
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module M)
      ~type_attrs:[ Attr.type_ "range"; Attr.create_float "step" step ]
      ~value
      ~on_input
  ;;

  let time ?extra_attrs ?call_on_input_when ?disabled ?placeholder ~value ~on_input () =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module Time_compat.Ofday)
      ~should_normalize:false
      ~type_attrs:[ Attr.type_ "time" ]
      ~value
      ~on_input
  ;;

  let date ?extra_attrs ?call_on_input_when ?disabled ?placeholder ~value ~on_input () =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module Date)
      ~should_normalize:false
      ~type_attrs:[ Attr.type_ "date" ]
      ~value
      ~on_input
  ;;

  let datetime_local
        ?extra_attrs
        ?call_on_input_when
        ?disabled
        ?placeholder
        ?utc_offset
        ~value
        ~on_input
        ()
    =
    let hours =
      Option.value_map
        utc_offset
        (* getTimezoneOffset returns the time zone difference, in minutes, from current
           locale to UTC. Utc offset is the difference from UTC to current locale which
           is where the minus comes from.

           The minutes have to be converted to hours since that is the format
           Time.Zone.of_utc_offset expects for the utc_offset. *)
        ~default:((new%js Js_of_ocaml.Js.date_now)##getTimezoneOffset / -60)
        ~f:(fun utc_offset -> Time_ns.Span.to_hr utc_offset |> Float.to_int)
    in
    let (module Zoned_time) = Time_compat.zoned (Time.Zone.of_utc_offset ~hours) in
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module Zoned_time)
      ~type_attrs:[ Attr.type_ "datetime-local" ]
      ~should_normalize:false
      ~value
      ~on_input
  ;;

  let text_area
        ?(extra_attrs = [])
        ?(call_on_input_when = Call_on_input_when.Text_changed)
        ?(disabled = false)
        ?(placeholder = "")
        ~value
        ~on_input
        ()
    =
    Node.textarea
      ~attr:
        (Attr.many_without_merge
           ([ Attr.placeholder placeholder
            ; Call_on_input_when.listener call_on_input_when (fun _ev value ->
                on_input value)
            ; Value_normalizing_hook.create value ~f:Option.return
            ]
            |> maybe_disabled ~disabled
            |> add_attrs extra_attrs))
      []
  ;;

  (* According to
     https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color#Value the
     value must be set in hex format and will always comes back in hex format. *)
  let color_picker
        ?(extra_attr = Attr.empty)
        ?(call_on_input_when = Call_on_input_when.Text_changed)
        ?disabled
        ~value
        ~on_input
        ()
    =
    let (`Hex value_) = value in
    [ Attr.(type_ "color" @ value_prop value_ @ extra_attr)
    ; Call_on_input_when.listener call_on_input_when (fun _ev s -> on_input (`Hex s))
    ]
    |> input_node ?disabled
  ;;
end

module Button = struct
  let with_validation ?(extra_attrs = []) text ~validation ~on_click =
    match validation with
    | Ok result ->
      Node.button
        ~attr:
          (Attr.many_without_merge
             ([ Attr.on_click (fun _ev -> on_click result); Attr.type_ "button" ]
              |> add_attrs extra_attrs))
        [ Node.text text ]
    | Error reason ->
      Node.button
        ~attr:
          (Attr.many_without_merge
             ([ Attr.disabled
              ; Attr.type_ "button"
              ; Attr.create "tooltip" reason
              ; Attr.create "tooltip-position" "top"
              ]
              |> add_attrs extra_attrs))
        [ Node.text text ]
  ;;

  let simple ?(extra_attrs = []) ?(disabled = false) text ~on_click =
    Node.button
      ~attr:
        (Attr.many_without_merge
           ([ Attr.type_ "button"; Attr.on_click (fun _ev -> on_click ()) ]
            |> maybe_disabled ~disabled
            |> add_attrs extra_attrs))
      [ Node.text text ]
  ;;
end

module Radio_buttons = struct
  module Style = struct
    type t =
      | Native
      | Button_like of { extra_attrs : checked:bool -> Attr.t list }

    let barebones_button_like =
      Button_like
        { extra_attrs =
            (fun ~checked ->
               if checked
               then
                 [ Attr.style
                     Css_gen.(
                       border ~width:(`Px 1) ~color:(`Hex "#D0D0D0") ~style:`Solid ()
                       @> background_color (`Hex "#404040")
                       @> color (`Hex "#F7F7F7"))
                 ]
               else
                 [ Attr.style
                     Css_gen.(
                       border ~width:(`Px 1) ~color:(`Hex "#D0D0D0") ~style:`Solid ()
                       @> background_color (`Hex "#EFEFEF"))
                 ])
        }
    ;;
  end

  let hide_native_inputs =
    Css_gen.(create ~field:"appearance" ~value:"none" @> uniform_margin (`Px 0))
  ;;

  let impl
        ?(extra_attrs = [])
        ?(disabled = false)
        ?(style : Style.t = Native)
        ~orientation
        ~name
        ~on_click
        ~selected
        ~to_string
        ~equal
        values
    =
    let input_attrs, label_attrs =
      match style with
      | Native -> [], fun ~checked:_ -> []
      | Button_like { extra_attrs } -> [ Attr.style hide_native_inputs ], extra_attrs
    in
    structural_list
      ~orientation
      ([ Attr.classes [ "widget-radio-buttons"; "radio-button-container" ] ]
       |> add_attrs extra_attrs)
      (List.map values ~f:(fun item ->
         let checked = Option.value_map selected ~default:false ~f:(equal item) in
         Node.label
           ~attr:(Attr.many_without_merge (label_attrs ~checked))
           [ Node.input
               ~attr:
                 (Attr.many_without_merge
                    ([ Attr.type_ "radio"
                     ; Attr.name name
                     ; Attr.classes [ "radio-button" ]
                     ; Attr.on_click (fun _ev -> on_click item)
                     ; Attr.bool_property "checked" checked
                     ]
                     @ input_attrs
                     |> maybe_disabled ~disabled))
               []
           ; Node.text (to_string item)
           ]))
  ;;

  let of_values
        (type t)
        ?extra_attrs
        ?disabled
        ?style
        (module E : Equal with type t = t)
        ~name
        ~on_click
        ~selected
        values
    =
    impl
      ?extra_attrs
      ?disabled
      ?style
      ~orientation:`Vertical
      ~name
      ~on_click
      ~selected
      ~to_string:E.to_string
      ~equal:E.equal
      values
  ;;

  let of_values_horizontal
        (type t)
        ?extra_attrs
        ?disabled
        ?style
        (module E : Equal with type t = t)
        ~name
        ~on_click
        ~selected
        values
    =
    impl
      ?extra_attrs
      ?disabled
      ?style
      ~orientation:`Horizontal
      ~name
      ~on_click
      ~selected
      ~to_string:E.to_string
      ~equal:E.equal
      values
  ;;
end

module File_select = struct
  module Js = Js_of_ocaml.Js

  let accept_attrs = function
    | None -> Attr.empty
    | Some accepts ->
      Attr.create
        "accept"
        (List.map accepts ~f:(function
           | `Extension s -> if String.is_prefix s ~prefix:"." then s else "." ^ s
           | `Mimetype s -> s)
         |> String.concat ~sep:",")
  ;;

  let list ?(extra_attrs = []) ?accept ~on_input () =
    Node.input
      ~attr:
        (Attr.many_without_merge
           ([ Attr.type_ "file"
            ; accept_attrs accept
            ; Attr.create "multiple" ""
            ; Attr.on_file_input (fun _ev file_list ->
                let files =
                  List.init file_list##.length ~f:(fun i ->
                    file_list##item i
                    |> Js.Opt.to_option
                    |> Option.value_exn
                         ~message:[%string "couldn't get file %{i#Int}"])
                in
                on_input files)
            ]
            |> add_attrs extra_attrs))
      []
  ;;

  let single ?(extra_attrs = []) ?accept ~on_input () =
    Node.input
      ~attr:
        (Attr.many_without_merge
           ([ Attr.type_ "file"
            ; accept_attrs accept
            ; Attr.on_file_input (fun _ev file_list ->
                let file = file_list##item 0 |> Js.Opt.to_option in
                on_input file)
            ]
            |> add_attrs extra_attrs))
      []
  ;;
end
