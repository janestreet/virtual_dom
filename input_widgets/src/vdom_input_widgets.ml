open Core_kernel
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

module Normalizing_hook = struct
  module Unsafe = Js_of_ocaml.Js.Unsafe
  open Js_of_ocaml
  open Js_of_ocaml.Dom_html

  let id : Dom.event_listener_id Type_equal.Id.t =
    Type_equal.Id.create ~name:"normalizing-hook event-listener-id" (fun _ ->
      Sexp.Atom "event-listener-id")
  ;;

  let is_active element =
    let document_active_element = Unsafe.get document (Js.string "activeElement") in
    phys_equal element document_active_element
  ;;

  let get_property element name : 'a Js.t = Unsafe.get element (Js.string name)
  let set_property element name value = Unsafe.set element (Js.string name) value

  let install_event_handler element name ~f =
    (* This event handler normalizes the value on the input element on the [change] event.
       For a text entry, this means when the user presses enter, and when the user blurs
       the element. Why don't we simply [to_string] the value in the model? Because for
       some input elements, you can have a change event that fires after the value changes
       but before [Incr_dom] can update the model. For example, this happens when you
       press the up arrow on a number input. This leads to a bug where the value in the
       model swaps back and forth with the value in the element. *)
    let change_handler _ =
      let value = Js.to_string (get_property element name) in
      let normalized = Js.string (f value) in
      set_property element name normalized;
      Js._true
    in
    let change_handler = Dom.handler change_handler in
    addEventListener element Event.change change_handler Js._false
  ;;

  [@@@ocaml.warning "-3"]

  let create name value ~f =
    Attr.Expert.create_stateful_hook
      name
      ~hook:(fun element ->
        if not (is_active element) then set_property element name (Js.string value);
        install_event_handler element name ~f)
      ~unhook:(fun event_handle _ -> removeEventListener event_handle)
      ~id
  ;;
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
      ([ Attr.class_ "widget-dropdown"
       ; Attr.on_change (fun _ value ->
           on_change (Int.of_string value |> List.nth_exn values))
       ]
       |> maybe_disabled ~disabled
       |> add_attrs extra_attrs)
      (List.mapi values ~f:(fun index value ->
         Node.option
           [ Attr.value (Int.to_string index)
           ; Attr.bool_property "selected" (equal value selected)
           ]
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
      extra_attrs
      [ Node.input
          ([ Attr.type_ "checkbox"
           ; Attr.on_click (fun _ev -> on_toggle ())
           ; Attr.bool_property "checked" is_checked
           ]
           |> maybe_disabled ~disabled)
          []
      ; Node.text label
      ]
  ;;

  let simple ?extra_attrs ?disabled ~is_checked ~label ~on_toggle () =
    Node.div
      [ Attr.class_ "checkbox-container" ]
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
    Node.ul
      ([ Attr.classes [ "widget-checklist"; "checkbox-container" ]
       ; Attr.style
           Css_gen.(create ~field:"list-style" ~value:"none" @> margin_left (`Px 0))
       ]
       |> add_attrs extra_attrs)
      (List.map values ~f:(fun item ->
         Node.li
           []
           [ Checkbox.impl
               ~extra_attrs
               ~disabled
               ~is_checked:(is_checked item)
               ~label:(to_string item)
               ~on_toggle:(fun () -> on_toggle item)
               ()
           ]))
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
        (module M : Set with type t = t and type comparator_witness = cmp)
        values
        ~selected
        ~on_change
    =
    let open Js_of_ocaml in
    let attrs =
      [ Attr.create "multiple" ""
      ; Attr.create "size" (Int.to_string (List.length values))
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
          ([ Some (Attr.bool_property "selected" is_selected)
           ; Some
               (Attr.on_click (fun evt ->
                  let was_repeated_click =
                    (not (Js.to_bool evt##.ctrlKey))
                    && Set.equal selected (Set.singleton (module M) value)
                  in
                  match was_repeated_click, repeated_click_behavior with
                  | false, _ | true, No_action -> Event.Ignore
                  | true, Clear_all -> on_change (Set.empty (module M))
                  | true, Select_all -> on_change (Set.of_list (module M) values)))
           ]
           |> List.filter_opt)
          [ Node.text (M.to_string value) ])
    in
    Node.select attrs options
  ;;

  let of_values
        (type t cmp)
        ?extra_attrs
        ?repeated_click_behavior
        ?disabled
        (module M : Set with type t = t and type comparator_witness = cmp)
        values
        ~selected
        ~on_change
    =
    impl
      ?extra_attrs
      ?repeated_click_behavior
      ?disabled
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
        (module M : Enum_set with type t = t and type comparator_witness = cmp)
        ~selected
        ~on_change
    =
    impl
      ?extra_attrs
      ?repeated_click_behavior
      ?disabled
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
    try M.to_string (M.of_string s) with
    | _ -> ""
  ;;

  let maybe_on_return on_return attrs =
    match on_return with
    | None -> attrs
    | Some on_return ->
      Attr.on_keydown (fun ev -> if ev##.keyCode = 13 then on_return () else Event.Ignore)
      :: attrs
  ;;

  let input_node ?(extra_attrs = []) ?(disabled = false) ?(placeholder = "") attrs =
    Node.input
      (attrs
       |> add_attrs [ Attr.placeholder placeholder; Attr.create "spellcheck" "false" ]
       |> maybe_disabled ~disabled
       |> add_attrs extra_attrs)
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
        (module M : Stringable.S with type t = t)
        ~type_attrs
        ~value
        ~on_input
    =
    let value =
      let value = Option.value_map ~f:M.to_string value ~default:"" in
      Normalizing_hook.create "value" value ~f:(normalize (module M))
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
      | _ -> Normalizing_hook.create "value" (V.to_string value) ~f:(normalize (module V))
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

  let time ?extra_attrs ?call_on_input_when ?disabled ?placeholder ~value ~on_input () =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      (module Time_compat.Ofday)
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
      ([ Attr.placeholder placeholder
       ; Call_on_input_when.listener call_on_input_when (fun _ev value -> on_input value)
       ; Normalizing_hook.create "value" value ~f:Fn.id
       ]
       |> maybe_disabled ~disabled
       |> add_attrs extra_attrs)
      []
  ;;
end

module Button = struct
  let with_validation ?(extra_attrs = []) text ~validation ~on_click =
    match validation with
    | Ok result ->
      Node.button
        ([ Attr.on_click (fun _ev -> on_click result); Attr.type_ "button" ]
         |> add_attrs extra_attrs)
        [ Node.text text ]
    | Error reason ->
      Node.button
        ([ Attr.disabled
         ; Attr.type_ "button"
         ; Attr.create "tooltip" reason
         ; Attr.create "tooltip-position" "top"
         ]
         |> add_attrs extra_attrs)
        [ Node.text text ]
  ;;

  let simple ?(extra_attrs = []) ?(disabled = false) text ~on_click =
    Node.button
      ([ Attr.type_ "button"; Attr.on_click (fun _ev -> on_click ()) ]
       |> maybe_disabled ~disabled
       |> add_attrs extra_attrs)
      [ Node.text text ]
  ;;
end
