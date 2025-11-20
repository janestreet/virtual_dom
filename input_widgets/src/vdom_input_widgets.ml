open Core
open Virtual_dom.Vdom
include Vdom_input_widgets_intf

module Merge_behavior = struct
  type t =
    | Merge
    | Legacy_dont_merge
end

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
    (* This is used to avoid marking as invalid a field that hasn't ever been touched by
       the user, to improve UX. *)
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

      (* Format from the browser: yyyy-MM-ddThh:mm. It allows trailing ":ss" or ":ss.SSS". *)
      let of_string s =
        let date, time = String.lsplit2_exn ~on:'T' s in
        let hr, min, sec, ms =
          let time_parts = String.split_on_chars ~on:[ ':'; '.' ] time in
          match List.map ~f:Int.of_string time_parts with
          | [ hr; min ] -> hr, min, None, None
          | [ hr; min; sec ] -> hr, min, Some sec, None
          | [ hr; min; sec; ms ] -> hr, min, Some sec, Some ms
          | _ -> failwith "Invalid time format. Expected hh:mm[:ss[.ms]]"
        in
        let date = Date.of_string date in
        let ofday = Time_ns.Ofday.create ~hr ~min ?sec ?ms () in
        Time_ns.of_date_ofday ~zone date ofday
      ;;

      let to_string t =
        let s = Time_ns.to_string_iso8601_basic ~zone t in
        (*=The browser expect a yyyy-MM-ddThh:mm format and it allows
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
let add_attrs attrs' attrs = [ Attr.many (attrs @ attrs') ]

let merge ~(here : [%call_pos]) behavior l =
  match behavior with
  | Merge_behavior.Merge -> Attr.many l
  | Legacy_dont_merge -> Attr.many_without_merge ~here l
;;

let structural_list
  ?(orientation = `Vertical)
  ?(merge_behavior = Merge_behavior.Merge)
  attrs
  children
  =
  let layout_style =
    match orientation with
    | `Vertical -> Css_gen.(display `Block)
    | `Horizontal -> Css_gen.(display `Inline_block)
  in
  Node.ul
    ~attrs:
      [ (merge merge_behavior)
          ([ Attr.style
               Css_gen.(create ~field:"list-style" ~value:"none" @> margin_left (`Px 0))
           ]
           |> add_attrs attrs)
      ]
    (List.map children ~f:(fun child ->
       Node.li ~attrs:[ Attr.style layout_style ] [ child ]))
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
  let type_property = Js.string "type"
  let get_value (element : 'a Js.t) : 'b Js.t = Unsafe.get element value_property

  let set_value (element : 'a Js.t) (value : string) =
    Unsafe.set element value_property (Js.string value)
  ;;

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
      Option.iter (f value) ~f:(fun normalized -> set_value element normalized);
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
        ; allow_updates_when_focused : [ `Always | `Never ]
        }

      let sexp_of_t { value; _ } = Sexp.Atom value
      let combine _left right = right
    end

    let init { Input.value; f; allow_updates_when_focused = _ } element =
      set_value element value;
      let event_id = install_event_handler element ~f in
      { State.event_id }
    ;;

    let on_mount = `Do_nothing
    let destroy _input state _element = removeEventListener state.State.event_id

    let set_value_if_necessary ~allow_updates_when_focused ~new_value element =
      let element_value : string = Js.to_string (Unsafe.get element value_property) in
      (* [equal_numerical_value] exists because the JSOO version of [parseFloat] raises if
         the value parsed is NaN. From MDN, the value of a number input can either be a
         number or the empty string, the latter of which parses to NaN.

         Because we only call this function when two OCaml string values are not equal and
         only use the output of [parseFloat] to compare and decide whether we need to
         update the value property, this is fine. We don't have to worry about leaking
         NaNs back into OCaml.

         We could special case the empty string, but using the browser's underlying
         [parseFloat] sounds a bit more resilient to potential browser behaviour changes
         in the future. *)
      let equal_numerical_value : string -> string -> bool =
        let equal = Unsafe.pure_js_expr {|(a, b) => (parseFloat(a) === parseFloat(b))|} in
        fun a b ->
          Js.to_bool
            (Unsafe.fun_call
               equal
               [| Unsafe.inject (Js.string a); Unsafe.inject (Js.string b) |])
      in
      let should_set_value =
        match allow_updates_when_focused with
        | `Never -> if is_active element then `Should_not_update else `Should_update
        | `Always ->
          (* This condition looks complex, but it's mostly interacting with JSOO. We
             SHOULD NOT set the value property of the element only if either:
             1. The element's current value already matches the value being set.
             2. The element is a number input and
                [parseFloat(element.value) !== parseFloat(new_value)]

             The first condition ensures that typing an invalid state into the input
             doesn't cause the input to be cleared.

             The second condition ensures that typing "1e4" into a number input doesn't
             immediately expand to 10000.

             There is one small catch: clearing a form might not work in certain niche
             scenarios. If the form is currently in a state where its value is "", then
             setting "" won't actually trigger a [set_value]. This would happen if the
             user is in an invalid state (e.g. they have typed the month and day but not
             year of a date input) and we attempt to set the empty value.

             In practice, this probably isn't a problem. If people run into this and are
             seriously sad, they can hack around it by setting a non-empty value, followed
             by the empty one. A more principled solution would be to distinguish whether
             the new value is being set from user input or programmatically. *)
          (match String.equal new_value element_value with
           | true -> `Should_not_update
           | false ->
             let element_type : Js.js_string Js.t option =
               Js.Opt.to_option (Unsafe.get element type_property)
             in
             (match element_type with
              | None -> `Should_update
              | Some js_string ->
                (match Js.to_string js_string with
                 | "number" ->
                   (match equal_numerical_value element_value new_value with
                    | true -> `Should_not_update
                    | false -> `Should_update)
                 | _ -> `Should_update)))
      in
      match should_set_value with
      | `Should_update -> set_value element new_value
      | `Should_not_update -> ()
    ;;

    let update
      ~old_input:_
      ~new_input:{ Input.value; f; allow_updates_when_focused }
      state
      element
      =
      set_value_if_necessary ~allow_updates_when_focused ~new_value:value element;
      removeEventListener state.State.event_id;
      state.State.event_id <- install_event_handler element ~f
    ;;
  end

  include Attr.Hooks.Make (M)

  (* [create value ~f] will set the "value" property to [value] if the element is not
     focused and on each change, run the current value through [f] to re-set it. Again,
     this only happens if the element is not focused. If [f] returns [None], no change
     takes place. *)
  let create value ~f ~allow_updates_when_focused =
    Attr.create_hook "value:normalized" (create { value; f; allow_updates_when_focused })
  ;;
end

module Dropdown = struct
  let impl
    ?(extra_attrs = [])
    ?(extra_option_attrs = Fn.const [])
    ?(disabled = false)
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    values
    ~equal
    ~selected
    ~to_string
    ~on_change
    =
    Node.select
      ?key
      ~attrs:
        [ (merge merge_behavior)
            ([ Attr.class_ "widget-dropdown"
             ; Attr.on_change (fun _ value ->
                 on_change (Int.of_string value |> List.nth_exn values))
             ]
             |> maybe_disabled ~disabled
             |> add_attrs extra_attrs)
        ]
      (List.mapi values ~f:(fun index value ->
         Node.option
           ~attrs:
             ([ Attr.value (Int.to_string index)
              ; Attr.bool_property "selected" (equal value selected)
              ]
              @ extra_option_attrs value)
           [ Node.text (to_string value) ]))
  ;;

  let of_values
    (type t)
    ?extra_attrs
    ?extra_option_attrs
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    (module M : Equal with type t = t)
    values
    ~selected
    ~on_change
    =
    impl
      ?extra_attrs
      ?extra_option_attrs
      ?disabled
      ?key
      values
      ~equal:M.equal
      ~selected
      ~to_string:M.to_string
      ~on_change
      ~merge_behavior
  ;;

  let of_values_opt
    (type t)
    ?extra_attrs
    ?extra_option_attrs
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    ?(placeholder = "")
    ?key
    (module M : Equal with type t = t)
    values
    ~selected
    ~on_change
    =
    let values = None :: List.map values ~f:Option.some in
    let to_string = Option.value_map ~default:placeholder ~f:M.to_string in
    let extra_option_attrs =
      Option.map extra_option_attrs ~f:(fun f -> function
        | None -> []
        | Some value -> f value)
    in
    impl
      ?extra_attrs
      ?extra_option_attrs
      ?disabled
      ?key
      values
      ~equal:[%equal: M.t option]
      ~selected
      ~to_string
      ~on_change
      ~merge_behavior
  ;;

  let of_enum
    (type t)
    ?extra_attrs
    ?extra_option_attrs
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    (module M : Enum with type t = t)
    ~selected
    ~on_change
    =
    impl
      ?extra_attrs
      ?extra_option_attrs
      ?disabled
      ?key
      M.all
      ~equal:M.equal
      ~selected
      ~to_string:M.to_string
      ~on_change
      ~merge_behavior
  ;;

  let of_enum_opt
    (type t)
    ?extra_attrs
    ?extra_option_attrs
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    ?(placeholder = "")
    ?key
    (module M : Enum with type t = t)
    ~selected
    ~on_change
    =
    let values = None :: List.map M.all ~f:Option.some in
    let to_string = Option.value_map ~default:placeholder ~f:M.to_string in
    let extra_option_attrs =
      Option.map extra_option_attrs ~f:(fun f -> function
        | None -> []
        | Some value -> f value)
    in
    impl
      ?extra_attrs
      ?extra_option_attrs
      ?disabled
      ?key
      values
      ~equal:[%equal: M.t option]
      ~selected
      ~to_string
      ~on_change
      ~merge_behavior
  ;;
end

module Selectable_style = struct
  type t =
    | Native
    | Button_like

  let hide_native_inputs =
    Css_gen.(create ~field:"appearance" ~value:"none" @> uniform_margin (`Px 0))
  ;;
end

module Checkbox = struct
  let impl
    ?(extra_attrs = [])
    ?(disabled = false)
    ?key
    ~is_checked
    ~label
    ~on_toggle
    ?(merge_behavior = Merge_behavior.Merge)
    ()
    =
    Node.label
      ?key
      ~attrs:[ (merge merge_behavior) extra_attrs ]
      [ Node.input
          ~attrs:
            ([ Attr.type_ "checkbox"
             ; Attr.on_click (fun _ev -> on_toggle)
             ; Attr.bool_property "checked" is_checked
             ]
             |> maybe_disabled ~disabled)
          ()
      ; Node.text label
      ]
  ;;

  let simple
    ?extra_attrs
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    ~is_checked
    ~label
    ~on_toggle
    ()
    =
    Node.div
      ?key
      ~attrs:[ Attr.class_ "checkbox-container" ]
      [ impl ?extra_attrs ?disabled ~is_checked ~label ~on_toggle ~merge_behavior () ]
  ;;
end

module Checklist = struct
  let impl
    ?(style = Selectable_style.Native)
    ?(extra_container_attrs = [])
    ?(extra_checkbox_attrs = fun ~checked:_ -> [])
    ?(disabled = false)
    ?layout
    values
    ~is_checked
    ~on_toggle
    ~to_string
    ~merge_behavior
    =
    let input_attrs =
      match style with
      | Native -> []
      | Button_like -> [ Attr.style Selectable_style.hide_native_inputs ]
    in
    structural_list
      ~merge_behavior
      ?orientation:layout
      ([ Attr.classes [ "widget-checklist"; "checkbox-container" ] ]
       |> add_attrs extra_container_attrs)
      (List.map values ~f:(fun item ->
         Node.label
           ~attrs:
             [ (merge merge_behavior) (extra_checkbox_attrs ~checked:(is_checked item)) ]
           [ Node.input
               ~attrs:
                 ([ Attr.type_ "checkbox"
                  ; Attr.on_click (fun _ev -> on_toggle item)
                  ; Attr.bool_property "checked" (is_checked item)
                  ]
                  @ input_attrs
                  |> maybe_disabled ~disabled)
               ()
           ; Node.text (to_string item)
           ]))
  ;;

  let of_values
    (type t)
    ?style
    ?extra_container_attrs
    ?extra_checkbox_attrs
    ?disabled
    ?layout
    ?(merge_behavior = Merge_behavior.Merge)
    (module M : Display with type t = t)
    values
    ~is_checked
    ~on_toggle
    =
    impl
      ?style
      ?extra_container_attrs
      ?extra_checkbox_attrs
      ?disabled
      ?layout
      values
      ~is_checked
      ~on_toggle
      ~to_string:M.to_string
      ~merge_behavior
  ;;

  let of_enum
    (type t)
    ?style
    ?extra_container_attrs
    ?extra_checkbox_attrs
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    (module M : Enum with type t = t)
    ~is_checked
    ~on_toggle
    =
    impl
      ?style
      ?extra_container_attrs
      ?extra_checkbox_attrs
      ?disabled
      M.all
      ~is_checked
      ~on_toggle
      ~to_string:M.to_string
      ~merge_behavior
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
    ~merge_behavior
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
          ~attrs:
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
             |> List.filter_opt)
          [ Node.text (M.to_string value) ])
    in
    Node.select ~attrs:[ (merge merge_behavior) attrs ] options
  ;;

  let of_values
    (type t cmp)
    ?extra_attrs
    ?repeated_click_behavior
    ?disabled
    ?size
    ?(merge_behavior = Merge_behavior.Legacy_dont_merge)
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
      ~merge_behavior
  ;;

  let of_enum
    (type t cmp)
    ?extra_attrs
    ?repeated_click_behavior
    ?disabled
    ?size
    ?(merge_behavior = Merge_behavior.Merge)
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
      ~merge_behavior
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

  let input_node
    ?(extra_attrs = [])
    ?(disabled = false)
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    attrs
    =
    Node.input
      ?key
      ~attrs:
        [ (merge merge_behavior)
            (attrs
             |> add_attrs
                  [ Option.value_map placeholder ~f:Attr.placeholder ~default:Attr.empty
                  ; Attr.create "spellcheck" "false"
                  ]
             |> maybe_disabled ~disabled
             |> add_attrs extra_attrs)
        ]
      ()
  ;;

  let raw
    ?extra_attrs
    ?disabled
    ?placeholder
    ?on_return
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    ~value
    ~on_input
    ()
    =
    [ Attr.string_property "value" value; Attr.on_input (fun _ev -> on_input) ]
    |> maybe_on_return on_return
    |> input_node ?extra_attrs ?disabled ?placeholder ~merge_behavior ?key
  ;;

  let stringable_input_opt
    (type t)
    ?extra_attrs
    ?(call_on_input_when = Call_on_input_when.Text_changed)
    ?disabled
    ?placeholder
    ?(should_normalize = true)
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    (module M : Stringable.S with type t = t)
    ~type_attrs
    ~value
    ~on_input
    =
    let value =
      let value = Option.value_map ~f:M.to_string value ~default:"" in
      if should_normalize
      then
        Value_normalizing_hook.create
          value
          ~f:(normalize (module M))
          ~allow_updates_when_focused
      else Value_normalizing_hook.create value ~f:(const None) ~allow_updates_when_focused
    in
    [ Call_on_input_when.listener call_on_input_when (fun _ev -> function
        | "" -> on_input None
        | s -> on_input (Option.try_with (fun () -> M.of_string s)))
    ; value
    ]
    |> add_attrs type_attrs
    |> input_node ?extra_attrs ?disabled ?placeholder ~merge_behavior ?key
  ;;

  let of_stringable
    (type t)
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    (module M : Stringable.S with type t = t)
    ~value
    ~on_input
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      ?key
      (module M)
      ~type_attrs:[ Attr.type_ "text" ]
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let validated
    (type t)
    ?extra_attrs
    ?(call_on_input_when = Call_on_input_when.Text_changed)
    ?disabled
    ?placeholder
    ?on_return
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    (module M : Stringable.S with type t = t)
    ~value
    ~on_input
    =
    let (module V) = Validated.lift (module M) in
    let value_attr =
      match (value : V.t) with
      | Initial -> Attr.string_property "value" ""
      | _ ->
        Value_normalizing_hook.create
          (V.to_string value)
          ~f:(normalize (module V))
          ~allow_updates_when_focused
    in
    [ Call_on_input_when.listener call_on_input_when (fun _ev s ->
        on_input (V.of_string s))
    ; value_attr
    ; Attr.type_ "text"
    ]
    |> maybe_on_return on_return
    |> maybe_invalid value
    |> input_node ?extra_attrs ?disabled ?placeholder ~merge_behavior ?key
  ;;

  let text
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    ~value
    ~on_input
    ()
    =
    of_stringable
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      ?key
      (module String)
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let password
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    ~value
    ~on_input
    ()
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      ?key
      (module String)
      ~type_attrs:[ Attr.type_ "password" ]
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let number
    (type t)
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
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
      ?key
      (module M)
      ~type_attrs:[ Attr.type_ "number"; Attr.create_float "step" step ]
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let range
    (type t)
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
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
      ?key
      (module M)
      ~type_attrs:[ Attr.type_ "range"; Attr.create_float "step" step ]
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let time
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    ~value
    ~on_input
    ()
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      ?key
      (module Time_compat.Ofday)
      ~should_normalize:false
      ~type_attrs:[ Attr.type_ "time" ]
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let date
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    ~value
    ~on_input
    ()
    =
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      ?key
      (module Date)
      ~should_normalize:false
      ~type_attrs:[ Attr.type_ "date" ]
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused
  ;;

  let datetime_local
    ?extra_attrs
    ?call_on_input_when
    ?disabled
    ?placeholder
    ?utc_offset
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = (`Never : [ `Never ]))
    ?key
    ~value
    ~on_input
    ()
    =
    let hours =
      (* In tests, we use UTC as the local timezone (i.e. offset 0). This prevents changes
         between test output around daylight savings time, and if [utc_offset] isn't
         supplied, then callers should be indifferent to what offset gets used here. *)
      match Core.am_running_test || Ppx_inline_test_lib.am_running with
      | true -> 0
      | false ->
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
    let (module Zoned_time) = Time_compat.zoned (Time_float.Zone.of_utc_offset ~hours) in
    stringable_input_opt
      ?extra_attrs
      ?call_on_input_when
      ?disabled
      ?placeholder
      ?key
      (module Zoned_time)
      ~type_attrs:[ Attr.type_ "datetime-local" ]
      ~should_normalize:false
      ~value
      ~on_input
      ~merge_behavior
      ~allow_updates_when_focused:(allow_updates_when_focused :> [ `Always | `Never ])
  ;;

  let text_area
    ?(extra_attrs = [])
    ?(call_on_input_when = Call_on_input_when.Text_changed)
    ?(disabled = false)
    ?(placeholder = "")
    ?(merge_behavior = Merge_behavior.Merge)
    ?(allow_updates_when_focused = `Always)
    ?key
    ~value
    ~on_input
    ()
    =
    Node.textarea
      ?key
      ~attrs:
        [ (merge merge_behavior)
            ([ Attr.placeholder placeholder
             ; Call_on_input_when.listener call_on_input_when (fun _ev value ->
                 on_input value)
             ; Value_normalizing_hook.create
                 value
                 ~f:Option.return
                 ~allow_updates_when_focused
             ]
             |> maybe_disabled ~disabled
             |> add_attrs extra_attrs)
        ]
      []
  ;;

  (* According to
     https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color#Value the value
     must be set in hex format and will always comes back in hex format. *)
  let color_picker
    ?(extra_attr = Attr.empty)
    ?(call_on_input_when = Call_on_input_when.Text_changed)
    ?disabled
    ?(merge_behavior = Merge_behavior.Merge)
    ?key
    ~value
    ~on_input
    ()
    =
    let (`Hex value_) = value in
    [ Attr.(type_ "color" @ value_prop value_ @ extra_attr)
    ; Call_on_input_when.listener call_on_input_when (fun _ev s -> on_input (`Hex s))
    ]
    |> input_node ?disabled ~merge_behavior ?key
  ;;
end

module Button = struct
  let with_validation
    ?(extra_attrs = [])
    ?(merge_behavior = Merge_behavior.Merge)
    text
    ~validation
    ~on_click
    =
    match validation with
    | Ok result ->
      Node.button
        ~attrs:
          [ (merge merge_behavior)
              ([ Attr.on_click (fun _ev -> on_click result); Attr.type_ "button" ]
               |> add_attrs extra_attrs)
          ]
        [ Node.text text ]
    | Error reason ->
      Node.button
        ~attrs:
          [ (merge merge_behavior)
              ([ Attr.disabled
               ; Attr.type_ "button"
               ; Attr.create "tooltip" reason
               ; Attr.create "tooltip-position" "top"
               ]
               |> add_attrs extra_attrs)
          ]
        [ Node.text text ]
  ;;

  let simple
    ?(extra_attrs = [])
    ?(disabled = false)
    ?(merge_behavior = Merge_behavior.Merge)
    text
    ~on_click
    =
    Node.button
      ~attrs:
        [ (merge merge_behavior)
            ([ Attr.type_ "button"; Attr.on_click (fun _ev -> on_click ()) ]
             |> maybe_disabled ~disabled
             |> add_attrs extra_attrs)
        ]
      [ Node.text text ]
  ;;
end

module Radio_buttons = struct
  let impl
    ?(extra_container_attrs = [])
    ?(extra_button_attrs = fun ~checked:_ -> [])
    ?(disabled = false)
    ?(style : Selectable_style.t = Native)
    ?(merge_behavior = Merge_behavior.Merge)
    ~orientation
    ~name
    ~on_click
    ~selected
    ~to_string
    ~equal
    values
    =
    let input_attrs =
      match style with
      | Native -> []
      | Button_like -> [ Attr.style Selectable_style.hide_native_inputs ]
    in
    structural_list
      ~merge_behavior
      ~orientation
      ([ Attr.classes [ "widget-radio-buttons"; "radio-button-container" ] ]
       |> add_attrs extra_container_attrs)
      (List.map values ~f:(fun item ->
         let checked = Option.value_map selected ~default:false ~f:(equal item) in
         Node.label
           ~attrs:[ (merge merge_behavior) (extra_button_attrs ~checked) ]
           [ Node.input
               ~attrs:
                 ([ Attr.type_ "radio"
                  ; Attr.name name
                  ; Attr.classes [ "radio-button" ]
                  ; Attr.on_click (fun _ev -> on_click item)
                  ; Attr.bool_property "checked" checked
                  ]
                  @ input_attrs
                  |> maybe_disabled ~disabled)
               ()
           ; Node.text (to_string item)
           ]))
  ;;

  let of_values
    (type t)
    ?extra_container_attrs
    ?extra_button_attrs
    ?disabled
    ?style
    ?(merge_behavior = Merge_behavior.Merge)
    (module E : Equal with type t = t)
    ~name
    ~on_click
    ~selected
    values
    =
    impl
      ?extra_container_attrs
      ?extra_button_attrs
      ?disabled
      ?style
      ~orientation:`Vertical
      ~name
      ~on_click
      ~selected
      ~to_string:E.to_string
      ~equal:E.equal
      ~merge_behavior
      values
  ;;

  let of_values_horizontal
    (type t)
    ?extra_container_attrs
    ?extra_button_attrs
    ?disabled
    ?style
    ?(merge_behavior = Merge_behavior.Merge)
    (module E : Equal with type t = t)
    ~name
    ~on_click
    ~selected
    values
    =
    impl
      ?extra_container_attrs
      ?extra_button_attrs
      ?disabled
      ?style
      ~orientation:`Horizontal
      ~name
      ~on_click
      ~selected
      ~to_string:E.to_string
      ~equal:E.equal
      ~merge_behavior
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

  let list
    ?(extra_attrs = [])
    ?accept
    ?(merge_behavior = Merge_behavior.Merge)
    ~on_input
    ()
    =
    Node.input
      ~attrs:
        [ (merge merge_behavior)
            ([ Attr.type_ "file"
             ; accept_attrs accept
             ; Attr.create "multiple" ""
             ; Attr.on_file_input (fun _ev file_list ->
                 let files =
                   List.init file_list##.length ~f:(fun i ->
                     file_list##item i
                     |> Js.Opt.to_option
                     |> Option.value_exn ~message:[%string "couldn't get file %{i#Int}"])
                 in
                 on_input files)
             ]
             |> add_attrs extra_attrs)
        ]
      ()
  ;;

  let single
    ?(extra_attrs = [])
    ?accept
    ?(merge_behavior = Merge_behavior.Merge)
    ~on_input
    ()
    =
    Node.input
      ~attrs:
        [ (merge merge_behavior)
            ([ Attr.type_ "file"
             ; accept_attrs accept
             ; Attr.on_file_input (fun _ev file_list ->
                 let file = file_list##item 0 |> Js.Opt.to_option in
                 on_input file)
             ]
             |> add_attrs extra_attrs)
        ]
      ()
  ;;
end
