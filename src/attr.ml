open! Core
open Js_of_ocaml
module Vdom_raw = Raw

let explicitly_print_locations = ref false

let () =
  (* use the native-javascript implementation of float -> string with a fixed number of
     numbers after the decimal place. *)
  Css_gen.Private.float_to_string_with_fixed := Dom_float.to_string_fixed
;;

(** This has 3 kinds of constructors.
    {v
      - constructors for properties / attributes for which we
        have written first class ocaml representations (so far only Style,
        Class, and Handler)

      - Those which we immediately convert into Js called Raw, which
        in turn has two cases:
        - Property for properties on the DOM
        - Attribute for attributes on the DOM

      - Hooks, which register callbacks on property addition and removal.
    v}

    Generally speaking one should avoid creating a property or attribute for something for
    which we have a first class representation. *)

module Event_handler : sig
  type t

  val create
    :  handler:((#Dom_html.event as 'a) Js.t -> unit Ui_effect.t)
    -> type_id:'a Type_equal.Id.t
    -> t

  val get_function : t -> Js.Unsafe.any
  val combine : t -> t -> t
end = struct
  type t =
    | T :
        { type_id : 'a Type_equal.Id.t
        ; handler : (#Dom_html.event as 'a) Js.t -> unit Ui_effect.t
        ; lowered : (_, (#Dom_html.event as 'a) Js.t) Dom.event_listener Lazy.t
        }
        -> t

  let create ~handler ~type_id =
    let lowered =
      lazy
        (Dom.handler (fun e ->
           Effect.Expert.handle e (handler e);
           Js._true))
    in
    T { handler; lowered; type_id }
  ;;

  let get_function (T { lowered; _ }) = Js.Unsafe.inject (Lazy.force lowered)

  let combine
    (T { type_id = ltid; handler = lhandler; lowered = _ })
    (T { type_id = rtid; handler = rhandler; lowered = _ } as right)
    =
    (* If they are not the same witness, then it is a bug in virtual_dom, since
       we do not expose [on] anymore which means this library can determined the
       [Type_equal.Id] corresponding to each event. virtual_dom maintains the
       invariant that any two events with the same name will produce handlers
       that have the same [Type_equal.Id]. *)
    match Type_equal.Id.same_witness ltid rtid with
    | Some T ->
      create ~type_id:ltid ~handler:(fun value ->
        Effect.sequence_as_sibling (lhandler value) ~unless_stopped:(fun () ->
          rhandler value))
    | None ->
      eprint_s
        [%message
          "BUG!  Type-ids for event handlers differ"
            (ltid : _ Type_equal.Id.t)
            (rtid : _ Type_equal.Id.t)];
      right
  ;;
end

type t =
  | Property of
      { suppress_merge_warnings : bool
      ; name : string
      ; value : Js.Unsafe.any
      ; here : Source_code_position.t
      }
  | Attribute of
      { suppress_merge_warnings : bool
      ; name : string
      ; value : Js.Unsafe.any
      ; here : Source_code_position.t
      }
  | Handler of
      { name : string
      ; handler : Event_handler.t
      }
  | Hook of
      { name : string
      ; hook : Hooks.t
      }
  | Style of Css_gen.t
  | Class of string list
  | Many of t list
  | Lazy of t Lazy.t
  | Many_only_merge_classes_and_styles of
      { attrs : t list
      ; map_styles : Css_gen.t -> Css_gen.t
      ; map_classes : string list -> string list
      ; here : Source_code_position.t
      }
  | Many_without_merge of
      { attrs : t list
      ; here : Source_code_position.t
      }

let create ~here name value =
  Attribute
    { suppress_merge_warnings = false
    ; name
    ; value = Js.Unsafe.inject (Js.string value)
    ; here
    }
;;

let create_float ~here name value =
  Attribute
    { suppress_merge_warnings = false
    ; name
    ; value = Js.Unsafe.inject (Dom_float.to_js_string value)
    ; here
    }
;;

let property ~here name value =
  Property { suppress_merge_warnings = false; name; value; here }
;;

let string_property ~here name value =
  Property
    { suppress_merge_warnings = false
    ; name
    ; value = Js.Unsafe.inject (Js.string value)
    ; here
    }
;;

let bool_property ~here name value =
  Property
    { suppress_merge_warnings = false
    ; name
    ; value = Js.Unsafe.inject (Js.bool value)
    ; here
    }
;;

let suppress_merge_warnings = function
  | Attribute attribute -> Attribute { attribute with suppress_merge_warnings = true }
  | Property property -> Property { property with suppress_merge_warnings = true }
  | t -> t
;;

let create_hook name hook = Hook { name; hook }
let many attrs = Many attrs
let many_without_merge ~(here : [%call_pos]) attrs = Many_without_merge { attrs; here }
let empty = Many []
let lazy_ lazy_t = Lazy lazy_t
let combine left right = Many [ left; right ]
let ( @ ) = combine

let of_opt = function
  | None -> empty
  | Some attr -> attr
;;

module Unmerged_warning_mode = struct
  type t =
    | No_warnings
    | All_warnings
    | Stop_after_quota of int

  let warning_count = ref 0
  let current = ref (Stop_after_quota 100)

  let warn_s ~here s =
    incr warning_count;
    let add_location s =
      match s with
      | Sexp.List x ->
        Sexp.List (List.append x [ [%message (here : Source_code_position.t)] ])
      | Atom s -> List [ Atom s; [%message (here : Source_code_position.t)] ]
    in
    let s =
      (* NOTE: Similar to async, we sometimes intentionally hide the location to avoid
         introducing brittleness. We could revisit this at some point in the future. *)
      let location_might_be_noisy =
        Source_code_position.is_dummy here
        || Dynamic.get Backtrace.elide
        || (not (Backtrace.Exn.am_recording ()))
        || am_running_test
      in
      if !explicitly_print_locations
      then add_location s
      else if location_might_be_noisy
      then s
      else add_location s
    in
    match !current with
    | No_warnings -> ()
    | All_warnings -> eprint_s s
    | Stop_after_quota quota ->
      let warning_count = !warning_count in
      if warning_count <= quota
      then (
        eprint_s s;
        if warning_count = quota
        then
          eprint_s
            [%message
              "WARNING: reached warning message quota; no more messages will be printed"
                (quota : int)])
  ;;

  module For_testing = struct
    let reset_warning_count () = warning_count := 0
  end
end

type merge =
  { styles : Css_gen.t
  ; classes : string list
  ; handlers : Event_handler.t Map.M(String).t
  ; hooks : Hooks.t Map.M(String).t
  }

let combining_map_add map key value ~combine =
  Map.update map key ~f:(function
    | Some existing_value -> combine ~key existing_value value
    | None -> value)
;;

let empty_merge =
  { styles = Css_gen.empty
  ; classes = []
  ; handlers = Map.empty (module String)
  ; hooks = Map.empty (module String)
  }
;;

let to_raw attr =
  let attrs = [ attr ] in
  (* When input elements have their value set to what it already is
     the cursor gets moved to the end of the field even when the user
     is editing in the middle. SoftSetHook (from ./soft-set-hook.js)
     compares before setting, avoiding the problem just like in
     https://github.com/Matt-Esch/virtual-dom/blob/947ecf92b67d25bb693a0f625fa8e90c099887d5/virtual-hyperscript/index.js#L43-L51

     note that Elm's virtual-dom includes a workaround for this so
     if we switch to that the workaround here will be unnecessary.
     https://github.com/elm-lang/virtual-dom/blob/17b30fb7de48672565d6227d33c0176f075786db/src/Native/VirtualDom.js#L434-L439
  *)
  let attrs_obj : Vdom_raw.Attrs.t = Vdom_raw.Attrs.create () in
  (* [take_second_*] is the trivial merge function (i.e. no merge at all); it
     takes two attributes of the same kind, ignores a first, and emits
     a warning if [warn_about_unmerged_attributes] is enabled. *)
  let take_second_styles ~here first second =
    if not (Css_gen.is_empty first)
    then
      Unmerged_warning_mode.warn_s
        ~here
        [%message
          "WARNING: not combining styles" (first : Css_gen.t) (second : Css_gen.t)];
    second
  in
  let take_second_classes ~here first second =
    if not (List.is_empty first)
    then (
      let first = List.sort ~compare:[%compare: string] first in
      let second = List.sort ~compare:[%compare: string] second in
      Unmerged_warning_mode.warn_s
        ~here
        [%message
          "WARNING: not combining classes" (first : string list) (second : string list)]);
    second
  in
  let take_second_handler ~here ~key:name _first second =
    Unmerged_warning_mode.warn_s
      ~here
      [%message "WARNING: not combining handlers" (name : string)];
    second
  in
  let take_second_hook ~here ~key:name _first second =
    Unmerged_warning_mode.warn_s
      ~here
      [%message "WARNING: not combining hooks" (name : string)];
    second
  in
  (* We merge attributes when they are written to the raw attribute object,
     rather than when the user-facing merge functions ([many], [combine], and
     [@]) are called. This strategy is better in both speed and memory usage,
     since it means we do not need to concatenate the list of "unmergeable"
     attributes (Property and Attribute); instead, we can iterate through the
     tree of attributes and eagerly write unmergeable attributes to the
     attribute object as we find them. If two unmergeable attributes have the
     same name, the second will simply overwrite the first, as desired.

     In order to preserve the existing behavior of the [Multi] module (that is,
     it must be possible to merge classes and styles, but not hooks and
     handlers), we introduce the workaround constructor
     [Many_only_merge_classes_and_styles].

     There are thus three cases that each have different merge behaviors:
     - Simple lists - no merging
     - Lists wrapped in a [Many] - merges classes, styles, hooks, and handlers
     - Lists wrapped in a [Many_only_merge_classes_and_styles] - merges classes and styles

     To avoid duplicating the match expression logic, we paremeterize it by the
     merging behavior, since "no merge" really means "merge by taking the
     second one". *)
  let rec merge ~combine_hook ~combine_handler ~combine_styles ~combine_classes acc =
    List.fold ~init:acc ~f:(fun acc attr ->
      match attr with
      | Property { suppress_merge_warnings; name; value; here } ->
        let js_name = Js.string name in
        if Raw.Attrs.has_property attrs_obj js_name && not suppress_merge_warnings
        then
          Unmerged_warning_mode.warn_s
            ~here
            [%message "WARNING: not combining properties" (name : string)];
        (match name with
         | "value" ->
           let softSetHook x : Js.Unsafe.any = Js.Unsafe.global ## SoftSetHook x in
           let value = softSetHook value in
           Vdom_raw.Attrs.set_property attrs_obj (Js.string "value") value
         | _ -> Raw.Attrs.set_property attrs_obj js_name value);
        acc
      | Attribute { suppress_merge_warnings; name; value; here } ->
        let js_name = Js.string name in
        if Raw.Attrs.has_attribute attrs_obj js_name && not suppress_merge_warnings
        then
          Unmerged_warning_mode.warn_s
            ~here
            [%message "WARNING: not combining attributes" (name : string)];
        Raw.Attrs.set_attribute attrs_obj js_name value;
        acc
      | Style new_styles -> { acc with styles = combine_styles acc.styles new_styles }
      | Class new_classes ->
        { acc with classes = combine_classes acc.classes new_classes }
      | Hook { name; hook } ->
        { acc with hooks = combining_map_add acc.hooks name hook ~combine:combine_hook }
      | Handler { name; handler } ->
        { acc with
          handlers = combining_map_add acc.handlers name handler ~combine:combine_handler
        }
      | Lazy (lazy t) ->
        merge ~combine_hook ~combine_handler ~combine_styles ~combine_classes acc [ t ]
      | Many attrs ->
        let sub_merge =
          merge
            ~combine_hook:(fun ~key:_ -> Hooks.combine)
            ~combine_handler:(fun ~key:_ -> Event_handler.combine)
            ~combine_styles:Css_gen.combine
            ~combine_classes:Core.( @ )
            empty_merge
            attrs
        in
        { styles = combine_styles acc.styles sub_merge.styles
        ; classes = combine_classes acc.classes sub_merge.classes
        ; handlers =
            Map.merge_skewed acc.handlers sub_merge.handlers ~combine:combine_handler
        ; hooks = Map.merge_skewed acc.hooks sub_merge.hooks ~combine:combine_hook
        }
      | Many_only_merge_classes_and_styles { attrs; map_styles; map_classes; here } ->
        let sub_merge =
          merge
            ~combine_hook:(take_second_hook ~here)
            ~combine_handler:(take_second_handler ~here)
            ~combine_styles:Css_gen.combine
            ~combine_classes:Core.( @ )
            empty_merge
            attrs
        in
        { styles = map_styles (combine_styles acc.styles sub_merge.styles)
        ; classes = map_classes (combine_classes acc.classes sub_merge.classes)
        ; handlers =
            Map.merge_skewed acc.handlers sub_merge.handlers ~combine:combine_handler
        ; hooks = Map.merge_skewed acc.hooks sub_merge.hooks ~combine:combine_hook
        }
      | Many_without_merge { attrs; here } ->
        let sub_merge =
          merge
            ~combine_hook:(take_second_hook ~here)
            ~combine_handler:(take_second_handler ~here)
            ~combine_styles:(take_second_styles ~here)
            ~combine_classes:(take_second_classes ~here)
            empty_merge
            attrs
        in
        { styles = combine_styles acc.styles sub_merge.styles
        ; classes = combine_classes acc.classes sub_merge.classes
        ; handlers =
            Map.merge_skewed acc.handlers sub_merge.handlers ~combine:combine_handler
        ; hooks = Map.merge_skewed acc.hooks sub_merge.hooks ~combine:combine_hook
        })
  in
  let merge =
    merge
      ~combine_hook:(take_second_hook ~here:[%here])
      ~combine_handler:(take_second_handler ~here:[%here])
      ~combine_styles:(take_second_styles ~here:[%here])
      ~combine_classes:(take_second_classes ~here:[%here])
      empty_merge
      attrs
  in
  Map.iteri merge.hooks ~f:(fun ~key:name ~data:hook ->
    Raw.Attrs.set_property attrs_obj (Js.string name) (Hooks.pack hook));
  Map.iteri merge.handlers ~f:(fun ~key:name ~data:handler ->
    let f = Event_handler.get_function handler in
    Raw.Attrs.set_property attrs_obj (Js.string ("on" ^ name)) f);
  let () =
    if not (Css_gen.is_empty merge.styles)
    then (
      let props = Css_gen.to_string_list merge.styles in
      let obj = Raw.Attrs.create () in
      List.iter props ~f:(fun (k, v) ->
        Raw.Attrs.set_property obj (Js.string k) (Js.Unsafe.inject (Js.string v)));
      Raw.Attrs.set_property attrs_obj (Js.string "style") (obj :> Js.Unsafe.any))
  in
  let () =
    if List.is_empty merge.classes
    then ()
    else
      Raw.Attrs.set_attribute
        attrs_obj
        (Js.string "class")
        (Js.Unsafe.inject (Js.string (String.concat merge.classes ~sep:" ")))
  in
  attrs_obj
;;

let to_raw attr =
  match attr with
  | Many [] | Many_without_merge { attrs = []; here = _ } -> Raw.Attrs.create ()
  | attr -> to_raw attr
;;

let style css = Style css
let class_ classname = Class [ classname ]
let classes' classes = Class (Set.to_list classes)
let classes classnames = Class classnames
let id ~(here : [%call_pos]) s = create ~here "id" s
let name ~(here : [%call_pos]) s = create ~here "name" s
let href ~(here : [%call_pos]) r = create ~here "href" r
let rel ~(here : [%call_pos]) r = create ~here "rel" r
let label ~(here : [%call_pos]) r = create ~here "label" r
let target ~(here : [%call_pos]) s = create ~here "target" s
let checked = create ~here:[%here] "checked" ""
let checked_prop ~(here : [%call_pos]) b = bool_property ~here "checked" b
let selected = create ~here:[%here] "selected" ""
let hidden = create ~here:[%here] "hidden" ""
let readonly = create ~here:[%here] "readonly" ""
let disabled = create ~here:[%here] "disabled" ""
let disabled' b = if b then disabled else empty
let inert = create ~here:[%here] "inert" ""
let placeholder ~(here : [%call_pos]) x = create ~here "placeholder" x
let role ~(here : [%call_pos]) r = create ~here "role" r

let autofocus ~(here : [%call_pos]) = function
  | true -> create ~here "autofocus" ""
  | false -> empty
;;

let allow ~(here : [%call_pos]) x = create ~here "allow" x
let for_ ~(here : [%call_pos]) x = create ~here "for" x
let type_ ~(here : [%call_pos]) x = create ~here "type" x
let value ~(here : [%call_pos]) x = create ~here "value" x
let value_prop ~(here : [%call_pos]) x = string_property ~here "value" x
let tabindex ~(here : [%call_pos]) x = create ~here "tabindex" (Int.to_string x)
let title ~(here : [%call_pos]) x = create ~here "title" x
let alt ~(here : [%call_pos]) x = create ~here "alt" x
let src ~(here : [%call_pos]) x = create ~here "src" x
let open_ = create ~here:[%here] "open" ""
let start ~(here : [%call_pos]) x = create ~here "start" (Int.to_string x)
let min ~(here : [%call_pos]) x = create_float ~here "min" x
let max ~(here : [%call_pos]) x = create_float ~here "max" x
let min_date ~(here : [%call_pos]) x = create ~here "min" (Date.to_string x)
let max_date ~(here : [%call_pos]) x = create ~here "max" (Date.to_string x)

let min_date_time ~(here : [%call_pos]) x =
  create ~here "min" (Date.to_string x ^ "T00:00")
;;

let max_date_time ~(here : [%call_pos]) x =
  create ~here "max" (Date.to_string x ^ "T23:59")
;;

let colspan ~(here : [%call_pos]) x = create ~here "colspan" (Int.to_string x)
let rowspan ~(here : [%call_pos]) x = create ~here "rowspan" (Int.to_string x)
let rows ~(here : [%call_pos]) x = create ~here "rows" (Int.to_string x)
let cols ~(here : [%call_pos]) x = create ~here "cols" (Int.to_string x)
let draggable ~(here : [%call_pos]) b = create ~here "draggable" (Bool.to_string b)

module Type_id = struct
  (* We provide a trivial [to_sexp] function since we only want
     to unify type ids and not convert types to ids *)
  let create name = Type_equal.Id.create ~name (fun _ -> Sexplib.Sexp.List [])
  let (event : Dom_html.event Type_equal.Id.t) = create "event"
  let (focus : Dom_html.focusEvent Type_equal.Id.t) = create "focusEvent"
  let (mouse : Dom_html.mouseEvent Type_equal.Id.t) = create "mouseEvent"
  let (keyboard : Dom_html.keyboardEvent Type_equal.Id.t) = create "keyboardEvent"
  let (submit : Dom_html.submitEvent Type_equal.Id.t) = create "submitEvent"
  let (mousewheel : Dom_html.mousewheelEvent Type_equal.Id.t) = create "mousewheelEvent"
  let (wheel : Dom_html.wheelEvent Type_equal.Id.t) = create "wheelwheelEvent"
  let (clipboard : Dom_html.clipboardEvent Type_equal.Id.t) = create "clipboardEvent"
  let (drag : Dom_html.dragEvent Type_equal.Id.t) = create "dragEvent"
  let (pointer : Dom_html.pointerEvent Type_equal.Id.t) = create "pointerEvent"
  let (animation : Dom_html.animationEvent Type_equal.Id.t) = create "animationEvent"
end

let on type_id name (handler : #Dom_html.event Js.t -> unit Ui_effect.t) : t =
  let handler = Event_handler.create ~handler ~type_id in
  Handler { name; handler }
;;

let on_focus = on Type_id.focus "focus"
let on_blur = on Type_id.focus "blur"
let on_cancel = on Type_id.event "cancel"
let on_click = on Type_id.mouse "click"
let on_close = on Type_id.event "close"
let on_contextmenu = on Type_id.mouse "contextmenu"
let on_double_click = on Type_id.mouse "dblclick"
let on_drag = on Type_id.drag "drag"
let on_dragstart = on Type_id.drag "dragstart"
let on_dragend = on Type_id.drag "dragend"
let on_dragenter = on Type_id.drag "dragenter"
let on_dragleave = on Type_id.drag "dragleave"
let on_dragover = on Type_id.drag "dragover"
let on_drop = on Type_id.drag "drop"
let on_mousemove = on Type_id.mouse "mousemove"
let on_mouseup = on Type_id.mouse "mouseup"
let on_mousedown = on Type_id.mouse "mousedown"
let on_mouseenter = on Type_id.mouse "mouseenter"
let on_mouseleave = on Type_id.mouse "mouseleave"
let on_mouseover = on Type_id.mouse "mouseover"
let on_mouseout = on Type_id.mouse "mouseout"
let on_keyup = on Type_id.keyboard "keyup"
let on_keypress = on Type_id.keyboard "keypress"
let on_keydown = on Type_id.keyboard "keydown"
let on_scroll = on Type_id.event "scroll"
let on_load = on Type_id.event "load"
let on_error = on Type_id.event "error"
let on_submit = on Type_id.submit "submit"
let on_toggle = on Type_id.event "toggle"
let on_pointerdown = on Type_id.pointer "pointerdown"
let on_pointerup = on Type_id.pointer "pointerup"
let on_pointermove = on Type_id.pointer "pointermove"
let on_pointerenter = on Type_id.pointer "pointerenter"
let on_pointerleave = on Type_id.pointer "pointerleave"
let on_mousewheel = on Type_id.mousewheel "mousewheel"
let on_wheel = on Type_id.wheel "wheel"
let on_copy = on Type_id.clipboard "copy"
let on_cut = on Type_id.clipboard "cut"
let on_paste = on Type_id.clipboard "paste"
let on_reset = on Type_id.event "reset"
let on_animationend = on Type_id.animation "animationend"
let const_ignore _ = Effect.Ignore

class type value_element = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

type value_coercion = Dom_html.element Js.t -> value_element Js.t Js.opt

let run_coercion coercion target prev =
  match prev with
  | Some _ -> prev
  | None -> Js.Opt.to_option (coercion target)
;;

let coerce_value_element target =
  let open Dom_html.CoerceTo in
  None
  |> run_coercion (input :> value_coercion) target
  |> run_coercion (select :> value_coercion) target
  |> run_coercion (textarea :> value_coercion) target
;;

let on_input_event type_id event handler =
  on type_id event (fun ev ->
    Js.Opt.case ev##.target const_ignore (fun target ->
      Option.value_map
        (coerce_value_element target)
        ~default:Effect.Ignore
        ~f:(fun target ->
          let text = Js.to_string target##.value in
          handler ev text)))
;;

let on_change = on_input_event Type_id.event "change"
let on_input = on_input_event Type_id.event "input"
let to_raw l = to_raw l

let on_file_input handler =
  on Type_id.event "input" (fun ev ->
    Js.Opt.case ev##.target const_ignore (fun target ->
      Js.Opt.case (Dom_html.CoerceTo.input target) const_ignore (fun target ->
        handler ev target##.files)))
;;

module Always_focus_hook = struct
  module T = struct
    module State = Unit

    module Input = struct
      include Unit

      let combine () () = ()
    end

    let init () _ = ()
    let on_mount () () element = element##focus
    let on_mount = `Schedule_animation_frame on_mount
    let update ~old_input:() ~new_input:() () _ = ()
    let destroy () () _ = ()
  end

  module Hook = Hooks.Make (T)

  let attr `Read_the_docs__this_hook_is_unpredictable =
    (* Append the id to the name of the hook to ensure that it is distinct
       from all other focus hooks. *)
    create_hook "always-focus-hook" (Hook.create ())
  ;;
end

module Single_focus_hook () = struct
  module T = struct
    module State = Unit

    let has_been_used = ref false

    module Input = struct
      type t = (unit Ui_effect.t[@sexp.opaque]) [@@deriving sexp_of]

      let combine left right = Ui_effect.Many [ left; right ]
    end

    let init _ _ = ()

    let on_mount event () element =
      if not !has_been_used
      then (
        has_been_used := true;
        element##focus;
        Effect.Expert.handle_non_dom_event_exn event)
    ;;

    let on_mount = `Schedule_animation_frame on_mount
    let update ~old_input:_ ~new_input:_ () _ = ()
    let destroy _ () _ = ()
  end

  module Hook = Hooks.Make (T)

  let attr `Read_the_docs__this_hook_is_unpredictable ~after =
    (* Append the id to the name of the hook to ensure that it is distinct
       from all other focus hooks. *)
    create_hook "single-focus-hook" (Hook.create after)
  ;;
end

module No_op_hook (M : sig
    module Input : Hooks_intf.Input

    val name : string
  end) =
struct
  module Hook = Hooks.Make (struct
      module State = Unit
      module Input = M.Input

      let init _ _ = ()
      let on_mount = `Do_nothing
      let update ~old_input:_ ~new_input:_ () _ = ()
      let destroy _ () _ = ()
    end)

  let attr input = create_hook M.name (Hook.create input)
  let type_id = Hook.For_testing.type_id
end

module Multi = struct
  type nonrec t = t list

  let map_style ~(here : [%call_pos]) t ~f =
    [ Many_only_merge_classes_and_styles
        { attrs = t; map_styles = f; map_classes = Fn.id; here }
    ]
  ;;

  let add_class ~(here : [%call_pos]) t c =
    [ Many_only_merge_classes_and_styles
        { attrs = t; map_styles = Fn.id; map_classes = (fun cs -> c :: cs); here }
    ]
  ;;

  let add_style t s = map_style t ~f:(fun ss -> Css_gen.combine ss s)

  let merge_classes_and_styles ~(here : [%call_pos]) t =
    [ Many_only_merge_classes_and_styles
        { attrs = t; map_styles = Fn.id; map_classes = Fn.id; here }
    ]
  ;;
end

module Css_var_hook = Hooks.Make (struct
    open Js_of_ocaml
    module State = Unit

    module Input = struct
      type t = (string * string) list [@@deriving sexp_of]

      let combine = List.append
    end

    let init input (element : Dom_html.element Js.t) =
      List.iter input ~f:(fun (k, v) ->
        element##.style##setProperty (Js.string k) (Js.string v) Js.undefined
        |> (ignore : Js.js_string Js.t -> unit))
    ;;

    let on_mount = `Do_nothing

    let destroy input () (element : Dom_html.element Js.t) =
      List.iter input ~f:(fun (k, _) ->
        element##.style##removeProperty (Js.string k)
        |> (ignore : Js.js_string Js.t -> unit))
    ;;

    let update ~old_input ~new_input () (element : Dom_html.element Js.t) =
      if phys_equal old_input new_input
         || [%equal: (string * string) list] old_input new_input
      then ()
      else (
        destroy old_input () element;
        init new_input element)
    ;;
  end)

let __css_vars_no_kebabs alist = create_hook "custom-css-vars" (Css_var_hook.create alist)
let css_var ~name v = __css_vars_no_kebabs [ "--" ^ name, v ]

module Expert = struct
  let rec filter_by_kind t ~f =
    match t with
    | Property _ -> if f `Property then t else empty
    | Attribute _ -> if f `Attribute then t else empty
    | Hook _ -> if f `Hook then t else empty
    | Handler _ -> if f `Handler then t else empty
    | Style _ -> if f `Style then t else empty
    | Class _ -> if f `Class then t else empty
    | Lazy (lazy t) -> filter_by_kind t ~f
    | Many attrs -> Many (List.map attrs ~f:(filter_by_kind ~f))
    | Many_only_merge_classes_and_styles { attrs; map_styles; map_classes; here } ->
      let attrs = List.map attrs ~f:(filter_by_kind ~f) in
      Many_only_merge_classes_and_styles { attrs; map_styles; map_classes; here }
    | Many_without_merge { attrs; here } ->
      let attrs = List.map attrs ~f:(filter_by_kind ~f) in
      Many_without_merge { attrs; here }
  ;;

  let rec contains_name looking_for = function
    | Property { name; _ } | Attribute { name; _ } | Hook { name; _ } ->
      String.equal looking_for name
    | Handler { name; _ } -> String.equal ("on" ^ name) looking_for
    | Style _ -> String.equal looking_for "style"
    | Class _ -> String.equal looking_for "class"
    | Lazy (lazy t) -> contains_name looking_for t
    | Many attrs
    | Many_only_merge_classes_and_styles { attrs; _ }
    | Many_without_merge { attrs; here = _ } ->
      List.exists ~f:(contains_name looking_for) attrs
  ;;

  let set_explicitly_print_locations bool = explicitly_print_locations := bool
end

let create ~(here : [%call_pos]) name value = create ~here name value
let create_float ~(here : [%call_pos]) name value = create_float ~here name value
let property ~(here : [%call_pos]) name value = property ~here name value
let string_property ~(here : [%call_pos]) name value = string_property ~here name value
let bool_property ~(here : [%call_pos]) name value = bool_property ~here name value
