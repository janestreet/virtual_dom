open! Base
open Js_of_ocaml
module Vdom_raw = Raw

(** This has 3 kinds of constructors.
    {v
      - constructors for properties / attributes for which we
        have written first class ocaml representations (so far only Style
        and Class)

      - Those which we immediately convert into Js called Raw, which
        in turn has two cases:
        - Property for properties on the DOM
        - Attribute for attributes on the DOM

      - Hooks, which register callbacks on property addition and removal.
    v}

    Generally speaking one should avoid creating a property or attribute
    for something for which we have a first class representation.
*)

module Hook : sig
  type t

  val create
    :  init:(Dom_html.element Js.t -> 'state)
    -> ?update:('state -> Dom_html.element Js.t -> 'state)
    -> ?destroy:('state -> Dom_html.element Js.t -> unit)
    -> id:'state Type_equal.Id.t
    -> t

  val pack : t -> Js.Unsafe.any
end = struct
  type t = Js.Unsafe.any

  let generic_hook = lazy Js.Unsafe.(get global (Js.string "GenericHook"))

  let create ~init ?update ?destroy ~id =
    let wrap a = a |> Js.wrap_callback |> Js.Unsafe.inject in
    let init = wrap init in
    let update =
      update |> Option.map ~f:Js.wrap_callback |> Js.Opt.option |> Js.Unsafe.inject
    in
    let destroy = destroy |> Option.value ~default:(fun _ _ -> ()) |> wrap in
    let generic_hook = Lazy.force generic_hook in
    Js.Unsafe.fun_call generic_hook [| init; update; destroy; id |> Js.Unsafe.inject |]
  ;;

  let pack = Fn.id
end

module T : sig
  type t

  (** {2 Attribute creation functions *)

  val create : string -> string -> t
  val create_float : string -> float -> t
  val create_hook : string -> Hook.t -> t

  (** {2 Property creation functions *)

  val property : string -> Js.Unsafe.any -> t
  val string_property : string -> string -> t
  val bool_property : string -> bool -> t

  (** {2 style creation functions *)

  val style : Css_gen.t -> t

  (** {2 class creation functions *)

  val classes : string list -> t
  val class_ : string -> t
  val classes' : (string, String.comparator_witness) Set.t -> t

  (** {2 convertion functions *)

  val to_raw : t list -> Raw.Attrs.t
  val to_style : t -> Css_gen.t option
  val to_class : t -> (string, String.comparator_witness) Set.t option
end = struct
  type t =
    | Property of string * Js.Unsafe.any
    | Attribute of string * Js.Unsafe.any
    | Hook of string * Hook.t
    | Style of Css_gen.t
    | Class of (string, String.comparator_witness) Set.t

  let create name value = Attribute (name, Js.Unsafe.inject (Js.string value))

  let create_float name value =
    Attribute (name, Js.Unsafe.inject (Dom_float.to_js_string value))
  ;;

  let property name value = Property (name, value)
  let string_property name value = Property (name, Js.Unsafe.inject (Js.string value))
  let bool_property name value = Property (name, Js.Unsafe.inject (Js.bool value))
  let create_hook name hook = Hook (name, hook)

  external ojs_of_any : Js.Unsafe.any -> Gen_js_api.Ojs.t = "%identity"

  let to_raw attrs =
    (* When input elements have their value set to what it already is
       the cursor gets moved to the end of the field even when the user
       is editing in the middle. SoftSetHook (from ./soft-set-hook.js)
       compares before setting, avoiding the problem just like in
       https://github.com/Matt-Esch/virtual-dom/blob/947ecf92b67d25bb693a0f625fa8e90c099887d5/virtual-hyperscript/index.js#L43-L51

       note that Elm's virtual-dom includes a workaround for this so
       if we switch to that the workaround here will be unnecessary.
       https://github.com/elm-lang/virtual-dom/blob/17b30fb7de48672565d6227d33c0176f075786db/src/Native/VirtualDom.js#L434-L439
    *)
    let softSetHook x : Gen_js_api.Ojs.t = Js.Unsafe.global ## SoftSetHook x in
    let attrs_obj : Vdom_raw.Attrs.t = Vdom_raw.Attrs.create () in
    List.iter
      ~f:(function
        | Hook (name, hook) ->
          Vdom_raw.Attrs.set_property attrs_obj name (ojs_of_any (Hook.pack hook))
        | Property ("value", value) ->
          let value = softSetHook value in
          Vdom_raw.Attrs.set_property attrs_obj "value" value
        | Property (name, value) ->
          Vdom_raw.Attrs.set_property attrs_obj name (ojs_of_any value)
        | Attribute (name, value) ->
          Vdom_raw.Attrs.set_attribute attrs_obj name (ojs_of_any value)
        | Style css ->
          let props = Css_gen.to_string_list css in
          let obj = Gen_js_api.Ojs.empty_obj () in
          List.iter
            ~f:(fun (k, v) -> Gen_js_api.Ojs.set obj k (Gen_js_api.Ojs.string_to_js v))
            props;
          Vdom_raw.Attrs.set_property attrs_obj "style" obj
        | Class classes ->
          Vdom_raw.Attrs.set_attribute
            attrs_obj
            "class"
            (Gen_js_api.Ojs.string_to_js (String.concat (Set.to_list classes) ~sep:" ")))
      attrs;
    attrs_obj
  ;;

  let to_style : t -> _ = function
    | Style s -> Some s
    | Class _ | Property _ | Attribute _ | Hook _ -> None
  ;;

  let style css = Style css

  let valid_class_name s =
    let invalid = String.is_empty s || String.exists s ~f:Char.is_whitespace in
    not invalid
  ;;

  let%test "valid" = valid_class_name "foo-bar"
  let%test "invalid-empty" = not (valid_class_name "")
  let%test "invalid-space" = not (valid_class_name "foo bar")

  let class_ classname =
    if not (valid_class_name classname)
    then raise_s [%message "invalid classname" (classname : string)];
    Class (Set.singleton (module String) classname)
  ;;

  let classes' classes = Class classes

  let classes classnames =
    if not (List.for_all ~f:valid_class_name classnames)
    then raise_s [%message "invalid classnames" (classnames : string list)];
    classes' (Set.of_list (module String) classnames)
  ;;

  let to_class = function
    | Class cs -> Some cs
    | Style _ | Property _ | Attribute _ | Hook _ -> None
  ;;
end

include T

let create name value = T.create name value
let create_float name value = T.create_float name value
let property name value = T.property name value
let string_property name value = T.string_property name value
let bool_property name value = T.bool_property name value
let id s = create "id" s
let name s = create "name" s
let href r = create "href" r
let checked = create "checked" ""
let selected = create "selected" ""
let hidden = create "hidden" ""
let disabled = create "disabled" ""
let placeholder x = create "placeholder" x
let autofocus b = create "autofocus" (Bool.to_string b)
let for_ x = create "for" x
let type_ x = create "type" x
let value x = create "value" x
let tabindex x = create "tabindex" (Int.to_string x)
let title x = create "title" x
let src x = create "src" x
let min x = create_float "min" x
let max x = create_float "max" x

let on event convert_to_vdom_event : t =
  let f e =
    Event.Expert.handle e (convert_to_vdom_event e);
    Js._true
  in
  property ("on" ^ event) (Js.Unsafe.inject (Dom.handler f))
;;

let on_focus = on "focus"
let on_blur = on "blur"
let on_click = on "click"
let on_contextmenu = on "contextmenu"
let on_double_click = on "dblclick"
let on_mousemove = on "mousemove"
let on_mouseup = on "mouseup"
let on_mousedown = on "mousedown"
let on_mouseenter = on "mouseenter"
let on_mouseleave = on "mouseleave"
let on_mouseover = on "mouseover"
let on_mouseout = on "mouseout"
let on_keyup = on "keyup"
let on_keypress = on "keypress"
let on_keydown = on "keydown"
let const_ignore _ = Event.Ignore

class type value_element =
  object
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

let on_input_event event handler =
  on event (fun ev ->
    Js.Opt.case ev##.target const_ignore (fun target ->
      Option.value_map
        (coerce_value_element target)
        ~default:Event.Ignore
        ~f:(fun target ->
          let text = Js.to_string target##.value in
          handler ev text)))
;;

let on_change = on_input_event "change"
let on_input = on_input_event "input"
let to_raw l = T.to_raw l

module Expert = struct
  let create_basic_hook name ?hook ?unhook () =
    let hook = Option.value hook ~default:(Fn.const ()) in
    let unhook = Option.map unhook ~f:(fun f () -> f) in
    let id = Type_equal.Id.create ~name:"placeholder" [%sexp_of: unit] in
    T.create_hook name (Hook.create ~init:hook ?update:None ?destroy:unhook ~id)
  ;;

  let create_stateful_hook name ~hook ~unhook ~id =
    T.create_hook name (Hook.create ~init:hook ?update:None ~destroy:unhook ~id)
  ;;

  let create_persistent_hook name ~init ~update ~destroy ~id =
    T.create_hook name (Hook.create ~init ~update ~destroy ~id)
  ;;
end
