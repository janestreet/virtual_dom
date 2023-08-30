open! Core
open! Js_of_ocaml
open Virtual_dom

type element =
  { tag_name : string
  ; attributes : (string * string) list [@sexp.list]
  ; string_properties : (string * string) list [@sexp.list]
  ; bool_properties : (string * bool) list [@sexp.list]
  ; styles : (string * string) list [@sexp.list]
  ; handlers : (string * Handler.t) list [@sexp.list]
  ; hooks : (string * Vdom.Attr.Hooks.For_testing.Extra.t) list [@sexp.list]
  ; key : string option [@sexp.option]
  ; children : t list [@sexp.list]
  }
[@@deriving sexp_of]

and t =
  | Text of string
  | Element of element
  | Widget
[@@deriving sexp_of]

let rec inner_text = function
  | Text s ->
    (match String.strip s with
     | "" -> None
     | s -> Some s)
  | Element { children; _ } ->
    (match children |> List.filter_map ~f:inner_text with
     | [] -> None
     | xs -> xs |> String.concat ~sep:" " |> Some)
  | Widget -> None
;;

let inner_text t = inner_text t |> Option.value ~default:""

let is_tag ~tag = function
  | Element { tag_name; _ } -> String.equal tag_name tag
  | _ -> false
;;

let has_class ~cls = function
  | Element { attributes; _ } ->
    List.exists attributes ~f:(function
      | "class", data -> data |> String.split ~on:' ' |> List.exists ~f:(String.equal cls)
      | _ -> false)
  | _ -> false
;;

let rec map t ~f =
  match f t with
  | `Replace_with t -> t
  | `Continue ->
    (match t with
     | Text _ | Widget -> t
     | Element element ->
       let children = List.map element.children ~f:(fun ch -> map ch ~f) in
       Element { element with children })
;;

type hidden_soup = Hidden_soup : _ Soup.node -> hidden_soup

type 'a breadcrumb_preference =
  | Don't_add_breadcrumbs : unit breadcrumb_preference
  | Add_breadcrumbs : (Soup.element Soup.node -> t) breadcrumb_preference

module Soup_id = Unique_id.Int ()

let soup_id_key = "soup-id"

let to_lambda_soup (type a) t (breadcrumb_preference : a breadcrumb_preference)
  : hidden_soup * a
  =
  let t_by_soup_id = String.Table.create () in
  let rec convert t =
    match t with
    | Text s -> Hidden_soup (Soup.create_text s)
    | Widget ->
      let element = Soup.create_element "widget" ~attributes:[] in
      Hidden_soup element
    | Element
        { tag_name
        ; attributes
          (* We ignore [string_properties] / [bool_properties] as their names can overlap
           with attributes. Ignoring them here currently just means that people cannot
           select on them when triggering events.

           *)
        ; string_properties = _
        ; bool_properties = _
        ; handlers
        ; key
        ; children
        ; hooks
        ; styles = _
        } ->
      let key_attrs =
        match key with
        | Some key -> [ "key", key ]
        | None -> []
      in
      let soup_id_attrs =
        match breadcrumb_preference with
        | Don't_add_breadcrumbs -> []
        | Add_breadcrumbs ->
          let soup_id = Soup_id.create () |> Soup_id.to_string in
          Hashtbl.add_exn t_by_soup_id ~key:soup_id ~data:t;
          [ soup_id_key, soup_id ]
      in
      let handler_attrs =
        List.map handlers ~f:(fun (name, _) -> name, "<event-handler>")
      in
      let hook_attrs = List.map hooks ~f:(fun (name, _) -> name, "<hook>") in
      let attributes =
        [ hook_attrs; key_attrs; soup_id_attrs; handler_attrs; attributes ]
        |> List.concat
        |> String.Map.of_alist_exn (* Raise on duplicate attributes *)
        |> Map.to_alist
      in
      let element = Soup.create_element tag_name ~attributes in
      List.iter children ~f:(fun child ->
        let (Hidden_soup child) = convert child in
        Soup.append_child element child);
      Hidden_soup element
  in
  ( convert t
  , match breadcrumb_preference with
    | Don't_add_breadcrumbs -> ()
    | Add_breadcrumbs ->
      fun soup ->
        (match Soup.attribute soup_id_key soup with
         | None -> raise_s [%message "Soup.node has no soup-id attribute"]
         | Some soup_id -> Hashtbl.find_exn t_by_soup_id soup_id) )
;;

let _to_string_html t =
  let Hidden_soup soup, () = to_lambda_soup t Don't_add_breadcrumbs in
  Soup.to_string soup
;;

(* Printing elements in single-line and multiline formats is essentially the
   same. The main difference is what attributes are separated by: in
   single-line, they are separated just by spaces, but in multiline they are
   separated by a newline and some indentation.
*)
let bprint_element
  buffer
  ~sep
  ~before_styles
  ~filter_printed_attributes
  { tag_name
  ; attributes
  ; string_properties
  ; bool_properties
  ; styles
  ; handlers
  ; key
  ; hooks
  ; children = _
  }
  =
  bprintf buffer "<%s" tag_name;
  let has_printed_an_attribute = ref false in
  let bprint_aligned_indent () =
    if !has_printed_an_attribute
    then bprintf buffer "%s" sep
    else (
      has_printed_an_attribute := true;
      bprintf buffer " ")
  in
  let list_iter_filter l ~f =
    List.filter_map l ~f:filter_printed_attributes |> List.iter ~f
  in
  Option.bind key ~f:(fun key -> filter_printed_attributes ("@key", key))
  |> Option.iter ~f:(fun (_, v) ->
       bprint_aligned_indent ();
       bprintf buffer "@key=%s" v);
  list_iter_filter attributes ~f:(fun (k, v) ->
    bprint_aligned_indent ();
    bprintf buffer "%s=\"%s\"" k v);
  list_iter_filter string_properties ~f:(fun (k, v) ->
    bprint_aligned_indent ();
    bprintf buffer "#%s=\"%s\"" k v);
  bool_properties
  |> List.map ~f:(Tuple2.map_snd ~f:Bool.to_string)
  |> list_iter_filter ~f:(fun (k, v) ->
       bprint_aligned_indent ();
       bprintf buffer "#%s=\"%b\"" k (Bool.of_string v));
  hooks
  |> List.map ~f:(fun (k, v) ->
       k, v |> [%sexp_of: Vdom.Attr.Hooks.For_testing.Extra.t] |> Sexp.to_string_mach)
  |> list_iter_filter ~f:(fun (k, v) ->
       bprint_aligned_indent ();
       bprintf buffer "%s=%s" k v);
  handlers
  |> List.map ~f:(fun (k, _) -> k, "handler")
  |> list_iter_filter ~f:(fun (k, _) ->
       bprint_aligned_indent ();
       bprintf buffer "%s" k);
  let styles =
    List.filter_map styles ~f:(fun (name, v) ->
      let open Option.Let_syntax in
      let%map _, v = filter_printed_attributes ("style." ^ name, v) in
      name, v)
  in
  if not (List.is_empty styles)
  then (
    bprint_aligned_indent ();
    bprintf buffer "style={";
    List.iter styles ~f:(fun (k, v) ->
      bprint_aligned_indent ();
      bprintf buffer "%s%s: %s;" before_styles k v);
    bprint_aligned_indent ();
    bprintf buffer "}");
  bprintf buffer ">"
;;

let bprint_element_single_line buffer element =
  bprint_element buffer ~sep:" " ~before_styles:"" element
;;

let bprint_element_multi_line buffer ~indent element =
  let align_with_first_attribute = String.map element.tag_name ~f:(Fn.const ' ') ^ "  " in
  let sep = "\n" ^ indent ^ align_with_first_attribute in
  bprint_element buffer ~sep ~before_styles:"  " element
;;

let path_regexp = Js_of_ocaml.Regexp.regexp "bonsai_path(_[a-z]*)*"
let hash_regexp = Js_of_ocaml.Regexp.regexp "_hash_[a-f0-9]+"

let to_string_html
  ?(filter_printed_attributes = fun ~key:_ ~data:_ -> true)
  ?(censor_paths = true)
  ?(censor_hash = true)
  ?(path_censoring_message = "bonsai_path_replaced_in_test")
  ?(hash_censoring_message = "_hash_replaced_in_test")
  t
  =
  let pre_censor to_censor apply_censor kv =
    if to_censor then Tuple2.map ~f:apply_censor kv else kv
  in
  let filter_printed_attributes (key, data) =
    match filter_printed_attributes ~key ~data with
    | true ->
      (key, data)
      |> pre_censor censor_paths (fun s ->
           Js_of_ocaml.Regexp.global_replace path_regexp s path_censoring_message)
      |> pre_censor censor_hash (fun s ->
           Js_of_ocaml.Regexp.global_replace hash_regexp s hash_censoring_message)
      |> Some
    | false -> None
  in
  (* Keep around the buffer so that it is not re-allocated for every element *)
  let single_line_buffer = Buffer.create 200 in
  let rec recurse buffer ~depth =
    let indent = String.init (depth * 2) ~f:(Fn.const ' ') in
    function
    | Text s -> bprintf buffer "%s%s" indent s
    | Element element ->
      bprintf buffer "%s" indent;
      Buffer.reset single_line_buffer;
      bprint_element_single_line ~filter_printed_attributes single_line_buffer element;
      if Buffer.length single_line_buffer < 100 - String.length indent
      then Buffer.add_buffer buffer single_line_buffer
      else bprint_element_multi_line ~filter_printed_attributes buffer ~indent element;
      let children_should_collapse =
        List.for_all element.children ~f:(function
          | Text _ -> true
          | _ -> false)
        && List.fold element.children ~init:0 ~f:(fun acc child ->
             match child with
             | Text s -> acc + String.length s
             | _ -> acc)
           < 80 - String.length indent
      in
      let depth = if children_should_collapse then 0 else depth + 1 in
      List.iter element.children ~f:(fun child ->
        if children_should_collapse then bprintf buffer " " else bprintf buffer "\n";
        recurse buffer ~depth child);
      if children_should_collapse
      then bprintf buffer " "
      else (
        bprintf buffer "\n";
        bprintf buffer "%s" indent);
      bprintf buffer "</%s>" element.tag_name
    | Widget -> bprintf buffer "%s<widget/>" indent
  in
  let buffer = Buffer.create 100 in
  recurse buffer ~depth:0 t;
  Buffer.contents buffer
;;

let select t ~selector =
  let Hidden_soup element, find_t_by_soup_exn = to_lambda_soup t Add_breadcrumbs in
  let soup = Soup.create_soup () in
  Soup.append_root soup element;
  soup |> Soup.select selector |> Soup.to_list |> List.map ~f:find_t_by_soup_exn
;;

let select_first t ~selector = select t ~selector |> List.hd

let select_first_exn t ~selector =
  match select_first t ~selector with
  | Some node -> node
  | None ->
    raise_s
      [%message
        "Failed to find element matching selector"
          (selector : string)
          ~from_node:(to_string_html t : string)]
;;

let rec unsafe_of_js_exn =
  let make_text_node (text : Js.js_string Js.t) = Text (Js.to_string text) in
  let make_element_node
    (tag_name : Js.js_string Js.t)
    (children : t Js.js_array Js.t)
    (handlers : (Js.js_string Js.t * Js.Unsafe.any) Js.js_array Js.t)
    (attributes : (Js.js_string Js.t * Js.js_string Js.t) Js.js_array Js.t)
    (string_properties : (Js.js_string Js.t * Js.js_string Js.t) Js.js_array Js.t)
    (bool_properties : (Js.js_string Js.t * bool Js.t) Js.js_array Js.t)
    (styles : (Js.js_string Js.t * Js.js_string Js.t) Js.js_array Js.t)
    (hooks : (Js.js_string Js.t * Vdom.Attr.Hooks.For_testing.Extra.t) Js.js_array Js.t)
    (key : Js.js_string Js.t Js.Opt.t)
    =
    let tag_name = tag_name |> Js.to_string in
    let children = children |> Js.to_array |> Array.to_list in
    let handlers =
      handlers
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (s, h) ->
           let name = Js.to_string s in
           name, Handler.of_any_exn h ~name)
    in
    let attributes =
      attributes
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (k, v) ->
           let k, v = Js.to_string k, Js.to_string v in
           let v =
             if [%equal: string] k "class"
             then
               v
               |> String.split ~on:' '
               |> List.dedup_and_sort ~compare:[%compare: string]
               |> String.concat ~sep:" "
             else v
           in
           k, v)
    in
    let hooks =
      hooks
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (k, v) -> Js.to_string k, v)
    in
    let string_properties =
      string_properties
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (k, v) -> Js.to_string k, Js.to_string v)
    in
    let bool_properties =
      bool_properties
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (k, v) -> Js.to_string k, Js.to_bool v)
    in
    let styles =
      styles
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (k, v) -> Js.to_string k, Js.to_string v)
    in
    let key = key |> Js.Opt.to_option |> Option.map ~f:Js.to_string in
    Element
      { tag_name
      ; children
      ; handlers
      ; attributes
      ; string_properties
      ; bool_properties
      ; key
      ; hooks
      ; styles
      }
  in
  let make_widget_node
    (_id : _ Type_equal.Id.t)
    (vdom_for_testing : Js.Unsafe.any Lazy.t option)
    =
    match vdom_for_testing with
    | Some vdom -> unsafe_of_js_exn (Lazy.force vdom)
    | None -> Widget
  in
  let raise_unknown_node_type node_type =
    let node_type = Js.to_string node_type in
    raise_s [%message "unrecognized node type" (node_type : string)]
  in
  let f =
    Js.Unsafe.pure_js_expr
      {js|
   // Convert analyzes a Vdom node that was produced by [Node.to_js] and walks the tree
   // recursively, calling make_text_node, make_element_node, and make_widget_node depending
   // on the type of node.
   (function convert(node, make_text_node, make_element_node, make_widget_node, raise_unknown_node_type) {
       switch (node.type) {
           case 'Thunk':
               var n = node.fn(node.args);
               return convert(n, make_text_node, make_element_node, make_widget_node, raise_unknown_node_type);
           case 'VirtualText':
               return make_text_node(node.text);
           case 'Widget':
               return make_widget_node(node.id, node.vdomForTesting);
           case 'VirtualNode':
               var attributes = node.properties.attributes || {};
               var attr_list = Object.keys(attributes).map(function(key) {
                   return [0, key, attributes[key].toString()];
               });
               var children = node.children.map(function(node) {
                   return convert(node, make_text_node, make_element_node, make_widget_node, raise_unknown_node_type);
               });
               var handlers =
                   Object.keys(node.properties)
                   .filter(function(key) {
                       // This is a bit of a hack, but it works for all the handlers that we
                       // have defined at the moment.  Consider removing the 'on' check?
                       return key.startsWith("on") && typeof node.properties[key] === 'function';
                   })
                   .map(function(key) {
                       // [0, ...] is how to generate an OCaml tuple from the JavaScript side.
                       return [0, key, node.properties[key]];
                   });
               var string_properties =
                   Object.keys(node.properties)
                   .filter(function(key) {
                       return typeof node.properties[key] === 'string';
                   })
                   .map(function(key) {
                       return [0, key, node.properties[key]]
                   });
               var bool_properties =
                   Object.keys(node.properties)
                   .filter(function(key) {
                     return typeof node.properties[key] === 'boolean';
                   })
                   .map(function(key) {
                       return [0, key, node.properties[key]]
                   });
               var styles =
                   Object.keys(node.properties.style ? node.properties.style : {})
                   .filter(function(key) {
                       return typeof node.properties.style[key] === 'string';
                   })
                   .map(function(key) {
                       return [0, key, node.properties.style[key]]
                   });
               var hooks =
                   Object.keys(node.properties)
                   .filter(function(key) {
                       return typeof node.properties[key] === 'object' &&
                           typeof node.properties[key]['extra'] === 'object';
                   })
                   .map(function(key) {
                       return [0, key, node.properties[key]['extra']]
                   });
               var soft_set_hooks =
                   Object.keys(node.properties)
                   .filter(function(key) {
                     return node.properties[key] instanceof joo_global_object.SoftSetHook;
                   })
                   .map(function(key) {
                     return [0, key, "" + node.properties[key].value];
                   });
               return make_element_node(
                   node.tagName,
                   children,
                   handlers,
                   attr_list,
                   string_properties.concat(soft_set_hooks),
                   bool_properties,
                   styles,
                   hooks,
                   node.key || null);
           default:
               raise_unknown_node_type("" + node.type);
       }
   })
   |js}
  in
  fun value ->
    Js.Unsafe.fun_call
      f
      [| value
       ; Js.Unsafe.inject (Js.wrap_callback make_text_node)
       ; Js.Unsafe.inject (Js.wrap_callback make_element_node)
       ; Js.Unsafe.inject (Js.wrap_callback make_widget_node)
       ; Js.Unsafe.inject (Js.wrap_callback raise_unknown_node_type)
      |]
;;

let unsafe_convert_exn vdom_node =
  vdom_node |> Virtual_dom.Vdom.Node.to_raw |> Js.Unsafe.inject |> unsafe_of_js_exn
;;

let get_handlers (node : t) =
  match node with
  | Element { handlers; _ } -> handlers
  | _ -> raise_s [%message "expected Element node" (node : t)]
;;

let trigger_many ?extra_fields node ~event_names =
  let all_handlers = get_handlers node in
  let count =
    List.count event_names ~f:(fun event_name ->
      match List.Assoc.find all_handlers event_name ~equal:String.equal with
      | None -> false
      | Some handler ->
        Handler.trigger handler ?extra_fields;
        true)
  in
  match count with
  | 0 -> raise_s [%message "No handler found on element" (event_names : string list)]
  | _ -> ()
;;

let trigger ?extra_fields node ~event_name =
  trigger_many ?extra_fields node ~event_names:[ event_name ]
;;

let get_hook_value : type a. t -> type_id:a Type_equal.Id.t -> name:string -> a =
  fun t ~type_id ~name ->
  match t with
  | Element { hooks; _ } ->
    (match List.Assoc.find ~equal:String.equal hooks name with
     | Some hook ->
       let (Vdom.Attr.Hooks.For_testing.Extra.T { type_id = type_id_v; value }) = hook in
       (match Type_equal.Id.same_witness type_id_v type_id with
        | Some T -> value
        | None ->
          failwithf
            "get_hook_value: a hook for %s was found, but the type-ids were not the \
             same; are you using the same type-id that you got from the For_testing \
             module from your hook creator?"
            name
            ())
     | None -> failwithf "get_hook_value: no hook found with name %s" name ())
  | Text _ -> failwith "get_hook_value: expected Element, found Text"
  | Widget -> failwith "get_hook_value: expected Element, found Widget"
;;

let trigger_hook t ~type_id ~name ~f ~arg =
  Ui_effect.Expert.handle (f (get_hook_value t ~type_id ~name) arg)
;;

module User_actions = struct
  let prevent_default = "preventDefault", Js.Unsafe.inject (Js.wrap_callback Fn.id)
  let stop_propagation = "stopPropagation", Js.Unsafe.inject (Js.wrap_callback Fn.id)
  let both_event_handlers = [ prevent_default; stop_propagation ]

  let build_event_object
    ?(shift_key_down = false)
    ?(ctrl_key_down = false)
    ?(alt_key_down = false)
    ?(meta_key_down = false)
    ~extra_event_fields
    ~include_modifier_keys
    event_specific_fields
    =
    let extra_event_fields = Option.value extra_event_fields ~default:[] in
    let modifiers =
      if include_modifier_keys
      then
        [ "shiftKey", Js.Unsafe.inject (Js.bool shift_key_down)
        ; "ctrlKey", Js.Unsafe.inject (Js.bool ctrl_key_down)
        ; "altKey", Js.Unsafe.inject (Js.bool alt_key_down)
        ; "metaKey", Js.Unsafe.inject (Js.bool meta_key_down)
        ]
      else []
    in
    modifiers @ both_event_handlers @ extra_event_fields @ event_specific_fields
  ;;

  let click_on
    ?extra_event_fields
    ?shift_key_down
    ?ctrl_key_down
    ?alt_key_down
    ?meta_key_down
    node
    =
    trigger
      ~event_name:"onclick"
      node
      ~extra_fields:
        (build_event_object
           ?shift_key_down
           ?ctrl_key_down
           ?alt_key_down
           ?meta_key_down
           ~extra_event_fields
           ~include_modifier_keys:true
           [])
  ;;

  let focus ?extra_event_fields node =
    trigger
      ~event_name:"onfocus"
      node
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let blur ?extra_event_fields ?related_target node =
    let related_target =
      match related_target with
      | Some related_target ->
        Js.Unsafe.inject
          (object%js
             val id =
               match related_target with
               | Text _ | Widget -> Js.null
               | Element { attributes; _ } ->
                 (match List.Assoc.find attributes ~equal:String.equal "id" with
                  | Some id -> Js.Opt.return (Js.string id)
                  | None -> Js.null)
          end)
      | None -> Js.Unsafe.inject Js.undefined
    in
    trigger
      ~event_name:"onblur"
      node
      ~extra_fields:
        (build_event_object
           ~extra_event_fields
           ~include_modifier_keys:false
           [ "relatedTarget", related_target ])
  ;;

  let tag_name_exn = function
    | Element { tag_name; _ } -> tag_name
    | other ->
      let node = to_string_html other in
      raise_s [%message (node : string) "is not an element"]
  ;;

  let build_target ~element ~value =
    (* When an [on_input] event is fired, in order to pull the value of
       the element, [Virtual_dom.Vdom.Attr.on_input_event] looks at the
       "target" property on the event and tries to coerce that value to one
       of [input element, select element, textarea element].  This coercion
       function is implemented in [Js_of_ocaml.Dom_html.CoerceTo], and the
       way that the coercion function works is by comparing the value of
       the [tagName] property on the event target to the string of the tag
       name that the coercion is targeting.

       By mocking out the [tagName] and [value] properties on the target of
       the event, we can trick the virtual_dom code into handling our event
       as though there was a real DOM element! *)
    Js.Unsafe.inject
      (object%js
         val tagName = Js.string (tag_name_exn element)
         val value = Js.string value
      end)
  ;;

  let set_checkbox
    ?extra_event_fields
    ?shift_key_down
    ?ctrl_key_down
    ?alt_key_down
    ?meta_key_down
    element
    ~checked
    =
    let target =
      (* Similarly to [build_target] we inject a target field with some additional
         attributes that are relied upon -- in this case by
         Bonsai_web_ui_form.Elements.checkbox, which is a common way to construct checkbox
         elements. *)
      Js.Unsafe.inject
        (object%js
           val tagName = Js.string (tag_name_exn element)
           val checked = Js.bool checked
           val value = Js.string "the 'value' property of a checkbox is not used"
        end)
    in
    let event_object =
      build_event_object
        ?shift_key_down
        ?ctrl_key_down
        ?alt_key_down
        ?meta_key_down
        ~extra_event_fields
        ~include_modifier_keys:true
        [ "target", target ]
    in
    (* [Vdom_input_widgets] uses [on_click] and [Multi_select] uses
       [on_change], so we trigger both.  This is safe, as both of these
       events are actually triggered in the dom, so it's a realistic
       simulation of what's actually going on in the browser. *)
    trigger_many element ~event_names:[ "onclick"; "onchange" ] ~extra_fields:event_object
  ;;

  let input_text ?extra_event_fields element ~text =
    let target = build_target ~element ~value:text in
    let event_names = [ "oninput"; "onchange" ] in
    trigger_many
      element
      ~event_names
      ~extra_fields:
        (build_event_object
           ~extra_event_fields
           ~include_modifier_keys:true
           [ "target", target ])
  ;;

  let keydown
    ?extra_event_fields
    ?shift_key_down
    ?ctrl_key_down
    ?alt_key_down
    ?meta_key_down
    element
    ~key
    =
    let open Vdom_keyboard in
    let key_code = Keystroke.Keyboard_code.to_key_code key in
    let location = Keystroke.Keyboard_code.to_location key in
    (* The keydown event only requires a [tagName] field *)
    let target =
      Js.Unsafe.inject
        (object%js
           val tagName = Js.string (tag_name_exn element)
        end)
    in
    let int_to_any x = Js.Unsafe.coerce (Js.number_of_float (Int.to_float x)) in
    let event_names = [ "onkeydown" ] in
    let default_prevented _ =
      print_s [%message "default prevented" (key : Keystroke.Keyboard_code.t)]
    in
    trigger_many
      element
      ~event_names
      ~extra_fields:
        (build_event_object
           ?shift_key_down
           ?ctrl_key_down
           ?alt_key_down
           ?meta_key_down
           ~extra_event_fields
           ~include_modifier_keys:true
           [ "location", int_to_any location
           ; "keyCode", int_to_any key_code
           ; "code", Js.Unsafe.coerce (Js.string "")
           ; "key", Js.Unsafe.coerce (Js.string "")
           ; "preventDefault", Js.Unsafe.inject (Js.wrap_callback default_prevented)
           ; "target", target
           ; "getModifierState", Js.Unsafe.inject (Js.wrap_callback (fun _ -> Js._false))
           ])
  ;;

  let enter ?extra_event_fields element =
    trigger
      element
      ~event_name:"ondragenter"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let over ?extra_event_fields element =
    trigger
      element
      ~event_name:"ondragover"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let submit_form ?extra_event_fields element =
    trigger
      element
      ~event_name:"onsubmit"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let change ?extra_event_fields element ~value =
    let target = build_target ~element ~value in
    trigger
      element
      ~event_name:"onchange"
      ~extra_fields:
        (build_event_object
           ~extra_event_fields
           ~include_modifier_keys:false
           [ "target", target ])
  ;;

  let drag ?extra_event_fields element =
    trigger
      element
      ~event_name:"ondragstart"
      ~extra_fields:
        (build_event_object
           ~extra_event_fields
           ~include_modifier_keys:false
           [ "offsetX", Js.Unsafe.inject 0; "offsetY", Js.Unsafe.inject 0 ])
  ;;

  let leave ?extra_event_fields element =
    trigger
      element
      ~event_name:"ondragleave"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let drop ?extra_event_fields element =
    trigger
      element
      ~event_name:"ondrop"
      ~extra_fields:
        (build_event_object
           ~extra_event_fields
           ~include_modifier_keys:false
           [ "clientX", Js.Unsafe.inject 0; "clientY", Js.Unsafe.inject 0 ])
  ;;

  let end_ ?extra_event_fields element =
    trigger
      element
      ~event_name:"ondragend"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let mousemove ?extra_event_fields element =
    trigger
      element
      ~event_name:"onmousemove"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let mouseenter ?extra_event_fields element =
    trigger
      element
      ~event_name:"onmouseenter"
      ~extra_fields:
        (build_event_object ~extra_event_fields ~include_modifier_keys:false [])
  ;;

  let wheel ?extra_event_fields element ~delta_y =
    trigger
      element
      ~event_name:"onwheel"
      ~extra_fields:
        (build_event_object
           ~extra_event_fields
           ~include_modifier_keys:false
           [ "deltaY", Js.Unsafe.inject delta_y ])
  ;;
end
