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

(* https://developer.mozilla.org/en-US/docs/Glossary/Void_element *)
let is_void_element = function
  | "area"
  | "base"
  | "br"
  | "col"
  | "embed"
  | "hr"
  | "img"
  | "input"
  | "link"
  | "meta"
  | "param"
  | "source"
  | "track"
  | "wbr" -> true
  | _ -> false
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
    let formatted_key = String.chop_prefix_if_exists k ~prefix:"on" in
    bprintf buffer "@on_%s" formatted_key);
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
  match is_void_element tag_name with
  | true ->
    bprintf buffer "/>";
    fun _buffer -> ()
  | false ->
    bprintf buffer ">";
    fun buffer -> bprintf buffer "</%s>" tag_name
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

let pre_censor to_censor apply_censor kv =
  if to_censor then Tuple2.map ~f:apply_censor kv else kv
;;

let gen_filter_printed_attributes
  ?(censor_paths = true)
  ?(censor_hash = true)
  ?(path_censoring_message = "bonsai_path_replaced_in_test")
  ?(hash_censoring_message = "_hash_replaced_in_test")
  ~f
  (key, data)
  =
  match f ~key ~data with
  | true ->
    (key, data)
    |> pre_censor censor_paths (fun s ->
      Js_of_ocaml.Regexp.global_replace path_regexp s path_censoring_message)
    |> pre_censor censor_hash (fun s ->
      Js_of_ocaml.Regexp.global_replace hash_regexp s hash_censoring_message)
    |> Some
  | false -> None
;;

let to_string_html
  ?(filter_printed_attributes = fun ~key:_ ~data:_ -> true)
  ?censor_paths
  ?censor_hash
  ?path_censoring_message
  ?hash_censoring_message
  t
  =
  let filter_printed_attributes =
    gen_filter_printed_attributes
      ?censor_paths
      ?censor_hash
      ?path_censoring_message
      ?hash_censoring_message
      ~f:filter_printed_attributes
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
      let single_line_close =
        bprint_element_single_line ~filter_printed_attributes single_line_buffer element
      in
      let close =
        if Buffer.length single_line_buffer < 100 - String.length indent
        then (
          Buffer.add_buffer buffer single_line_buffer;
          single_line_close)
        else bprint_element_multi_line ~filter_printed_attributes buffer ~indent element
      in
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
      close buffer
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

class type ['a] prop = object
  method key : Js.js_string Js.t Js.prop
  method value : 'a Js.prop
end

let with_prop f p = f (p##.key, p##.value)

let rec unsafe_of_js_exn =
  let make_text_node (text : Js.js_string Js.t) = Text (Js.to_string text) in
  let make_element_node
    (tag_name : Js.js_string Js.t)
    (children : t Js.js_array Js.t)
    (handlers : Js.Unsafe.any prop Js.t Js.js_array Js.t)
    (attributes : Js.js_string Js.t prop Js.t Js.js_array Js.t)
    (string_properties : Js.js_string Js.t prop Js.t Js.js_array Js.t)
    (bool_properties : bool Js.t prop Js.t Js.js_array Js.t)
    (styles : Js.js_string Js.t prop Js.t Js.js_array Js.t)
    (hooks : Vdom.Attr.Hooks.For_testing.Extra.t prop Js.t Js.js_array Js.t)
    (key : Js.js_string Js.t Js.Opt.t)
    =
    let tag_name = tag_name |> Js.to_string in
    let children = children |> Js.to_array |> Array.to_list in
    let handlers =
      handlers
      |> Js.to_array
      |> Array.to_list
      |> List.map
           ~f:
             (with_prop
              @@ fun (s, h) ->
              let name = Js.to_string s in
              name, Handler.of_any_exn h ~name)
    in
    let attributes =
      attributes
      |> Js.to_array
      |> Array.to_list
      |> List.map
           ~f:
             (with_prop
              @@ fun (k, v) ->
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
      |> List.map ~f:(with_prop @@ fun (k, v) -> Js.to_string k, v)
    in
    let string_properties =
      string_properties
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(with_prop @@ fun (k, v) -> Js.to_string k, Js.to_string v)
    in
    let bool_properties =
      bool_properties
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(with_prop @@ fun (k, v) -> Js.to_string k, Js.to_bool v)
    in
    let styles =
      styles
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(with_prop @@ fun (k, v) -> Js.to_string k, Js.to_string v)
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
                   return {key:key, value:attributes[key].toString()};
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
                       return {key:key, value:node.properties[key]};
                   });
               var string_properties =
                   Object.keys(node.properties)
                   .filter(function(key) {
                       return typeof node.properties[key] === 'string';
                   })
                   .map(function(key) {
                       return {key:key, value:node.properties[key]};
                   });
               var bool_properties =
                   Object.keys(node.properties)
                   .filter(function(key) {
                     return typeof node.properties[key] === 'boolean';
                   })
                   .map(function(key) {
                      return {key:key, value:node.properties[key]};
                   });
               var styles =
                   Object.keys(node.properties.style ? node.properties.style : {})
                   .filter(function(key) {
                       return typeof node.properties.style[key] === 'string';
                   })
                   .map(function(key) {
                       return {key, value:node.properties.style[key]}
                   });
               var hooks =
                   Object.keys(node.properties)
                   .filter(function(key) {
                       return typeof node.properties[key] === 'object' &&
                           typeof node.properties[key]['extra'] === 'object';
                   })
                   .map(function(key) {
                       return {key, value:node.properties[key]['extra']}
                   });
               var soft_set_hooks =
                   Object.keys(node.properties)
                   .filter(function(key) {
                     return node.properties[key] instanceof joo_global_object.SoftSetHook;
                   })
                   .map(function(key) {
                     return {key, value:"" + node.properties[key].value};
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

let get_element_hook_value
  : type a. element -> type_id:a Type_equal.Id.t -> name:string -> a option
  =
  fun { hooks; _ } ~type_id ~name ->
  match List.Assoc.find ~equal:String.equal hooks name with
  | Some hook ->
    let (Vdom.Attr.Hooks.For_testing.Extra.T { type_id = type_id_v; value }) = hook in
    (match Type_equal.Id.same_witness type_id_v type_id with
     | Some T -> Some value
     | None ->
       failwithf
         "get_hook_value: a hook for %s was found, but the type-ids were not the same; \
          are you using the same type-id that you got from the For_testing module from \
          your hook creator?"
         name
         ())
  | None -> None
;;

let get_hook_value_opt t ~type_id ~name =
  match t with
  | Element e -> get_element_hook_value e ~type_id ~name
  | Text _ -> failwith "get_hook_value: expected Element, found Text"
  | Widget -> failwith "get_hook_value: expected Element, found Widget"
;;

let get_hook_value t ~type_id ~name =
  match t with
  | Element e ->
    (match get_element_hook_value e ~type_id ~name with
     | Some v -> v
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

  let input_files ?(extra_event_fields = []) element ~files =
    let value =
      match files with
      | [] -> ""
      | (file : File.file Js.t) :: _ -> Js.to_string file##.name
    in
    let target =
      Js.Unsafe.inject
        (object%js
           val tagName = Js.string (tag_name_exn element)
           val value = Js.string value

           val files =
             object%js
               val length = Js.number_of_float (Float.of_int (List.length files))
               method item i = List.nth_exn files (Js.float_of_number i |> Int.of_float)
             end
        end)
    in
    let event_names = [ "oninput"; "onchange" ] in
    trigger_many
      element
      ~event_names
      ~extra_fields:([ "target", target ] @ extra_event_fields)
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
           [ "deltaY", Js.Unsafe.inject (Js.float delta_y) ])
  ;;
end

module Linter = struct
  module Outcome = struct
    type t =
      | Pass
      | Fail
  end

  module Severity = struct
    type t =
      | Only_report_app_crashing_errors
      | Report_all_errors
    [@@deriving equal, compare]

    let to_string = function
      | Only_report_app_crashing_errors -> "Fatal"
      | Report_all_errors -> "High"
    ;;

    let at_least ~compare_to t = compare t compare_to <= 0

    let%expect_test "more severe than" =
      assert (at_least ~compare_to:Report_all_errors Only_report_app_crashing_errors);
      assert (at_least ~compare_to:Report_all_errors Report_all_errors);
      assert (
        at_least
          ~compare_to:Only_report_app_crashing_errors
          Only_report_app_crashing_errors)
    ;;
  end

  module Rule = struct
    type t =
      | Undetectable_clickable_element
      | Invalid_tabindex
      | Event_handler_html_attribute
      | Duplicate_ids
      | Whitespace_in_id
      | Siblings_have_same_vdom_key
      | Unsafe_target_blank
      | Clickable_role_but_no_tabindex
      | Button_without_valid_type
    [@@deriving hash, compare, sexp, equal, enumerate, string ~capitalize:"Sentence case"]

    let severity = function
      | Undetectable_clickable_element -> Severity.Report_all_errors
      | Invalid_tabindex -> Report_all_errors
      | Event_handler_html_attribute -> Report_all_errors
      | Duplicate_ids -> Report_all_errors
      | Whitespace_in_id -> Report_all_errors
      | Siblings_have_same_vdom_key -> Only_report_app_crashing_errors
      | Unsafe_target_blank -> Report_all_errors
      | Clickable_role_but_no_tabindex -> Report_all_errors
      | Button_without_valid_type -> Report_all_errors
    ;;

    let recommendation = function
      | Undetectable_clickable_element ->
        String.strip
          {|
This element has a "click" event listener, but does not have an HTML tag or
role that indicates that it's accessible (e.g. `a`, `button`, a form control, etc).
Accessibility tools, such as Vimium, might not detect this element.

Likely fix: Add a `role="button"` attribute, or use a `<button />` tag instead of
`<div />`.|}
      | Invalid_tabindex -> "Only use `0` and `-1` as tabindex values."
      | Event_handler_html_attribute ->
        let ppx_html_string = "{%html|<button on_click=%{...}></button>|}" in
        String.strip
          {%string|
Event handling via literal `onclick`, `onfocus`, `onblur`, etc
HTML attributes is bad practice, and will likely result in CSP errors.

You should attach event handlers via `Vdom.Attr.on_*` functions, or
%{ppx_html_string}, which attaches event listeners programmatically.
|}
      | Duplicate_ids ->
        String.strip
          {|
Do not use the same value for an HTML id attribute more than once.|}
      | Whitespace_in_id -> "HTML IDs must not contain spaces."
      | Siblings_have_same_vdom_key ->
        String.strip
          {|
Sibling vdom nodes MUST NOT have the same key. This will crash your web app at runtime.|}
      | Unsafe_target_blank ->
        String.strip
          {|
HTML `<a />` links may only have `target="_blank"` if they are relative links, or
`rel="noopener noreferrer"` is set. For more info, see:
https://hackernoon.com/unsafe-use-of-target_blank-39413ycf

Likely fix: either use a relative link, or specify `rel="noopener noreferrer"`|}
      | Clickable_role_but_no_tabindex ->
        String.strip
          {|
This element has a role attribute that marks it as clickable, but has not set
tabindex="0". This means it will not be focusable via tab, which negatively impacts
keyboard users.

Likely fix: add tabindex="0".|}
      | Button_without_valid_type ->
        String.strip
          {|
HTML `<button />` elements should explicitly specify a `type` attribute, which should be
"button", "submit", or "reset". If not set, the browser will use "submit" by default,
which will cause the button to submit any `<form />`s that contain it when clicked.

Likely fix: add a `type="button"` attribute. |}
    ;;

    let display ~failure_expected t =
      let expected_message = if failure_expected then " (failure expected)" else "" in
      let first_line =
        [%string "[%{Severity.to_string (severity t)}] %{to_string t}%{expected_message}"]
      in
      let spacer = String.init (String.length first_line) ~f:(fun _ -> '-') in
      [%string "%{first_line}\n%{spacer}\n%{recommendation t}"]
    ;;
  end

  module Test = struct
    module Node_context = struct
      type 'a t =
        { acc : 'a
        ; rules_broken : Rule.t Hash_set.t
        }
    end

    type t =
      | T :
          { rule : Rule.t
          ; f : 'a Node_context.t -> element -> Outcome.t * 'a
          ; mutable acc : 'a
          }
          -> t

    let create rule ~init ~f = T { rule; f; acc = init }
    let create_isolated rule ~f = create rule ~init:() ~f:(fun _ helper -> f helper, ())
    let rule (T { rule; _ }) = rule

    let rules_broken ts element =
      let rules_broken = Hash_set.create (module Rule) in
      List.filter ts ~f:(fun (T test) ->
        let outcome, acc = test.f { acc = test.acc; rules_broken } element in
        test.acc <- acc;
        match outcome with
        | Pass -> false
        | Fail -> true)
      |> List.iter ~f:(fun (T { rule; _ }) -> Hash_set.add rules_broken rule);
      rules_broken
    ;;

    let severity_at_least ~min_severity (T { rule; _ }) =
      let severity = Rule.severity rule in
      Severity.at_least ~compare_to:min_severity severity
    ;;
  end

  let pass_if cond = if cond then Outcome.Pass else Fail
  let fail_if cond = pass_if (not cond)

  let clickable_tags =
    Set.of_list
      (module String)
      [ "a"; "textarea"; "input"; "button"; "select"; "object"; "embed"; "details" ]
  ;;

  let clickable_roles =
    Set.of_list
      (module String)
      [ "button"
      ; "tab"
      ; "link"
      ; "checkbox"
      ; "menuitem"
      ; "menuitemcheckbox"
      ; "menuitemradio"
      ; "radio"
      ]
  ;;

  let test_unrecognized_clickable = function
    | { handlers; _ } when not (List.Assoc.mem ~equal:String.equal handlers "onclick") ->
      Outcome.Pass
    | { tag_name; _ } when Set.mem clickable_tags tag_name -> Pass
    | { attributes; _ } ->
      (match List.Assoc.find attributes "role" ~equal:String.equal with
       | None -> Fail
       | Some role_val ->
         let has_clickable_role =
           String.split role_val ~on:' ' |> List.exists ~f:(Set.mem clickable_roles)
         in
         pass_if has_clickable_role)
  ;;

  let get_attr = List.Assoc.find ~equal:String.equal
  let uri_has_scheme str = Uri.of_string str |> Uri.scheme |> Option.is_some

  open struct
    open Outcome

    let test_invalid_tab_index { attributes; _ } =
      match get_attr attributes "tabindex" with
      | None -> Pass
      | Some tabindex -> pass_if String.(tabindex = "0" || tabindex = "-1")
    ;;

    let test_event_handler_html_attribute { attributes; _ } =
      List.exists attributes ~f:(fun (name, _) ->
        match String.chop_prefix name ~prefix:"on" with
        | None -> false
        | Some rest -> String.for_all rest ~f:Char.is_alpha)
      |> fail_if
    ;;

    let test_whitespace_in_id { attributes; _ } =
      match get_attr attributes "id" with
      | None -> Pass
      | Some id -> fail_if (String.contains id ' ')
    ;;

    let test_clickable_role_but_no_tabindex { attributes; _ } =
      match get_attr attributes "role", get_attr attributes "tabindex" with
      | None, None -> Pass
      | Some role, None when Set.mem clickable_roles role -> Fail
      | Some _, None | _, Some _ -> Pass
    ;;

    let test_button_without_valid_type { tag_name; attributes; _ } =
      match tag_name with
      | "button" ->
        (match get_attr attributes "type" with
         | Some "button" | Some "submit" | Some "reset" -> Pass
         | _ -> Fail)
      | _ -> Pass
    ;;

    let test_unsafe_target_blank = function
      | { tag_name = "a"; attributes; _ } ->
        (match
           ( get_attr attributes "target"
           , get_attr attributes "href"
           , get_attr attributes "rel" )
         with
         | None, _, _ -> Outcome.Pass
         | Some "_blank", Some href, _ when not (uri_has_scheme href) -> Pass
         | Some "_blank", _, Some rel
           when List.for_all [ "noopener"; "noreferrer" ] ~f:(fun substring ->
                  String.is_substring ~substring rel) -> Pass
         | Some "_blank", _, _ -> Fail
         | _ -> Pass)
      | _ -> Pass
    ;;
  end

  let test_for_rule : Rule.t -> Test.t =
    fun rule ->
    let isolated = Test.create_isolated rule in
    match rule with
    | Undetectable_clickable_element -> isolated ~f:test_unrecognized_clickable
    | Invalid_tabindex -> isolated ~f:test_invalid_tab_index
    | Event_handler_html_attribute -> isolated ~f:test_event_handler_html_attribute
    | Whitespace_in_id -> isolated ~f:test_whitespace_in_id
    | Duplicate_ids ->
      Test.create
        rule
        ~init:String.Map.empty
        ~f:(fun { acc; rules_broken = curr_node_rules_broken } { attributes; _ } ->
          match get_attr attributes "id" with
          | None -> Pass, acc
          | Some id ->
            (match Map.find acc id with
             | Some first_occurence_rules_broken ->
               Hash_set.add first_occurence_rules_broken Rule.Duplicate_ids;
               Fail, acc
             | None ->
               (* The match asserts that this key is not in the map *)
               Pass, Map.add_exn acc ~key:id ~data:curr_node_rules_broken))
    | Siblings_have_same_vdom_key ->
      Test.create rule ~init:[] ~f:(fun { acc; _ } ({ children; _ } as self) ->
        let failing_children =
          List.filter_map children ~f:(function
            | Text _ | Widget | Element { key = None; _ } -> None
            | Element ({ key = Some key; _ } as e) -> Some (key, e))
          |> Map.of_alist_multi (module String)
          |> Map.data
          |> List.filter ~f:(fun data -> List.length data > 1)
          |> List.concat
        in
        let result, acc_without_self =
          match List.mem acc self ~equal:phys_equal with
          | true -> Outcome.Fail, List.filter acc ~f:(fun e -> not (phys_equal e self))
          | false -> Pass, acc
        in
        result, failing_children @ acc_without_self)
    | Unsafe_target_blank -> isolated ~f:test_unsafe_target_blank
    | Clickable_role_but_no_tabindex -> isolated ~f:test_clickable_role_but_no_tabindex
    | Button_without_valid_type -> isolated ~f:test_button_without_valid_type
  ;;

  let tests = lazy (List.map Rule.all ~f:test_for_rule)

  let%expect_test "All rules accounted for" =
    let all_rules = Rule.all in
    let rules_tested = force tests |> List.map ~f:Test.rule in
    assert (List.length all_rules = List.length rules_tested);
    List.iter all_rules ~f:(fun rule ->
      assert (List.exists rules_tested ~f:(Rule.equal rule)));
    [%expect {| |}]
  ;;

  module Tree = struct
    type t =
      | Node of
          { element : element
          ; children : t list
              (* [rules_broken] is mutable because we might only know that a rule is broken
                 after the [Tree.t] has been built. *)
          ; rules_broken : Rule.t Hash_set.t
          }
      | Empty

    let build ~min_severity helper =
      let tests = force tests |> List.filter ~f:(Test.severity_at_least ~min_severity) in
      let rec loop helper =
        match helper with
        | Text _ | Widget -> Empty
        | Element ({ children; _ } as element) ->
          let rules_broken = Test.rules_broken tests element in
          let unpruned_children = List.map children ~f:loop in
          let pruned_children =
            List.fold unpruned_children ~init:[] ~f:(fun acc child ->
              match acc, child with
              | Empty :: _, Empty -> acc
              | _, _ -> child :: acc)
            |> List.rev
          in
          (match pruned_children, Hash_set.to_list rules_broken with
           | [], [] | [ Empty ], [] -> Empty
           | [ Empty ], _ -> Node { element; rules_broken; children = [] }
           | _, _ -> Node { element; rules_broken; children = pruned_children })
      in
      loop helper
    ;;

    let filter_printed_attributes =
      gen_filter_printed_attributes
        ~censor_paths:false
        ~censor_hash:false
        ~f:(fun ~key:_ ~data:_ -> true)
    ;;

    let rec to_string_hum ~depth ~buf t =
      let recurse ?(incr_depth_by = 1) t =
        to_string_hum ~depth:(depth + incr_depth_by) ~buf t
      in
      let pad ?(depth_offset = 0) () =
        Buffer.add_char buf '\n';
        Fn.apply_n_times
          ~n:((depth + depth_offset) * 2)
          (fun () -> Buffer.add_char buf ' ')
          ()
      in
      let print_error rules =
        let errors_string =
          Hash_set.to_list rules |> List.map ~f:Rule.to_string |> String.concat ~sep:", "
        in
        bprintf buf " <- [ERRORS]: %s" errors_string
      in
      pad ();
      match t with
      | Empty -> bprintf buf "..."
      | Node { rules_broken; children = []; _ } when Hash_set.is_empty rules_broken ->
        bprintf buf "ERROR_IN_LINTER"
      | Node { element; rules_broken; children = [] } ->
        let close = bprint_element_single_line ~filter_printed_attributes buf element in
        close buf;
        print_error rules_broken
      | Node { element; rules_broken; children } ->
        (match Hash_set.to_list rules_broken with
         | [] -> bprintf buf "<%s>" element.tag_name
         | _ ->
           let close =
             bprint_element_single_line ~filter_printed_attributes buf element
           in
           close buf;
           print_error rules_broken);
        List.iter children ~f:recurse;
        pad ();
        bprintf buf "</%s>" element.tag_name
    ;;
  end

  let get_all_broken_rules tree =
    let all_rules_broken = Hash_set.create (module Rule) in
    let rec loop = function
      | Tree.Empty -> ()
      | Node { rules_broken; children; _ } ->
        Hash_set.iter rules_broken ~f:(Hash_set.add all_rules_broken);
        List.iter children ~f:loop
    in
    loop tree;
    Hash_set.to_list all_rules_broken
  ;;

  let run ?(expected_failures = []) ?(min_severity = Severity.Report_all_errors) helper =
    match Tree.build ~min_severity helper with
    | Empty -> None
    | tree ->
      let buf = Buffer.create 1024 in
      let expected_failures, unexpected_failures =
        get_all_broken_rules tree
        |> List.partition_tf ~f:(fun rule ->
          List.mem ~equal:Rule.equal expected_failures rule)
      in
      let display_and_sort ~failure_expected rules =
        List.sort rules ~compare:(fun a b ->
          Severity.compare (Rule.severity a) (Rule.severity b))
        |> List.map ~f:(Rule.display ~failure_expected)
      in
      let title =
        if List.is_empty unexpected_failures
        then "Linting Failures:\n"
        else
          "(* C$R require-failed: UNEXPECTED LINTER ERRORS *)\n"
          |> String.substr_replace_first ~pattern:"$" ~with_:""
      in
      bprintf buf "%s" title;
      Tree.to_string_hum ~depth:0 ~buf tree;
      bprintf buf "\n\n";
      display_and_sort ~failure_expected:false unexpected_failures
      @ display_and_sort ~failure_expected:true expected_failures
      |> List.intersperse ~sep:"\n\n"
      |> List.iter ~f:(bprintf buf "%s");
      Some (Buffer.contents buf)
  ;;

  let print_report
    ?expected_failures
    ?min_severity
    ?(on_ok = fun () -> print_endline "ok!")
    helper
    =
    match run ?expected_failures ?min_severity helper with
    | None -> on_ok ()
    | Some report -> print_endline report
  ;;
end
