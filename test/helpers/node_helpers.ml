open! Core_kernel
open! Js_of_ocaml

type t =
  | Text of string
  | Element of
      { tag_name : string
      ; attributes : (string * string) list [@sexp.list]
      ; string_properties : (string * string) list [@sexp.list]
      ; handlers : (string * Handler.t) list [@sexp.list]
      ; key : string option [@sexp.option]
      ; children : t list [@sexp.list]
      }
  | Widget of Sexp.t
[@@deriving sexp_of]

let is_tag ~tag = function
  | Element { tag_name; _ } -> String.equal tag_name tag
  | _ -> false
;;

let has_class ~cls = function
  | Element { attributes; _ } ->
    List.exists attributes ~f:(function
      | "class", data ->
        data |> String.split ~on:' ' |> List.exists ~f:(String.equal cls)
      | _ -> false)
  | _ -> false
;;

let rec map t ~f =
  match f t with
  | `Replace_with t -> t
  | `Continue ->
    (match t with
     | Text _ | Widget _ -> t
     | Element { tag_name; attributes; string_properties; handlers; key; children } ->
       let children = List.map children ~f:(fun ch -> map ch ~f) in
       Element { tag_name; attributes; string_properties; handlers; key; children })
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
    | Widget w ->
      let info_text = Soup.create_text (Sexp.to_string w) in
      let element = Soup.create_element "widget" ~attributes:[] in
      Soup.append_child element info_text;
      Hidden_soup element
    | Element { tag_name; attributes; string_properties; handlers; key; children } ->
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
      let attributes =
        [ key_attrs; soup_id_attrs; handler_attrs; attributes; string_properties ]
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

let to_string_html t =
  let rec recurse buffer ~depth =
    let indent = String.init (depth * 2) ~f:(Fn.const ' ') in
    function
    | Text s -> bprintf buffer "%s%s" indent s
    | Element { tag_name; attributes; string_properties; handlers; key; children } ->
      bprintf buffer "%s<%s" indent tag_name;
      Option.iter key ~f:(bprintf buffer " @key=%s");
      List.iter attributes ~f:(fun (k, v) -> bprintf buffer " %s=\"%s\"" k v);
      List.iter string_properties ~f:(fun (k, v) -> bprintf buffer " #%s=\"%s\"" k v);
      List.iter handlers ~f:(fun (k, _) -> bprintf buffer " %s={handler}" k);
      bprintf buffer ">";
      let children_should_collapse =
        List.for_all children ~f:(function
          | Text _ -> true
          | _ -> false)
        && List.fold children ~init:0 ~f:(fun acc child ->
          match child with
          | Text s -> acc + String.length s
          | _ -> acc)
           < 80 - String.length indent
      in
      let depth = if children_should_collapse then 0 else depth + 1 in
      List.iter children ~f:(fun child ->
        if children_should_collapse then bprintf buffer " " else bprintf buffer "\n";
        recurse buffer ~depth child);
      if children_should_collapse
      then bprintf buffer " "
      else (
        bprintf buffer "\n";
        bprintf buffer "%s" indent);
      bprintf buffer "</%s>" tag_name
    | Widget s -> bprintf buffer "%s<widget %s />" indent (Sexp.to_string s)
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

let unsafe_of_js_exn =
  let make_text_node (text : Js.js_string Js.t) = Text (Js.to_string text) in
  let make_element_node
        (tag_name : Js.js_string Js.t)
        (children : t Js.js_array Js.t)
        (handlers : (Js.js_string Js.t * Js.Unsafe.any) Js.js_array Js.t)
        (attributes : (Js.js_string Js.t * Js.js_string Js.t) Js.js_array Js.t)
        (string_properties : (Js.js_string Js.t * Js.js_string Js.t) Js.js_array Js.t)
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
      |> List.map ~f:(fun (k, v) -> Js.to_string k, Js.to_string v)
    in
    let string_properties =
      string_properties
      |> Js.to_array
      |> Array.to_list
      |> List.map ~f:(fun (k, v) -> Js.to_string k, Js.to_string v)
    in
    let key = key |> Js.Opt.to_option |> Option.map ~f:Js.to_string in
    Element { tag_name; children; handlers; attributes; string_properties; key }
  in
  let make_widget_node (id : _ Type_equal.Id.t) (info : Sexp.t Lazy.t option) =
    match info with
    | Some sexp -> Widget (Lazy.force sexp)
    | None -> Widget (Sexp.Atom (Type_equal.Id.name id))
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
          case 'VirtualText':
              return make_text_node(node.text);
          case 'Widget':
              return make_widget_node(node.id, node.info);
          case 'VirtualNode':
              var attributes = node.properties.attributes || {};
              var attr_list = Object.keys(attributes).map(function (key) {
                  return [0, key, attributes[key].toString()];
              });
              var children = node.children.map(function (node) {
                  return convert(node, make_text_node, make_element_node, raise_unknown_node_type);
              });
              var handlers =
                  Object.keys(node.properties)
                  .filter(function (key) {
                      // This is a bit of a hack, but it works for all the handlers that we
                      // have defined at the moment.  Consider removing the 'on' check?
                      return key.startsWith("on") && typeof node.properties[key] === 'function';
                  })
                  .map(function (key) {
                      // [0, ...] is how to generate an OCaml tuple from the JavaScript side.
                      return [0, key, node.properties[key]];
                  });
              var string_properties =
                  Object.keys(node.properties)
                  .filter(function (key) {
                      return typeof node.properties[key] === 'string';
                  })
                  .map(function (key) {
                      return [0, key, node.properties[key]]
                  });
              return make_element_node(
                  node.tagName,
                  children,
                  handlers,
                  attr_list,
                  string_properties,
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
  | Element
      { handlers
      ; tag_name = _
      ; attributes = _
      ; string_properties = _
      ; key = _
      ; children = _
      } -> handlers
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

module User_actions = struct
  let click_on node = trigger ~event_name:"onclick" node

  let input_text element ~text =
    let tag_name =
      match element with
      | Element { tag_name; _ } -> tag_name
      | other ->
        let node = to_string_html other in
        raise_s [%message (node : string) "is not an element"]
    in
    let value_element =
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
          val tagName = Js.string tag_name

          val value = Js.string text
        end)
    in
    let extra_fields = [ "target", value_element ] in
    let event_names = [ "oninput"; "onchange" ] in
    trigger_many element ~extra_fields ~event_names
  ;;
end
