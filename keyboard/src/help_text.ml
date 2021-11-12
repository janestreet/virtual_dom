open Core
open Import

module View_spec = struct
  type t =
    { key : string -> Vdom.Node.t
    ; plain_text : string -> Vdom.Node.t
    }

  let plain =
    let open Vdom in
    { key = Node.text; plain_text = Node.text }
  ;;

  let with_classes ~key_class ~plain_text_class =
    let text_span class_ text =
      let open Vdom in
      Node.span ~attr:(Attr.class_ class_) [ Node.text text ]
    in
    { key = text_span key_class; plain_text = text_span plain_text_class }
  ;;
end

module Command = struct
  type t =
    { keys : Keystroke.t list
    ; description : string
    }
  [@@deriving sexp, compare]

  module Format = struct
    type t =
      [ `Keys of [ `Sep of string ]
      | `Description of (string -> string) option
      | `Text of string
      ]
        list

    let default =
      [ `Text "Press "
      ; `Keys (`Sep " or ")
      ; `Text " to "
      ; `Description (Some String.uncapitalize)
      ; `Text "."
      ]
    ;;
  end

  let view_keys t (view_spec : View_spec.t) ~sep =
    let keys =
      t.keys
      |> List.map ~f:Keystroke.to_string_hum
      (* Dedup keystrokes that map to the same string, e.g. Enter and NumpadEnter. *)
      |> List.dedup_and_sort ~compare:String.compare
      |> List.map ~f:view_spec.key
    in
    List.intersperse keys ~sep:(view_spec.plain_text sep)
  ;;

  let view_description ?(f = Fn.id) t (view_spec : View_spec.t) =
    view_spec.plain_text (f t.description)
  ;;

  let view t view_spec format =
    let open Vdom in
    Node.div
      (List.concat_map format ~f:(function
         | `Keys (`Sep sep) -> view_keys t view_spec ~sep
         | `Description f -> [ view_description ?f t view_spec ]
         | `Text text -> [ view_spec.plain_text text ]))
  ;;
end

type t = Command.t list [@@deriving sexp, compare]

let empty = []
let is_empty = List.is_empty
let of_command_list = Fn.id
let commands = Fn.id
let add_command t command = t @ [ command ]

let view_rows ?(sep = " or ") t (view_spec : View_spec.t) =
  let open Vdom in
  let align how = Css_gen.(text_align how) |> Attr.style in
  List.map (commands t) ~f:(fun command ->
    Node.tr
      [ Node.td
          ~attr:(align `Right)
          (Command.view_keys command view_spec ~sep @ [ view_spec.plain_text " : " ])
      ; Node.td ~attr:(align `Left) [ Command.view_description command view_spec ]
      ])
;;

let view ?sep t view_spec = Vdom.Node.table (view_rows ?sep t view_spec)
