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
      Node.span ~attrs:[ Attr.class_ class_ ] [ Node.text text ]
    in
    { key = text_span key_class; plain_text = text_span plain_text_class }
  ;;
end

(* Dedup keystrokes that map to the same string, e.g. Enter and NumpadEnter. *)
let dedup_keys keys =
  List.dedup_and_sort keys ~compare:(fun a b ->
    Comparable.lift String.compare ~f:Keystroke.to_string_hum a b)
;;

(* If a command has "consecutive" keystrokes, we display only the first and the last in
   the range instead of listing out each one. Currently we only consider digits. *)
let keys_are_consecutive (k0 : Keystroke.t) (k1 : Keystroke.t) =
  let extract_digit k =
    Option.try_with (fun () ->
      Keystroke.key k |> Keystroke.create' |> Keystroke.to_string_hum |> Int.of_string)
  in
  match extract_digit k0, extract_digit k1 with
  | None, _ | _, None -> false
  | Some digit0, Some digit1 ->
    digit0 + 1 = digit1
    && List.for_all
         Keystroke.[ ctrl; alt; shift; meta ]
         ~f:(fun modifier -> Bool.( = ) (modifier k0) (modifier k1))
;;

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
      |> dedup_keys
      |> List.group ~break:(fun a b -> not (keys_are_consecutive a b))
      |> List.map ~f:(function
        | [] -> []
        | first_key :: keys ->
          let keys = first_key :: Option.to_list (List.last keys) in
          List.map keys ~f:Keystroke.to_string_hum
          |> List.map ~f:view_spec.key
          |> List.intersperse ~sep:(view_spec.plain_text " to "))
    in
    List.intersperse keys ~sep:[ view_spec.plain_text sep ] |> List.concat
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

let group_consecutive_commands (commands : Command.t list) =
  List.group commands ~break:(fun c0 c1 ->
    match dedup_keys c0.keys, dedup_keys c1.keys with
    | [ k0 ], [ k1 ] ->
      not (keys_are_consecutive k0 k1 && String.( = ) c0.description c1.description)
    | _ -> true)
  |> List.map ~f:(fun commands ->
    { Command.keys = List.concat_map commands ~f:(fun c -> c.keys)
    ; description = (List.hd_exn commands).description
    })
;;

let view_rows ?(sep = " or ") t (view_spec : View_spec.t) =
  let open Vdom in
  let commands = group_consecutive_commands (commands t) in
  List.map commands ~f:(fun command ->
    Node.tr
      [ Node.td
          ~attrs:
            (Css_gen.[ text_align `Right; vertical_align `Middle; line_height (`Raw "1") ]
             |> List.map ~f:Attr.style)
          (Command.view_keys command view_spec ~sep @ [ view_spec.plain_text " : " ])
      ; Node.td
          ~attrs:
            (Css_gen.[ text_align `Left; vertical_align `Middle; line_height (`Raw "1") ]
             |> List.map ~f:Attr.style)
          [ Command.view_description command view_spec ]
      ])
;;

let view ?sep t view_spec = Vdom.Node.table (view_rows ?sep t view_spec)
