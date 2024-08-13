open! Core
module Attr = Virtual_dom.Vdom.Attr
module Node = Virtual_dom.Vdom.Node

let rec wrap_in_element_if_necessary node =
  let wrap_with_div children =
    let div = Node.div children in
    match div with
    | Node.Element e -> e
    | _ -> assert false
  in
  match node with
  | Node.Text _ | Node.Widget _ | Node.None -> wrap_with_div [ node ]
  | Fragment children -> wrap_with_div children
  | Node.Lazy { t; _ } -> wrap_in_element_if_necessary (Lazy.force t)
  | Node.Element e -> e
;;

let add_style node ~style =
  let element = wrap_in_element_if_necessary node in
  Node.Element (Node.Element.add_style element style)
;;

let map_style node ~f =
  let element = wrap_in_element_if_necessary node in
  Node.Element
    (Node.Element.map_attrs element ~f:(fun attrs ->
       Attr.many_without_merge (Attr.Multi.map_style [ attrs ] ~f)))
;;

let grow n = add_style n ~style:Css_gen.(flex_item ~grow:1. ())
let grow_and_shrink n = add_style n ~style:Css_gen.(flex_item ~grow:1. ~shrink:1. ())

let scrollable n =
  add_style
    n
    ~style:
      Css_gen.(flex_item ~grow:1. ~shrink:1. () @> overflow_y `Auto @> overflow_x `Auto)
;;

let spacer ?(attrs = []) ?min_width ?min_height () =
  let style =
    let mw = Option.value_map min_width ~default:Css_gen.empty ~f:Css_gen.min_width in
    let mh = Option.value_map min_height ~default:Css_gen.empty ~f:Css_gen.min_height in
    Css_gen.concat [ mw; mh ]
  in
  Node.span ~attrs:[ Attr.many_without_merge (Attr.style style :: attrs) ] []
;;

let as_box
  (direction : [ `Row | `Column ])
  ?gap
  ?align_items
  (node_creator : Node.Aliases.node_creator)
  ?key
  ?attrs
  nodes
  =
  let nodes =
    match gap with
    | None -> nodes
    | Some gap_len ->
      let gap =
        match direction with
        | `Column -> spacer ~min_height:gap_len ()
        | `Row -> spacer ~min_width:gap_len ()
      in
      List.intersperse nodes ~sep:gap
  in
  let nodes =
    List.map
      nodes
      ~f:
        (map_style ~f:(fun style ->
           let has_flex_shrink_set =
             Css_gen.to_string_list style
             |> List.exists ~f:(fun (f, _) ->
               String.( = ) f "flex-shrink" || String.( = ) f "flex")
           in
           if has_flex_shrink_set
           then style
           else Css_gen.(style @> create ~field:"flex-shrink" ~value:"0")))
  in
  let node = node_creator ?key ?attrs nodes in
  let direction =
    (direction :> [ `Row | `Column | `Row_reverse | `Column_reverse | `Default ])
  in
  add_style node ~style:Css_gen.(flex_container ~direction ?align_items ())
;;

let as_hbox = as_box `Row
let as_vbox = as_box `Column

let hbox ?gap ?align_items ?key ?attrs children =
  as_hbox ?gap ?align_items Node.div ?key ?attrs children
;;

let vbox ?gap ?align_items ?key ?attrs children =
  as_vbox ?gap ?align_items Node.div ?key ?attrs children
;;

let body ?(direction = `Column) ?gap ?align_items ?key ?attrs nodes =
  let p100 = Percent.of_percentage 100.0 in
  as_box direction ?gap ?align_items Node.body ?key ?attrs nodes
  |> add_style
       ~style:
         Css_gen.(
           width (`Vw p100)
           @> height (`Vh p100)
           @> uniform_margin (`Px 0)
           @> uniform_padding (`Px 0))
;;

let on_grayed_out_background nodes =
  Node.div
    ~attrs:
      [ Attr.style
          Css_gen.(
            position ~left:(`Px 0) ~top:(`Px 0) `Fixed
            @> width Length.percent100
            @> height Length.percent100
            @> overflow `Auto
            @> background_color
                 (`RGBA
                   (Color.RGBA.create ~r:0 ~g:0 ~b:0 ~a:(Percent.of_percentage 40.) ())))
      ]
    nodes
;;

let modal ?(direction = `Row) ?gap ?align_items ?key ?(attrs = []) nodes =
  on_grayed_out_background
    [ as_box
        direction
        Node.div
        ?gap
        ?align_items
        ?key
        ~attrs:
          [ Attr.many_without_merge
              ([ Attr.style
                   Css_gen.(
                     margin_top (`Percent (Percent.of_percentage 15.0))
                     @> margin_left `Auto
                     @> margin_right `Auto
                     @> width (`Percent (Percent.of_percentage 80.0))
                     @> background_color (`Name "#fefefe")
                     @> uniform_padding (`Px 20)
                     @> border ~width:(`Px 2) ~style:`Solid ~color:(`Name "black") ())
               ]
               @ attrs)
          ]
        nodes
    ]
;;
