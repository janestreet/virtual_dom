open! Core
module Attr := Virtual_dom.Vdom.Attr
module Node := Virtual_dom.Vdom.Node

type node_creator := ?key:string -> ?attrs:Attr.t list -> Node.t list -> Node.t

(** If node is an Element, do nothing other than returning that Element. Otherwise put a
    div around the node and return the div's Element. *)
val wrap_in_element_if_necessary : Node.t -> Node.Element.t

(** Change the style attribute of a node. Calls [wrap_in_element_if_necessary]. *)
val add_style : Node.t -> style:Css_gen.t -> Node.t

(** Turn a node_creator into a node_creator that sets the style information on the node to
    be a flexbox. We also set the flex attribute flex-shrink to 0 if it isn't set (but we
    leave it alone if it happens to be set). *)
val as_box
  :  [ `Row | `Column ]
  -> ?gap:Css_gen.Length.t
  -> ?align_items:Css_gen.item_alignment
  -> node_creator
  -> node_creator

val as_hbox
  :  ?gap:Css_gen.Length.t
  -> ?align_items:Css_gen.item_alignment
  -> node_creator
  -> node_creator

val as_vbox
  :  ?gap:Css_gen.Length.t
  -> ?align_items:Css_gen.item_alignment
  -> node_creator
  -> node_creator

(** Set flex-grow to 1 on the given Node. *)
val grow : Node.t -> Node.t

(** Set flex-grow and flex-shrink to 1 on the given Node. *)
val grow_and_shrink : Node.t -> Node.t

(** Make this a child of a flexbox that can scroll (e.g. if its content is bigger than the
    size given to it by the parent container, scrollbars will appear) *)
val scrollable : Node.t -> Node.t

(** Convenience wrapper same as [as_hbox Node.div]. *)
val hbox : ?gap:Css_gen.Length.t -> ?align_items:Css_gen.item_alignment -> node_creator

(** Convenience wrapper same as [as_vbox Node.div]. *)
val vbox : ?gap:Css_gen.Length.t -> ?align_items:Css_gen.item_alignment -> node_creator

(** a blank div element *)
val spacer
  :  ?attrs:Attr.t list
  -> ?min_width:Css_gen.Length.t
  -> ?min_height:Css_gen.Length.t
  -> unit
  -> Node.t

(** box (default direction=`Column), width = 100vh, height = 100vh *)
val body
  :  ?direction:[ `Row | `Column ]
  -> ?gap:Css_gen.Length.t
  -> ?align_items:Css_gen.item_alignment
  -> node_creator

(** Display nodes (layouted as a box) in a smaller window on top of everything else (with
    everything else grayed out and inaccessible via the mouse). Note that this renders a
    border but no control elements (not even a button to close the window) *)
val modal
  :  ?direction:[ `Row | `Column ]
  -> ?gap:Css_gen.Length.t
  -> ?align_items:Css_gen.item_alignment
  -> node_creator
