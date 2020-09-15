open Js_of_ocaml
open Base


(** The values associated with an Element and element like nodes.
    (that is in practice all nodes that aren't just text). *)
module Element : sig
  type t

  val tag : t -> string
  val attrs : t -> Attrs.t
  val key : t -> string option
  val with_key : t -> string -> t
  val map_attrs : t -> f:(Attrs.t -> Attrs.t) -> t
  val add_style : t -> Css_gen.t -> t
  val add_class : t -> string -> t
end

module Widget : sig
  type t

  module type S = sig
    type dom = private #Dom_html.element

    module Input : sig
      type t [@@deriving sexp_of]
    end

    module State : sig
      type t [@@deriving sexp_of]
    end

    val name : string
    val create : Input.t -> State.t * dom Js.t

    val update
      :  prev_input:Input.t
      -> input:Input.t
      -> state:State.t
      -> element:dom Js.t
      -> State.t * dom Js.t

    val destroy : prev_input:Input.t -> state:State.t -> element:dom Js.t -> unit
  end
end

type t =
  | None
  | Text of string
  | Element of Element.t
  | Widget of Widget.t

type node_creator = ?key:string -> Attr.t list -> t list -> t
type node_creator_childless = ?key:string -> Attr.t list -> t

val none : t
val text : string -> t
val textf : ('a, unit, string, t) format4 -> 'a
val a : node_creator
val body : node_creator
val button : node_creator
val code : node_creator
val div : node_creator
val footer : node_creator
val h1 : node_creator
val h2 : node_creator
val h3 : node_creator
val h4 : node_creator
val h5 : node_creator
val h6 : node_creator
val header : node_creator
val html : node_creator
val input : node_creator
val textarea : node_creator
val select : node_creator
val option : node_creator
val label : node_creator
val li : node_creator
val p : node_creator
val pre : node_creator
val section : node_creator
val span : node_creator
val strong : node_creator
val table : node_creator
val tbody : node_creator
val td : node_creator
val th : node_creator
val thead : node_creator
val tr : node_creator
val ul : node_creator
val ol : node_creator
val br : node_creator_childless
val hr : node_creator_childless

(** This function can be used to build a node with the tag and html content of
    that node provided as a string.  If this function was called with
    [~tag:"div" [] ~this_html...:"<b> hello world</b>"] then the resulting node would be
    [<div><b> hello world </b></div>]

    For totally sanitized content strings, this is fine; but if a user can influence
    the value of [content] and you don't have a sanitizer, they can inject code into
    the page, so use with extreme caution! *)
val inner_html
  :  tag:string
  -> Attr.t list
  -> this_html_is_sanitized_and_is_totally_safe_trust_me:string
  -> t

(** Same as [inner_html] but for svg elements *)
val inner_html_svg
  :  tag:string
  -> Attr.t list
  -> this_html_is_sanitized_and_is_totally_safe_trust_me:string
  -> t


(** [key] is used by Virtual_dom as a hint during diffing/patching *)
val create : string -> ?key:string -> Attr.t list -> t list -> t

(** Like [create] but for svg nodes (i.e. all to be placed inside <svg> tag). This is
    needed as browsers maintain separate namespaces for html and svg, and failing to use
    the correct one may result in delayed redraws. *)
val create_svg : string -> ?key:string -> Attr.t list -> t list -> t

val to_dom : t -> Dom_html.element Js.t
val to_raw : t -> Raw.Node.t

(** Creates a Node.t that has fine-grained control over the Browser DOM node.

    Callbacks
    =========

    init: Returns a Browser DOM Node and a widget state object.  The Browser
    DOM node is mounted into the dom in the location where the Node.t
    object would otherwise be.

    update: Given the previous Browser DOM Node and state, makes any changes
    necessary to either and returns a new state and Browser DOM Node.

    destroy: Called when this Node.t is removed from the Virtual_dom.
    Performs any necessary cleanup.

    Other
    =====

    The [id] is used to compare widgets, and is used to make sure that the
    state from one widget doesn't get interpreted as the state for another.
    Otherwise, you would be able to implement Obj.magic using this API.


    WARNING: While other Virtual_dom APIs shield the application from script
    injection attacks, the [Widget.create] function allows a developer to
    bypass these safeguards and manually create DOM nodes which could allow an
    attacker to change the behavior of the application or exfiltrate data.

    In using this API, you are being trusted to understand and follow security
    best-practices.
*)
val widget
  :  ?info:Sexp.t Lazy.t
  -> ?destroy:('s -> (#Dom_html.element as 'e) Js.t -> unit)
  -> ?update:('s -> 'e Js.t -> 's * 'e Js.t)
  -> id:('s * 'e Js.t) Type_equal.Id.t
  -> init:(unit -> 's * 'e Js.t)
  -> unit
  -> t

val widget_of_module
  :  (module Widget.S with type Input.t = 'input)
  -> ('input -> t) Staged.t

module Patch : sig
  type node = t
  type t

  val create : previous:node -> current:node -> t
  val apply : t -> Dom_html.element Js.t -> Dom_html.element Js.t
  val is_empty : t -> bool
end
