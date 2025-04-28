open! Core
module Node = Node
module Attr = Attr

module Html_syntax : sig
  module Html_syntax : sig
    module Node : sig
      type t = Virtual_dom.Vdom.Node.t

      include module type of Node
      module Primitives = Virtual_dom.Vdom.Html_syntax.Html_syntax.Node.Primitives
    end

    module Attr : sig
      type t = Virtual_dom.Vdom.Attr.t

      include module type of Attr
      module Primitives = Virtual_dom.Vdom.Html_syntax.Html_syntax.Attr.Primitives
    end
  end
end
