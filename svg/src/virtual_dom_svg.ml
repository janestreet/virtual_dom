open! Core
module Node = Node
module Attr = Attr

module Html_syntax = struct
  module Html_syntax = struct
    module Node = struct
      type t = Virtual_dom.Vdom.Node.t

      include Node
      module Primitives = Virtual_dom.Vdom.Html_syntax.Html_syntax.Node.Primitives
    end

    module Attr = struct
      type t = Virtual_dom.Vdom.Attr.t

      include Attr
      module Primitives = Virtual_dom.Vdom.Html_syntax.Html_syntax.Attr.Primitives
    end
  end
end
