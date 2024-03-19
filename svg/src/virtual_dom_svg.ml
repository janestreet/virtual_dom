open! Core
module Node = Node
module Attr = Attr

module Html_syntax = struct
  module Html_syntax = struct
    module Node = struct
      type t = Virtual_dom.Vdom.Node.t

      include Node
    end

    module Attr = struct
      type t = Virtual_dom.Vdom.Attr.t

      include Attr
    end
  end
end
