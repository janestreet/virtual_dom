open! Core
module Node = Node
module Attr = Attr

module Html_syntax : sig
  module Html_syntax : sig
    module Node : sig
      type t = Virtual_dom.Vdom.Node.t

      include module type of Node
    end

    module Attr : sig
      type t = Virtual_dom.Vdom.Attr.t

      include module type of Attr
    end
  end
end
