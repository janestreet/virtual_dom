module Attr = struct
  include Attr
  module Hooks = Hooks
  module Global_listeners = Global_listeners
end

module Attrs = Attr.Multi
module Effect = Effect

module Node = struct
  include Node
  module Map_children = Vdom_node_with_map_children
end

module Html_syntax = struct
  module Html_syntax = struct
    module Attr : sig
      include
        module type of Attr
        with type t := Attr.t
         and module Multi := Attr.Multi
         and module Always_focus_hook := Attr.Always_focus_hook
         and module Single_focus_hook := Attr.Single_focus_hook
         and module No_op_hook := Attr.No_op_hook
         and module Expert := Attr.Expert

      module Primitives : sig
        val create : here:[%call_pos] -> string -> string -> Attr.t
        val empty : Attr.t
        val many : Attr.t list -> Attr.t
      end
    end = struct
      include Attr

      module Primitives = struct
        let create = create
        let empty = empty
        let many = many
      end
    end

    module Node : sig
      include
        module type of Node
        with type t := Node.t
         and module Element := Node.Element
         and module Widget := Node.Widget
         and module Aliases := Node.Aliases
         and module Patch := Node.Patch
         and module Expert := Node.Expert

      module Primitives : sig
        val text : string -> Node.t
        val none : Node.t
        val fragment : Node.t list -> Node.t
      end
    end = struct
      include Node

      module Primitives = struct
        let text = text
        let none = none
        let fragment = fragment
      end
    end
  end
end
