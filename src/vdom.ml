module Attr = struct
  include Attr
  module Hooks = Hooks
  module Global_listeners = Global_listeners
end

module Attrs = Attr.Multi
module Effect = Effect
module Node = Node
