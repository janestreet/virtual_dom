module Attr = struct
  include Attr
  module Hooks = Hooks
end

module Attrs = Attr.Multi
module Event = Event
module Node = Node
