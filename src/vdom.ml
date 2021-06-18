module Attr = struct
  include Attr
  module Hooks = Hooks
  module Global_listeners = Global_listeners
end

module Attrs = Attr.Multi
module Event = Event
module Node = Node
