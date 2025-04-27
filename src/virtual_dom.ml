module Vdom = Vdom
module Dom_float = Dom_float
module Js_map = Js_map

module Top_level_effects = struct
  let () =
    (* use the native-javascript implementation of float -> string with a fixed number of
       numbers after the decimal place. *)
    Css_gen.Private.float_to_string_with_fixed := Dom_float.to_string_fixed
  ;;
end

module For_testing = struct
  let num_queued_on_mount_tasks = On_mount.For_testing.num_queued_tasks
end
