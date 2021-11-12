module Vdom = Vdom
module Dom_float = Dom_float

module Top_level_effects = struct
  let () =
    (* use the native-javascript implementation of float -> string with a fixed number of
       numbers after the decimal place. *)
    Css_gen.Private.float_to_string_with_fixed := Dom_float.to_string_fixed
  ;;
end
