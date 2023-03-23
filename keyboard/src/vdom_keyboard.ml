module Grouped_help_text = Grouped_help_text
module Help_text = Help_text
module Keyboard_event_handler = Keyboard_event_handler
module Keystroke = Keystroke
module Variable_keyboard_event_handler = Variable_keyboard_event_handler
module Keyboard_event = Keyboard_event

let with_keyboard_handler node keyboard_handler =
  let open Virtual_dom.Vdom in
  Node.div
    ~attrs:
      [ Attr.on_keydown (fun event ->
          Keyboard_event_handler.handle_or_ignore_event keyboard_handler event)
      ]
    [ node ]
;;
