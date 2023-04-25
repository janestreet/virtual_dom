## Release v0.16.0

The most significant change in this release is that most functions that create
`Vdom.Node.t` have changed from taking a single attribute via a `?attr`
argument to now take a list of attributes via the `?attrs` argument. This
change was made to reduce the boilerplate at each callsite of invoking
`Vdom.Attr.many` to add more than one attribute.

A straightforward way to upgrade code to the new interface, without changing
behavior, is to rename `~attr` to `~attrs` and wrap the argument into a
singlton list. Additionally, in the case where the argument was an immediate
invocation of `Vdom.Attr.many`, you can just remove that call and pass the list
directly.

We've applied this interface change to the main `virtual_dom` library, as well
as `vdom_layout`. However, we took a different approach with
`vdom_input_widgets`, since functions in that module already take a list of
attributes, but they do not merge the attributes in those lists. We've added a
`?merge_behavior` argument to those functions. To completely preserve behavior,
you should pass `Legacy_dont_merge`; otherwise, the default `Merge` behavior
will attempt to concatenate any styles or lists of classes in the attributes.

More minor changes include:

- `css_gen` changes:
  * Added `text_align` and `content_alignment` types.
  * Added `line_height`, `row_gap`, and `column_gap` functions.
  * `flex_container` now accepts `` `Default `` for the `?wrap` and
    `?direction` arguments. It also now accepts some new arguments:
    `?align_content`, `?row_gap`, and `?column_gap`.
- Added new `html5_history` library, which provides a layer on top of the
  browser's history API.
- Added `Vdom_keyboard.Keyboard_event_handler.get_action`.
- `vdom_input_widgets` changes:
  * The `Dropdown` module  now accepts `?extra_option_attrs` and `?placeholder` arguments.
  * The `on_toggle` argument to `Checkbox.simple` is now a plain effect instead
    of a function, since the function was expected to be pure anyway.
  * Renamed `Radio_buttons.Style` module to `Selectable_style`.
  * The `Checklist` module now accepts an argument `Selectable_style.t` argument.
  * Added `Entry.password`.
- `virtual_dom` changes:
  * Added more attributes creation functions to the `Attr` module.
  * Added more event listener functions to the `Attr.Global_listeners` module.
  * Added `Node.Widget.to_vdom_for_testing`.
  * Added `Lazy` constructor to `Node.t`.
  * Added more node-creation functions to the `Node` module.
  * Added `Node.lazy_`, a function that allows for writing code to compute
    virtual-dom nodes that only runs if necessary.
  * `Node.input` no longer accepts a list of child nodes. Users must either
    stop passing the list, or switch to using the behavior-preserving
    `Node.input_deprecated` function.
- Added `Virtual_dom_svg.Attr.stroke_dashoffset`.
