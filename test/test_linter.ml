open! Core
open Virtual_dom
open Vdom.Html_syntax

let test ?min_severity x =
  Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn x
  |> Virtual_dom_test_helpers.Node_helpers.Linter.print_report
       ~expected_failures:
         [ Undetectable_clickable_element
         ; Invalid_tabindex
         ; Event_handler_html_attribute
         ; Duplicate_ids
         ; Whitespace_in_id
         ; Siblings_have_same_vdom_key
         ; Unsafe_target_blank
         ; Button_without_valid_type
         ; Clickable_role_but_no_tabindex
         ]
       ?min_severity
;;

let on_click _ = Ui_effect.Ignore

let%expect_test "basic success" =
  test {%html|<div>Hello!</div>|};
  [%expect {| ok! |}]
;;

let%expect_test "button success" =
  test {%html|<button type="button" on_click=%{on_click}>Hello!</button>|};
  [%expect {| ok! |}]
;;

let%expect_test "basic failure" =
  test {%html|<div on_click=%{on_click}>Hello!</div>|};
  [%expect
    {|
    Linting Failures:

    <div @on_click></div> <- [ERRORS]: Undetectable clickable element

    [High] Undetectable clickable element (failure expected)
    --------------------------------------------------------
    This element has a "click" event listener, but does not have an HTML tag or
    role that indicates that it's accessible (e.g. `a`, `button`, a form control, etc).
    Accessibility tools, such as Vimium, might not detect this element.

    Likely fix: Add a `role="button"` attribute, or use a `<button />` tag instead of
    `<div />`.
    |}]
;;

let%expect_test "good tabindex" =
  test
    {%html|
      <div>
        <div tabindex=%{0}>Hello!</div>
        <div tabindex=%{-1}>Hello!</div>
      </div>
    |};
  [%expect {| ok! |}]
;;

let%expect_test "bad tabindex" =
  test
    {%html|
      <div>
        <div tabindex=%{0}>Hello!</div>
        <div tabindex=%{1}>Hello!</div>
        <div tabindex=%{-12}>Hello!</div>
        <div tabindex=%{5}>Hello!</div>
      </div>
    |};
  [%expect
    {|
    Linting Failures:

    <div>
      ...
      <div tabindex="1"></div> <- [ERRORS]: Invalid tabindex
      <div tabindex="-12"></div> <- [ERRORS]: Invalid tabindex
      <div tabindex="5"></div> <- [ERRORS]: Invalid tabindex
    </div>

    [High] Invalid tabindex (failure expected)
    ------------------------------------------
    Only use `0` and `-1` as tabindex values.
    |}]
;;

let%expect_test "bad event listener attribute" =
  test
    {%html|
      <div>
        <div %{Vdom.Attr.create "onclick" ""}>Hello!</div>
        <div %{Vdom.Attr.create "onbeforeunload" ""}>Hello!</div>
        <div %{Vdom.Attr.create "ongfdgdsfgsdfgs" ""}>Hello!</div>
      </div>
    |};
  [%expect
    {xxx|
    Linting Failures:

    <div>
      <div onclick=""></div> <- [ERRORS]: Event handler html attribute
      <div onbeforeunload=""></div> <- [ERRORS]: Event handler html attribute
      <div ongfdgdsfgsdfgs=""></div> <- [ERRORS]: Event handler html attribute
    </div>

    [High] Event handler html attribute (failure expected)
    ------------------------------------------------------
    Event handling via literal `onclick`, `onfocus`, `onblur`, etc
    HTML attributes is bad practice, and will likely result in CSP errors.

    You should attach event handlers via `Vdom.Attr.on_*` functions, or
    {%html|<button on_click=%{...}></button>|}, which attaches event listeners programmatically.
    |xxx}]
;;

let%expect_test "bad target blank" =
  test
    {%html|
      <div>
        <div class="unsafe">
          <a href="https://example.com" target="_blank">Example</a>
          <a href="mailto://example.com" target="_blank">Example</a>
          <a href="https://example.com" target="_blank" rel="nofollow">Example</a>
          <a href="https://example.com" target="_blank" rel="noopener opener"
            >Example</a
          >
          <a href="https://example.com" target="_blank" rel="noreferrer">Example</a>
          <a href="https://example.com" target="_blank" rel="noopener">Example</a>
        </div>
        <div class="safe">
          <a href="https://example.com">Example</a>
          <a href="https://example.com" target="_blank" rel="noopener noreferrer"
            >Example</a
          >
          <a href="https://example.com" target="_blank" rel="noreferrer noopener"
            >Example</a
          >
          <a href="/relative-path" target="_blank">Relative Safe 1</a>
          <a href="/another-relative-path" target="_blank">Relative Safe 2</a>
        </div>
      </div>
    |};
  [%expect
    {xxx|
    Linting Failures:

    <div>
      <div>
        <a href="https://example.com" target="_blank"></a> <- [ERRORS]: Unsafe target blank
        <a href="mailto://example.com" target="_blank"></a> <- [ERRORS]: Unsafe target blank
        <a href="https://example.com" target="_blank" rel="nofollow"></a> <- [ERRORS]: Unsafe target blank
        <a href="https://example.com" target="_blank" rel="noopener opener"></a> <- [ERRORS]: Unsafe target blank
        <a href="https://example.com" target="_blank" rel="noreferrer"></a> <- [ERRORS]: Unsafe target blank
        <a href="https://example.com" target="_blank" rel="noopener"></a> <- [ERRORS]: Unsafe target blank
      </div>
      ...
    </div>

    [High] Unsafe target blank (failure expected)
    ---------------------------------------------
    HTML `<a />` links may only have `target="_blank"` if they are relative links, or
    `rel="noopener noreferrer"` is set. For more info, see:
    https://hackernoon.com/unsafe-use-of-target_blank-39413ycf

    Likely fix: either use a relative link, or specify `rel="noopener noreferrer"`
    |xxx}]
;;

let%expect_test "whitespace in id" =
  test
    {%html|
      <div>
        <div id=" some_id">Hello!</div>
        <div id="some_id ">Hello!</div>
        <div id="id1 id2 id3 id4">Hello!</div>
      </div>
    |};
  [%expect
    {|
    Linting Failures:

    <div>
      <div id=" some_id"></div> <- [ERRORS]: Whitespace in id
      <div id="some_id "></div> <- [ERRORS]: Whitespace in id
      <div id="id1 id2 id3 id4"></div> <- [ERRORS]: Whitespace in id
    </div>

    [High] Whitespace in id (failure expected)
    ------------------------------------------
    HTML IDs must not contain spaces.
    |}]
;;

let%expect_test "duplicate ids" =
  test
    {%html|
      <div>
        <div id="id">
          Hello!
          <div id="id">Hello!</div>
        </div>
        <div id="id">Hello!</div>
      </div>
    |};
  [%expect
    {|
    Linting Failures:

    <div>
      <div id="id"></div> <- [ERRORS]: Duplicate ids
        ...
        <div id="id"></div> <- [ERRORS]: Duplicate ids
      </div>
      <div id="id"></div> <- [ERRORS]: Duplicate ids
    </div>

    [High] Duplicate ids (failure expected)
    ---------------------------------------
    Do not use the same value for an HTML id attribute more than once.
    |}]
;;

let%expect_test "duplicate keys" =
  test
    {%html|
      <div>
        <%{Vdom.Node.div ~key:"key1"}>#1</>
        <%{Vdom.Node.div ~key:"key2"}>#2</>
        <%{Vdom.Node.div ~key:"key3"}>#3</>
        <%{Vdom.Node.div ~key:"key1"}>#1</>
      </div>
    |};
  [%expect
    {|
    Linting Failures:

    <div>
      <div @key=key1></div> <- [ERRORS]: Siblings have same vdom key
      ...
      <div @key=key1></div> <- [ERRORS]: Siblings have same vdom key
    </div>

    [Fatal] Siblings have same vdom key (failure expected)
    ------------------------------------------------------
    Sibling vdom nodes MUST NOT have the same key. This will crash your web app at runtime.
    |}]
;;

let%expect_test "button without type or invalid type" =
  test
    {%html|
      <div>
        <button type="butno" on_click=%{on_click}>Hello!</button>
        <button on_click=%{on_click}>Hello!</button>
      </div>
    |};
  [%expect
    {|
    Linting Failures:

    <div>
      <button type="butno" @on_click></button> <- [ERRORS]: Button without valid type
      <button @on_click></button> <- [ERRORS]: Button without valid type
    </div>

    [High] Button without valid type (failure expected)
    ---------------------------------------------------
    HTML `<button />` elements should explicitly specify a `type` attribute, which should be
    "button", "submit", or "reset". If not set, the browser will use "submit" by default,
    which will cause the button to submit any `<form />`s that contain it when clicked.

    Likely fix: add a `type="button"` attribute.
    |}]
;;

let%expect_test "clickable role without tabindex" =
  test
    {%html|
      <div>
        <div role="button">Hello!</div>
        <div role="button" tabindex=%{0}>Hello!</div>
      </div>
    |};
  [%expect
    {|
    Linting Failures:

    <div>
      <div role="button"></div> <- [ERRORS]: Clickable role but no tabindex
      ...
    </div>

    [High] Clickable role but no tabindex (failure expected)
    --------------------------------------------------------
    This element has a role attribute that marks it as clickable, but has not set
    tabindex="0". This means it will not be focusable via tab, which negatively impacts
    keyboard users.

    Likely fix: add tabindex="0".
    |}]
;;

let%expect_test "bunch of errors" =
  let bad_code =
    {%html|
      <div>
        <div>
          Hi!
          <div on_click=%{on_click} id="best_id">
            Hello!
            <div id="best_id">Hello!</div>
          </div>
          <a href="http://example.com">Link</a>
          More text
          <div on_click=%{on_click}>Hello!</div>
          Bye...
        </div>
        <div tabindex=%{2} %{Vdom.Attr.create "onclick" ""}></div>
        <span id=" id1 id2 id3 id4">Hello!</span>
        <a href="http://example.com" id="best_id">Link</a>
        Wow, this is bad code...
        <div>
          <%{Vdom.Node.div ~key:"key1"}>#1</>
          <%{Vdom.Node.div ~key:"key2"}>#2</>
          <%{Vdom.Node.div ~key:"key3"}>#3</>
          <%{Vdom.Node.div ~key:"key1"}>#1</>
        </div>
        <a href="mailto://example.com" target="_blank">Example</a>
        <div>
          <button type="butno" on_click=%{on_click}>Hello!</button>
          <button on_click=%{on_click}>Hello!</button>
          <div role="button">Hello!</div>
        </div>
      </div>
    |}
  in
  test bad_code;
  [%expect
    {xxx|
    Linting Failures:

    <div>
      <div>
        ...
        <div id="best_id" @on_click></div> <- [ERRORS]: Duplicate ids, Undetectable clickable element
          ...
          <div id="best_id"></div> <- [ERRORS]: Duplicate ids
        </div>
        ...
        <div @on_click></div> <- [ERRORS]: Undetectable clickable element
        ...
      </div>
      <div tabindex="2" onclick=""></div> <- [ERRORS]: Event handler html attribute, Invalid tabindex
      <span id=" id1 id2 id3 id4"></span> <- [ERRORS]: Whitespace in id
      <a href="http://example.com" id="best_id"></a> <- [ERRORS]: Duplicate ids
      ...
      <div>
        <div @key=key1></div> <- [ERRORS]: Siblings have same vdom key
        ...
        <div @key=key1></div> <- [ERRORS]: Siblings have same vdom key
      </div>
      <a href="mailto://example.com" target="_blank"></a> <- [ERRORS]: Unsafe target blank
      <div>
        <button type="butno" @on_click></button> <- [ERRORS]: Button without valid type
        <button @on_click></button> <- [ERRORS]: Button without valid type
        <div role="button"></div> <- [ERRORS]: Clickable role but no tabindex
      </div>
    </div>

    [Fatal] Siblings have same vdom key (failure expected)
    ------------------------------------------------------
    Sibling vdom nodes MUST NOT have the same key. This will crash your web app at runtime.

    [High] Unsafe target blank (failure expected)
    ---------------------------------------------
    HTML `<a />` links may only have `target="_blank"` if they are relative links, or
    `rel="noopener noreferrer"` is set. For more info, see:
    https://hackernoon.com/unsafe-use-of-target_blank-39413ycf

    Likely fix: either use a relative link, or specify `rel="noopener noreferrer"`

    [High] Duplicate ids (failure expected)
    ---------------------------------------
    Do not use the same value for an HTML id attribute more than once.

    [High] Invalid tabindex (failure expected)
    ------------------------------------------
    Only use `0` and `-1` as tabindex values.

    [High] Undetectable clickable element (failure expected)
    --------------------------------------------------------
    This element has a "click" event listener, but does not have an HTML tag or
    role that indicates that it's accessible (e.g. `a`, `button`, a form control, etc).
    Accessibility tools, such as Vimium, might not detect this element.

    Likely fix: Add a `role="button"` attribute, or use a `<button />` tag instead of
    `<div />`.

    [High] Whitespace in id (failure expected)
    ------------------------------------------
    HTML IDs must not contain spaces.

    [High] Clickable role but no tabindex (failure expected)
    --------------------------------------------------------
    This element has a role attribute that marks it as clickable, but has not set
    tabindex="0". This means it will not be focusable via tab, which negatively impacts
    keyboard users.

    Likely fix: add tabindex="0".

    [High] Button without valid type (failure expected)
    ---------------------------------------------------
    HTML `<button />` elements should explicitly specify a `type` attribute, which should be
    "button", "submit", or "reset". If not set, the browser will use "submit" by default,
    which will cause the button to submit any `<form />`s that contain it when clicked.

    Likely fix: add a `type="button"` attribute.

    [High] Event handler html attribute (failure expected)
    ------------------------------------------------------
    Event handling via literal `onclick`, `onfocus`, `onblur`, etc
    HTML attributes is bad practice, and will likely result in CSP errors.

    You should attach event handlers via `Vdom.Attr.on_*` functions, or
    {%html|<button on_click=%{...}></button>|}, which attaches event listeners programmatically.
    |xxx}];
  test ~min_severity:Only_report_app_crashing_errors bad_code;
  [%expect
    {|
    Linting Failures:

    <div>
      ...
      <div>
        <div @key=key1></div> <- [ERRORS]: Siblings have same vdom key
        ...
        <div @key=key1></div> <- [ERRORS]: Siblings have same vdom key
      </div>
      ...
    </div>

    [Fatal] Siblings have same vdom key (failure expected)
    ------------------------------------------------------
    Sibling vdom nodes MUST NOT have the same key. This will crash your web app at runtime.
    |}]
;;
