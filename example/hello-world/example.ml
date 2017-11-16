open Js_of_ocaml
open Virtual_dom
open Vdom

let view =
  let chars =
    [|'h';'e';'l';'l';'o';'_';'w';'o';'r';'l';'d';'_'|]
  in
  let n = Array.length chars in
  Node.Lazy.create (fun k ->
    let s = Bytes.create (Array.length chars) in
    for i = 0 to n - 1 do
      Bytes.set s i chars.((i + k) mod n)
    done;
    let s = Bytes.to_string s in
    let open Tyxml.Html in
    div ~a:[ a_style ("background-color:blue")
           ; a_onclick (fun _e ->
               Firebug.console##log (Js.string "hi");
               Vdom.Event.Ignore
             )
           ]
      [ pcdata "Virtual dom test"
      ; div ~a:[a_class ["class-red"]] [pcdata s]
      ; svg Tyxml.Svg.([
          rect ~a:[ a_width (300., None);
                    a_height (100., None);
                    a_style
                      (Printf.sprintf "fill:rgb(%d,%d,%d);" ((11 * k) mod 255) 0 0);
                  ] [];
          circle ~a:[a_cx (5. *. cos (float_of_int k) +. 40., None);
                     a_cy (5. *. sin (float_of_int k) +. 40., None);
                     a_r  (20., None);
                     a_style "stroke:#006600; fill:#00cc00"]
            []])
      ]
    |> toelt)
;;

let () =
  Dom_html.window##.onload := Dom.handler (fun _ ->
    let k    = ref 0 in
    let vdom = ref (view !k) in
    let elt  = ref (Node.to_dom !vdom :> Dom.element Js.t) in
    Dom.appendChild Dom_html.document##.body !elt;
    Dom_html.window##setInterval (Js.wrap_callback (fun _ ->
      incr k;
      let new_vdom = view !k in
      elt := Node.Patch.apply (Node.Patch.create ~previous:!vdom ~current:new_vdom) !elt;
      vdom := new_vdom
    )) 100. |> ignore;
    Js._false
  )
