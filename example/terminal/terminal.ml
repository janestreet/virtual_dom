open Js_of_ocaml
open Virtual_dom
open Core_kernel
open Vdom

let colors = [| "red"; "green"; "blue" |]

class type terminal =
  object
    method get_command_ : Js.js_string Js.t Js.meth

    method echo : Js.js_string Js.t -> unit Js.meth
  end

let terminal =
  let id = Type_equal.Id.create ~name:"terminal-id" Sexplib.Conv.sexp_of_opaque in
  fun () ->
    let values = String.Table.create () in
    let interpret command (term : terminal Js.t) =
      match String.split ~on:' ' (Js.to_string command) with
      | [ "set"; key; data ] -> Hashtbl.set values ~key ~data
      | [ "get"; key ] ->
        let data = Hashtbl.find values key in
        term##echo (Js.string (sprintf !"%{sexp:string option}" data))
      | _ -> term##echo (Js.string "Invalid command")
    in
    let completion term _command k =
      Js.Unsafe.global##.term := term;
      let command = term##get_command_ in
      Firebug.console##log command;
      let completions =
        match String.split ~on:' ' (Js.to_string command) with
        | [ prefix ] -> List.filter [ "get"; "set" ] ~f:(String.is_prefix ~prefix)
        | [ "get"; key ] ->
          List.filter (Hashtbl.keys values) ~f:(String.is_prefix ~prefix:key)
        | _ -> []
      in
      let completions = Js.array (Array.of_list_rev_map ~f:Js.string completions) in
      Js.Unsafe.(fun_call k [| inject completions |])
    in
    let options =
      object%js
        val prompt = Js.string "> "

        val completion = Js.wrap_callback completion

        val exit = Js._false

        val clear = Js._false
      end
    in
    let init () =
      let div : < terminal : _ -> _ -> unit Js.meth ; get : int -> _ Js.meth > Js.t =
        Js.Unsafe.global##jQuery (Js.string "<div width='400px' height='300px'>")
      in
      div##terminal (Js.wrap_callback interpret) options;
      (), div##get 0
    in
    Node.widget ~init ~id ()
;;

let view count =
  Node.div [] [ Node.div [] [ Node.text (Int.to_string count) ]; terminal () ]
;;

let () =
  Dom_html.window##.onload
  := Dom.handler (fun _ ->
    let count = ref 0 in
    let vdom = ref (view !count) in
    let elt = Node.to_dom !vdom in
    Dom.appendChild Dom_html.document##.body elt;
    Dom_html.window##setInterval
      (Js.wrap_callback (fun _ ->
         incr count;
         let current = view !count in
         let patch = Node.Patch.create ~previous:!vdom ~current in
         vdom := current;
         Node.Patch.apply patch elt |> ignore))
      30.
    |> ignore;
    Js._false)
;;
