open Core
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

(* History api only cares about path, query, and fragment and the browser doesn't
   have access to userinfo to be able to fully recreate the uri correctly. Before
   sending to history api we strip all the parts of the URI that are not necessary *)
let uri_to_html5_history_string uri =
  Uri.make
    ~scheme:"https"
    ~path:(Uri.path uri)
    ~query:(Uri.query uri)
    ?fragment:(Uri.fragment uri)
    ()
  (* http/s uri's get their paths canonicalized with a leading slash so we ensure we
     canonicalize as a https uri and then remove. *)
  |> Uri.canonicalize
  |> (fun uri -> Uri.with_scheme uri None)
  |> Uri.to_string
;;

module T = struct
  module type Payload = sig
    type t [@@deriving bin_io]
  end

  module Entry = struct
    type 'p t =
      { payload : 'p option
      ; uri : Uri.t
      }
  end

  type 'p t =
    { payload_module : (module Payload with type t = 'p)
    ; payload_bin_shape : string
    ; popstate_bus : ('p Entry.t -> unit, read_write) Bus.t
    ; log_s : Sexp.t -> unit
    }

  let log_s t sexp : unit = t.log_s sexp
  let initialised = ref false

  let convert_state (type p) (t : p t) (state : Js.Unsafe.top Js.t Js.opt) =
    let result =
      match Js.Opt.to_option state with
      | None ->
        error_s
          [%message
            "Html5_history" "history state was null, presumably due to initial page load"]
      | Some state ->
        let (module Payload : Payload with type t = p) = t.payload_module in
        let get_string (x : _ Js.t) key =
          match Js.Optdef.to_option (Js.Unsafe.get x (Js.string key)) with
          | None -> None
          | Some value ->
            (match Js.to_string (Js.typeof value) with
             | "string" -> Some (Js.to_string value)
             | _ -> None)
        in
        (match get_string state "bin_shape", get_string state "payload_v1" with
         | None, _ | _, None ->
           let state =
             (* a state object can be "anything that can be serialised", which is not
                precisely the same as "can be JSONd", but this is a reasonable best
                effort. *)
             match Js_of_ocaml.Json.output state with
             | exception exn -> [%sexp "failed to turn state into JSON", (exn : exn)]
             | string -> [%sexp (Js.to_string string : string)]
           in
           error_s
             [%message
               "Html5_history"
                 "history state non-null, but bin_shape or payload missing"
                 (state : Sexp.t)]
         | Some saved_shape, Some payload ->
           (match String.equal saved_shape t.payload_bin_shape with
            | false ->
              error_s
                [%message
                  "Html5_history"
                    "history event bin shape mismatch"
                    ~saved_shape
                    ~expected:t.payload_bin_shape]
            | true ->
              Or_error.try_with (fun () ->
                (* Even though the bin-shapes are the same and we would expect this to
                   always succeed, it's still possible that it fails due to serializers
                   that involve other formats e.g. Binable.Of_sexpable *)
                let payload = Base64.decode_exn payload in
                Binable.of_string (module Payload) payload)))
    in
    match result with
    | Ok state -> Some state
    | Error error ->
      log_s t [%sexp (error : Error.t)];
      None
  ;;

  let current_uri () =
    let string = Js.to_string Dom_html.window##.location##.href in
    match Uri.of_string string with
    | uri -> uri
    | exception exn ->
      raise_s
        [%message
          "Html5_history" "BUG: browser gave us a URI we can't parse" string (exn : exn)]
  ;;

  let init_exn (type p) ?(log_s = ignore) payload_module =
    (match !initialised with
     | true -> failwith "You called Html5_history.init_exn twice"
     | false -> initialised := true);
    let (module Payload : Payload with type t = p) = payload_module in
    let payload_bin_shape =
      Bin_prot.Shape.eval_to_digest_string [%bin_shape: Payload.t]
    in
    let popstate_bus =
      Bus.create_exn
        [%here]
        Arity1
        ~on_subscription_after_first_write:Allow
        ~on_callback_raise:Error.raise
    in
    let t = { payload_module; payload_bin_shape; popstate_bus; log_s } in
    let (_ : Dom_html.event_listener_id) =
      let handler event =
        let payload =
          let state : Js.Unsafe.top Js.t Js.opt =
            (* [Dom_html.popStateEvent##.state] claims the type is [Js.Unsafe.top Js.t]
               when actually it could be null. *)
            Js.Unsafe.get event (Js.string "state")
          in
          convert_state t state
        in
        let uri = current_uri () in
        Bus.write popstate_bus { payload; uri };
        Js._true
      in
      Dom.addEventListener
        Dom_html.window
        Dom_html.Event.popstate
        (Dom_html.handler handler)
        Js._true
    in
    t
  ;;

  let popstate_bus t = Bus.read_only t.popstate_bus

  let current t =
    let payload =
      (* [Dom_html.window##.history##.state] claims the type is [Js.Unsafe.top Js.t] when
         actually it could be null. *)
      let state : Js.Unsafe.top Js.t Js.opt =
        Js.Unsafe.get Dom_html.window##.history (Js.string "state")
      in
      convert_state t state
    in
    let uri = current_uri () in
    { Entry.payload; uri }
  ;;

  let push_or_replace (type p) t action ?uri state : unit =
    let (module Payload : Payload with type t = p) = t.payload_module in
    let payload = Binable.to_string (module Payload) state in
    (* we've got to base64 it or unicode-inspired corruption happens *)
    let payload = Base64.encode_exn payload in
    let state =
      Js.Unsafe.obj
        [| "bin_shape", Js.Unsafe.inject (Js.string t.payload_bin_shape)
         ; "payload_v1", Js.Unsafe.inject (Js.string payload)
        |]
    in
    let title =
      (* according to https://developer.mozilla.org/en-US/docs/Web/API/History/pushState,
         most browsers ignore this parameter, and passing the empty string is safe against
         future behaviour changes. *)
      Js.string ""
    in
    let uri =
      match uri with
      | None -> Js.null
      | Some uri -> uri |> uri_to_html5_history_string |> Js.string |> Js.some
    in
    match action with
    | `Replace -> Dom_html.window##.history##replaceState state title uri
    | `Push -> Dom_html.window##.history##pushState state title uri
  ;;

  let replace t ?uri p : unit = push_or_replace t `Replace ?uri p
  let push t ?uri p : unit = push_or_replace t `Push ?uri p
end

module Opinionated = struct
  module Html5_history = T

  module type Uri_routing = sig
    type t [@@deriving equal, sexp_of]

    val parse : Uri.t -> (t, [ `Not_found ]) Result.t
    val to_path_and_query : t -> Uri.t
  end

  module type History_state = sig
    type uri_routing
    type t [@@deriving bin_io, equal, sexp_of]

    val to_uri_routing : t -> uri_routing
    val of_uri_routing : uri_routing -> t
  end

  module type Arg_modules = sig
    module Uri_routing : Uri_routing
    module History_state : History_state with type uri_routing := Uri_routing.t
  end

  type 's t =
    { html5_history : 's Html5_history.t
    ; arg_modules : (module Arg_modules with type History_state.t = 's)
    ; mutable current_state : 's
    ; changes_bus : ('s -> unit, read_write) Bus.t
    }

  let log_s t sexp : unit = Html5_history.log_s t.html5_history sexp

  let push_or_replace
    (type s)
    (module Arg_modules : Arg_modules with type History_state.t = s)
    html5_history
    action
    state
    =
    let open Arg_modules in
    let uri = Uri_routing.to_path_and_query (History_state.to_uri_routing state) in
    Html5_history.push_or_replace html5_history action ~uri state
  ;;

  let init_exn
    ?log_s:log_s_arg
    (type u s)
    history_state_module
    uri_routing_module
    ~on_bad_uri
    =
    let module Arg_modules = struct
      module History_state =
        (val history_state_module
            : History_state with type t = s and type uri_routing = u)

      module Uri_routing = (val uri_routing_module : Uri_routing with type t = u)
    end
    in
    let open Arg_modules in
    let html5_history = Html5_history.init_exn ?log_s:log_s_arg (module History_state) in
    let current_state =
      let { Html5_history.Entry.payload; uri } = Html5_history.current html5_history in
      match payload with
      | Some payload ->
        Html5_history.log_s
          html5_history
          [%message
            "Html5_history"
              "initial history state from state payload"
              (payload : History_state.t)];
        payload
      | None ->
        (match Uri_routing.parse uri with
         | Ok routing -> History_state.of_uri_routing routing
         | Error `Not_found ->
           let message =
             [%message
               "Html5_history"
                 "The server should not have served up the main HTML file on this uri, \
                  as it does not route"
                 ~uri:(Uri.to_string uri)]
           in
           (match on_bad_uri with
            | `Raise -> raise_s message
            | `Default_state s ->
              Html5_history.log_s html5_history message;
              s))
    in
    (* this effectively canonicalises the address bar, and sets the state object
       such that we can just un-bin-prot it on navigation rather than use the URI. *)
    push_or_replace (module Arg_modules) html5_history `Replace current_state;
    let t =
      { html5_history
      ; arg_modules = (module Arg_modules)
      ; current_state
      ; changes_bus =
          Bus.create_exn
            [%here]
            Arity1
            ~on_subscription_after_first_write:Allow
            ~on_callback_raise:Error.raise
      }
    in
    let (_ : _ Bus.Subscriber.t) =
      let bus = Html5_history.popstate_bus html5_history in
      Bus.subscribe_exn bus [%here] ~f:(fun state ->
        match state.payload with
        | None -> log_s t [%message "Html5_history" "ignored popstate due to no payload"]
        | Some payload ->
          log_s t [%message "Html5_history" "popstate" ~_:(payload : History_state.t)];
          t.current_state <- payload;
          Bus.write t.changes_bus payload)
    in
    t
  ;;

  let current t = t.current_state
  let changes_bus t = Bus.read_only t.changes_bus

  let update (type s) t next_state : unit =
    let (module Arg_modules : Arg_modules with type History_state.t = s) =
      t.arg_modules
    in
    let open Arg_modules in
    let prev_state = t.current_state in
    let what_do =
      match History_state.equal prev_state next_state with
      | true ->
        (* optimisation to avoid spamming history.state. *)
        `Nothing
      | false ->
        (match
           Uri_routing.equal
             (History_state.to_uri_routing prev_state)
             (History_state.to_uri_routing next_state)
         with
         | false -> `Push
         | true -> `Replace)
    in
    t.current_state <- next_state;
    match what_do with
    | `Nothing -> ()
    | (`Replace | `Push) as action ->
      log_s
        t
        [%message
          "Html5_history"
            "updating history state"
            (action : [ `Push | `Replace ])
            (prev_state : History_state.t)
            (next_state : History_state.t)];
      push_or_replace t.arg_modules t.html5_history action next_state
  ;;

  let replace t next_state : unit =
    t.current_state <- next_state;
    push_or_replace t.arg_modules t.html5_history `Replace next_state
  ;;

  let sync_to_bonsai t ~extra_bus ~get_state ~schedule_navigate_to =
    let (_ : _ Bus.Subscriber.t) =
      Bus.subscribe_exn (changes_bus t) [%here] ~f:schedule_navigate_to
    in
    let (_ : _ Bus.Subscriber.t) =
      Bus.subscribe_exn extra_bus [%here] ~f:(fun next ->
        match get_state next with
        | Error `Uninitialised -> ()
        | Ok next -> update t next)
    in
    ()
  ;;
end

include T
