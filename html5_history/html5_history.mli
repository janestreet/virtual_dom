open Core

(** {v
 A quick overview of HTML5-History.

    Primitives
    ----------
    The primitives the browser provides to you are:

    - A stack of "history entries", which are tuples [(state, uri)] (strictly speaking
      they're three-tuples, but MDN says the "title" is completely ignored)
    - A function pushState that adds a new item to the stack with the given [(state, uri)]
    - A function replaceState that replaces the current top of stack with [(state, uri)]
    - A way of getting the current state and URI, particularly useful on initial page
      load.
    - An event you can listen to, popState, which fires whenever the user uses the
      browser's back/forwards button to move between two history states that were created
      by the same "page".

    When using pushState or replaceState, the string in the address bar immediately
    changes to be [uri] without a page navigation/load (the URI must be of the same origin
    as the current URI).

    MDN says the "state" can be "anything that can be serialised" (with some size limit,
    see [push_or_replace]); here we bin-prot your OCaml value and store the string (along
    with its bin-shape), so you needn't worry about that.

    See more here: https://developer.mozilla.org/en-US/docs/Web/API/History.

    Note that these primitives allow you to:

    1. add an entry to the history stack (with a URI and/or state blob inside it), without
    causing a page load
    2. handle the back button without a page load happening
    3. change the string in the address bar without causing a page load; in fact without
    the addition of a new entry on the history stack if you wish
    4. change the state blob inside the current history stack entry without adding a new
    entry or changing the string in the address bar

    For an example of why you might want to do (4), have a look at the comment by
    [History_state] below ("you may like to put more information in your [History_state.t]
    ...").
    v} *)

(** We let you store a "payload" in each of the history entries. This works by
    bin-protting your value, and storing it alongside the bin-shape hash. When a history
    state is popped, we deserialise it only if the shape matches; otherwise you get
    [None]. *)
module type Payload = sig
  type t [@@deriving bin_io]
end

type _ t

(** You can only call one of the "init" functions (here or in [Opinionated]), and moreover
    only once; this module is inherantly global. *)
val init_exn : ?log_s:(Sexp.t -> unit) -> (module Payload with type t = 'p) -> 'p t

module Entry : sig
  (** Note that the [uri] here is always absolute, and therefore not necessarily the same
      string as passed to [push] or [replace] (see below). *)
  type 'p t =
    { payload : 'p option
    ; uri : Uri.t
    }
end

val popstate_bus : 'p t -> ('p Entry.t -> unit, read) Bus.t
val current : 'p t -> 'p Entry.t

(** When calling [push] or [replace], you can replace the uri by passing the optional
    argument [uri]; if ommitted it will be unchanged.

    Note that the uri you pass here can be relative, and it will be resolved relative to
    the current uri. However, the uri you get back out of an [_ Entry.t] is _not_
    relative; you won't necessarily get out the string you put in. I recommend you
    construct a uri with at most [path] and [query] in it.

    These functions will raise in the case of uri syntax errors. The _browser_ may throw
    an exception if it doesn't like your uri or state, for example if the uri is not of
    the same origin, or the state is excessively large (multiple megabytes in Chrome, 640k
    in Firefox). *)

val push_or_replace : 'p t -> [ `Push | `Replace ] -> ?uri:Uri.t -> 'p -> unit
val push : 'p t -> ?uri:Uri.t -> 'p -> unit
val replace : 'p t -> ?uri:Uri.t -> 'p -> unit

(** This module provides a more opinionated way to use html5-history; it's a thin wrapper
    over the above. *)
module Opinionated : sig
  (** The [Uri_routing] module is common to your server and your client. The server needs
      to call it because it should only serve up your [client.html] if routing succeeds.
      You shouldn't unconditionally serve the homepage and rely on the client javascript
      to filter out bad uris, because it means you'll serve up junk for favicon.ico and
      robots.txt etc. *)
  module type Uri_routing = sig
    (** [Uri_routing.t] is typically a variant type describing which page the user has
        browsed to.

        It's up to you whether or not you include a constructor for your static files, or
        have your server serve them up before it considers [Uri_routing.parse]. *)
    type t [@@deriving equal, sexp_of]

    (** [parse] should probably inspect only the [path] and maybe the [query]. *)
    val parse : Uri.t -> (t, [ `Not_found ]) Result.t

    (** It's strongly encouraged for [to_path_and_query] to produce a URI without the
        scheme or host (i.e, only provide the [path]); it will be handed to
        [push_or_replace] and the browser will resolve it relative to the current URI, as
        mentioned above.

        If the URI produced has e.g. a [host] in it, and is not of the same origin as the
        current page, [push_or_replace] will raise. *)
    val to_path_and_query : t -> Uri.t
  end

  module type History_state = sig
    type uri_routing

    (** [History_state.t] is the thing that is inserted into the html5-history state
        object. In the simplest case it can be equal to the [Uri_routing.t], and when the
        history state is popped by the user then it will cause a "navigation" with the
        same semantics as you get if the initial page load lands at that place (i.e., if
        if [Opinionated.t] forms the initial state via using [Uri_routing.t] on the
        initial uri loaded).

        However, you may like to put more information in your [History_state.t], for
        example, the current values filled out into a form. This is good because it means
        that when someone presses the back button after navigating away (even to another
        site) their form values are as they left them (just as the browser does for dumb
        forms).

        This means that [of_uri_routing] needs to construct something with [None] or
        default values, and [to_uri_routing] needs to throw away information. This is all
        very reasonable, and should match your intuition of what is preserved when
        pressing back/forwards, and what is contained in the uri/happens if you load that
        uri from scratch. *)
    type t [@@deriving bin_io, equal, sexp_of]

    val to_uri_routing : t -> uri_routing
    val of_uri_routing : uri_routing -> t
  end

  type _ t

  (** You can only call [init_exn] or [Opinionated.init_exn] once (see above). *)
  val init_exn
    :  ?log_s:(Sexp.t -> unit)
    -> (module History_state with type t = 's and type uri_routing = 'u)
    -> (module Uri_routing with type t = 'u)
    -> on_bad_uri:[ `Raise | `Default_state of 's ]
    -> 's t

  val current : 's t -> 's
  val changes_bus : 's t -> ('s -> unit, read) Bus.t

  (** We will "push" a state if the [Uri_routing.t] changes as a result of this update
      (i.e., [to_uri_routing previous_state <> to_uri_routing new_state]), and use
      "replace" otherwise. This matches the intuition that new history states are created
      when the address bar changes (but the current state can be updated if the things
      you're looking at within that view change). *)
  val update : 's t -> 's -> unit

  (** Like [update], but does not "push" a state even if the [Uri_routing.t] changes as a
      result of this update. *)
  val replace : 's t -> 's -> unit

  (** [extra_bus] is [Bonsai_web.Start.Handle.extra]; [get_state] projects the history
      state out of your "extra" value, and [schedule_navigate_to] should use whatever
      combination of [Handle.inject_incoming] or kicking off async jobs is required to
      navigate to the new model and start any processes required to initialise it (e.g.,
      by loading some data from the server).

      [get_state] is allowed to return an error (we just ignore the update) in case you
      need to have some temporary startup state in your model. It's just a convenience.

      We do not call [schedule_navigate_to] for the initial state. *)
  val sync_to_bonsai
    :  's t
    -> extra_bus:('e -> unit, read) Bus.t
    -> get_state:('e -> ('s, [ `Uninitialised ]) Result.t)
    -> schedule_navigate_to:('s -> unit)
    -> unit
end

(** Strips all but path, query, and fragment. Ensures a leading / on empty path. Mostly
    for testing but you can use it if you'd really like *)
val uri_to_html5_history_string : Uri.t -> string
