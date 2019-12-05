open! Core_kernel
open! Js_of_ocaml

type t = Js.Unsafe.any

let of_any_exn t ~name =
  let javascript_strict_equal = phys_equal in
  if not (javascript_strict_equal (Js.typeof t) (Js.string "function"))
  then raise_s [%message "handler for" name "is not a function"];
  t
;;

let sexp_of_t _ = Sexp.Atom "<handler>"

let throwing_proxy =
  let throwing_function missing_field_path =
    let missing_field_path = Js.to_string missing_field_path in
    raise_s
      [%message
        (missing_field_path : string)
          "The field was read on a fake DOM event.  You probably called \
           [Handler.trigger] in a test, and the handler accessed a field of the event \
           that was not provided to [~extra_fields]."]
  in
  let create_proxy : Js.Unsafe.any -> Js.Unsafe.any -> Js.Unsafe.any -> Js.Unsafe.any =
    (* Given a base-object, a function that raises an ocaml exception,
       and an optional "path" to the base-object, return a proxy object
       that behaves exactly like a read-only [base], but any accesses into
       that object which aren't present instead call [raise_missing_field_path]
       instead of returning [undefined].  The objects returned via successful
       indexes into [base] are also wrapped in a proxy of the same design.

       Because these proxies yield more proxies, the [path] parameter keeps track
       of the field names that were accessed in order to get to the current proxy.
       This enhances the error message that is thrown back to OCaml. *)
    Js.Unsafe.pure_js_expr
      {js|
        (function create_proxy (base, raise_missing_field_path, path) {
          path = path || [];
          function get(obj, field) {
            if (field in obj) {
              var field_value = obj[field];
              if (typeof field_value === 'object') {
                return create_proxy(obj[field], raise_missing_field_path, path.concat([field]));
              } else {
                return field_value;
              }
            } else {
              var missing_field_path = path.concat([field]).join('.');
              raise_missing_field_path(missing_field_path);
            }
          };
          function thrower(_obj, field) {
              var missing_field_path = path.concat([field]).join('.');
              raise_missing_field_path(missing_field_path);
          };
          return new Proxy(base, {
              get: get,
              set: thrower,
              deleteProperty: thrower,
              enumerate: thrower,
              ownKeys: thrower,
              has: thrower,
              defineProperty: thrower,
              getOwnPropertyDescriptor: thrower});
          })|js}
  in
  fun base ->
    let base = base |> List.to_array |> Js.Unsafe.obj |> Js.Unsafe.inject in
    let throwing_function = throwing_function |> Js.wrap_callback |> Js.Unsafe.inject in
    let path = Js.Unsafe.inject Js.null in
    create_proxy base throwing_function path
;;

let trigger ?(extra_fields = []) t =
  Js.Unsafe.fun_call t [| throwing_proxy extra_fields |]
;;
