open Base

type t = Attr.t list

let to_list t = t

let of_list t = t

let make_map ~extract ~combine ~is_empty ~make_attr t ~f =
  let specials, other =
    List.partition_map t ~f:(fun a ->
      match extract a with
      | Some c -> `Fst c
      | None -> `Snd a)
  in
  let cl = f (combine specials) in
  if is_empty cl then other else make_attr cl :: other
;;

let map_class =
  make_map
    ~extract:Attr.to_class
    ~combine:(Set.union_list (module String))
    ~is_empty:Set.is_empty
    ~make_attr:Attr.classes'
;;

let add_class t c = map_class t ~f:(fun cs -> Set.add cs c)

let map_style =
  make_map
    ~extract:Attr.to_style
    ~combine:Css.concat
    ~is_empty:([%compare.equal: Css.t] Css.empty)
    ~make_attr:Attr.style
;;

let add_style t s = map_style t ~f:(fun ss -> Css.combine ss s)

let merge t1 t2 = List.append t1 t2 |> map_style ~f:Fn.id |> map_class ~f:Fn.id
