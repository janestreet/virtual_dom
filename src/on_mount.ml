open Core

let stack = ref None

module Private_for_this_library_only = struct
  let schedule task =
    match !stack with
    | None ->
      print_endline
        "You attempted to schedule an [on_mount] task somewhere other than within a \
         [with_on_mount_at_end] call, which is not allowed, so the task will not be \
         scheduled. This is usually caused by running [Vdom.Node.to_dom] or \
         [Vdom.Node.Patch.apply] outside of a widget or hook."
    | Some tl -> stack := Some (task :: tl)
  ;;
end

let with_on_mount_at_end f =
  let r =
    Ref.set_temporarily stack (Some []) ~f:(fun () ->
      let r = f () in
      let stack_has_tasks () =
        match !stack with
        | None ->
          print_endline
            "BUG! Scheduled tasks should never be empty inside of a \
             [with_on_mount_at_end] call.";
          false
        | Some [] -> false
        | _ -> true
      in
      while stack_has_tasks () do
        let to_run = Option.value !stack ~default:[] in
        stack := Some [];
        List.iter (List.rev to_run) ~f:(fun f -> f ())
      done;
      r)
  in
  r
;;

module For_testing = struct
  let num_queued_tasks () =
    match !stack with
    | None -> 0
    | Some l -> List.length l
  ;;
end
