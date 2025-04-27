open! Core

module Private_for_this_library_only : sig
  val schedule : (unit -> unit) -> unit
end

val with_on_mount_at_end : (unit -> 'a) -> 'a

module For_testing : sig
  val num_queued_tasks : unit -> int
end
