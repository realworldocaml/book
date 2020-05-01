external clock_linux_get_time
  : unit -> (int64[@unboxed])
  = "clock_linux_get_time_byte" "clock_linux_get_time_native"
[@@noalloc]

external linux_get_tick
  : unit -> (int64[@unboxed])
  = "unimplemented" "clock_linux_get_tick"

let now () = clock_linux_get_time ()
let tick () = linux_get_tick ()
