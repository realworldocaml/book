external clock_windows_get_time : unit -> int64 = "clock_windows_get_time"
external clock_windows_init : unit -> unit = "clock_windows_init"

let () = clock_windows_init ()
let now () = clock_windows_get_time ()
let tick () = clock_windows_get_time () (* TODO: RDTSC on Windows? *)
