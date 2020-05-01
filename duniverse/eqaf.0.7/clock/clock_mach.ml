external clock_mach_init : unit -> unit = "clock_mach_init"
external clock_mach_get_time : unit -> int64 = "clock_mach_get_time"

let () = clock_mach_init ()
let now () = clock_mach_get_time ()
let tick () = clock_mach_get_time () (* TODO: RDTSC on Mach? *)
