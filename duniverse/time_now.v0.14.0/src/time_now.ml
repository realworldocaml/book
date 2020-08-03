[%%import
  "config.h"]

open! Base

[%%ifdef
  JSC_ARCH_SIXTYFOUR]

external nanoseconds_since_unix_epoch_or_zero
  :  unit
  -> Int63.t
  = "time_now_nanoseconds_since_unix_epoch_or_zero"
[@@noalloc]

[%%else]

external nanoseconds_since_unix_epoch_or_zero
  :  unit
  -> Int63.t
  = "time_now_nanoseconds_since_unix_epoch_or_zero"

[%%endif]

[%%ifdef
  JSC_POSIX_TIMERS]

let[@cold] gettime_failed () = failwith "clock_gettime(CLOCK_REALTIME) failed"

[%%else]

let[@cold] gettime_failed () = failwith "gettimeofday failed"

[%%endif]

let nanoseconds_since_unix_epoch () =
  let t = nanoseconds_since_unix_epoch_or_zero () in
  if Int63.( <> ) t Int63.zero then t else gettime_failed ()
;;
