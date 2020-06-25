
type t = int64

let of_us m =
  if m < 0 then
    invalid_arg "negative" ;
  let m = Int64.of_int m in
  if Int64.compare m 0x4189374BC6A7EDL = 1 then
    invalid_arg "out of range" ;
  Int64.mul 1_000L m

let of_ms m =
  if m < 0 then
    invalid_arg "negative" ;
  let m = Int64.of_int m in
  if Int64.compare m 0x10C6F7A0B5EDL = 1 then
    invalid_arg "out of range" ;
  Int64.mul 1_000_000L m

let of_sec s =
  if s < 0 then
    invalid_arg "negative" ;
  let s = Int64.of_int s in
  if Int64.compare s 0x44B82FA09L = 1 then
    invalid_arg "out of range" ;
  Int64.mul 1_000_000_000L s

let of_min m =
  if m < 0 then
    invalid_arg "negative" ;
  let m = Int64.of_int m in
  if Int64.compare m 0x12533FE6L = 1 then
    invalid_arg "out of range" ;
  Int64.mul 60_000_000_000L m

let hour = 3600_000_000_000L

let of_hour h =
  if h < 0 then
    invalid_arg "negative" ;
  let h = Int64.of_int h in
  if Int64.compare h 0x4E2FFFL = 1 then
    invalid_arg "out of range" ;
  Int64.mul hour h

let of_day d =
  if d < 0 then
    invalid_arg "negative" ;
  let d = Int64.of_int d in
  if Int64.compare d 0x341FFL = 1 then
    invalid_arg "out of range" ;
  Int64.mul (Int64.mul 24L hour) d

let of_year y =
  if y < 0 then
    invalid_arg "negative" ;
  let y = Int64.of_int y in
  if Int64.compare y 0x248L = 1 then
    invalid_arg "out of range" ;
  Int64.mul (Int64.mul 8766L hour) y

let of_f f =
  if f < 0. then
    invalid_arg "negative" ;
  if f > 18446744073.709549 then
    invalid_arg "out of range" ;
  let s = int_of_float f in
  let rem = f -. (float_of_int s) in
  let ns = Int64.of_float (rem *. 1_000_000_000.) in
  Int64.(add (mul 1_000_000_000L (of_int s)) ns)

let to_f t =
  let pl =
    if t >= 0L then
      0.
    else
      abs_float (2. *. (Int64.to_float 0x8000000000000000L))
  in
  let ns = Int64.to_float t in
  (ns +. pl) /. 1_000_000_000.

let to_int t d =
  let f c = Int64.(to_int (div c d)) in
  if t < 0L then
    f (Int64.add t Int64.min_int) + f Int64.max_int + 1
  else
    f t

let to_us t = to_int t 1_000L

let to_ms t = to_int t 1_000_000L

let to_sec t = to_int t 1_000_000_000L

let to_min t = to_int t 60_000_000_000L

let to_hour t = to_int t hour

let to_day t = to_int t (Int64.mul 24L hour)

let to_year t = to_int t (Int64.mul 8766L hour)

let pp ppf t =
  let hours = to_hour t in
  let left = Int64.rem t hour in
  let min = to_min left in
  let left = Int64.sub left (of_min min) in
  let sec = to_sec left in
  let left = Int64.sub left (of_sec sec) in
  let ms = to_ms left in
  let left = Int64.sub left (of_ms ms) in
  let us = to_us left in
  let ns = Int64.(to_int (sub left (of_us us))) in
  let p = ref false in
  let space () = if !p then Format.pp_print_space ppf () in
  if hours > 0 then
    (p := true ; Format.fprintf ppf "%d hours" hours) ;
  if (!p && (sec > 0 || ms > 0 || us > 0 || ns > 0)) || min > 0 then
    (space () ; p := true ; Format.fprintf ppf "%d minutes" min) ;
  if (!p && (ms > 0 || us > 0 || ns > 0)) || sec > 0 then
    (space () ; p := true ; Format.fprintf ppf "%d seconds" sec) ;
  if (!p && (us > 0 || ns > 0)) || ms > 0 then
    (space () ; p := true ; Format.fprintf ppf "%d milliseconds" ms) ;
  if (!p && ns > 0) || us > 0 then
    (space () ; p := true ; Format.fprintf ppf "%d microseconds" us) ;
  if ns > 0 then
    (space () ; Format.fprintf ppf "%d nanoseconds" ns)
