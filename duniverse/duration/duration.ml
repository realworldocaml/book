
type t = int64

let of_us_64 m =
  if m < 0L then
    invalid_arg "negative" ;
  if Int64.compare m 0x4189374BC6A7EDL = 1 then
    invalid_arg "out of range" ;
  Int64.mul 1_000L m

let of_us m = of_us_64 (Int64.of_int m)

let of_ms_64 m =
  if m < 0L then
    invalid_arg "negative" ;
  if Int64.compare m 0x10C6F7A0B5EDL = 1 then
    invalid_arg "out of range" ;
  Int64.mul 1_000_000L m

let of_ms m = of_ms_64 (Int64.of_int m)

let of_sec_64 s =
  if s < 0L then
    invalid_arg "negative" ;
  if Int64.compare s 0x44B82FA09L = 1 then
    invalid_arg "out of range" ;
  Int64.mul 1_000_000_000L s

let of_sec m = of_sec_64 (Int64.of_int m)

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

let day = Int64.mul 24L hour

let of_day d =
  if d < 0 then
    invalid_arg "negative" ;
  let d = Int64.of_int d in
  if Int64.compare d 0x341FFL = 1 then
    invalid_arg "out of range" ;
  Int64.mul day d

let year = Int64.mul 8766L hour

let of_year y =
  if y < 0 then
    invalid_arg "negative" ;
  let y = Int64.of_int y in
  if Int64.compare y 0x248L = 1 then
    invalid_arg "out of range" ;
  Int64.mul year y

let of_f f =
  if f < 0. then
    invalid_arg "negative" ;
  if f > 18446744073.709549 then
    invalid_arg "out of range" ;
  let s = Int64.of_float f in
  let rem = f -. (Int64.to_float s) in
  let ns = Int64.of_float (rem *. 1_000_000_000.) in
  Int64.(add (mul 1_000_000_000L s) ns)

let to_f t =
  let pl =
    if t >= 0L then
      0.
    else
      abs_float (2. *. (Int64.to_float 0x8000000000000000L))
  in
  let ns = Int64.to_float t in
  (ns +. pl) /. 1_000_000_000.

let to_int64 t d =
  let f c = Int64.div c d in
  if t < 0L then
    Int64.(add (f (Int64.add t Int64.min_int)) (add (f Int64.max_int) 1L))
  else
    f t

let to_int t d =
  let r = to_int64 t d in
  if r > Int64.of_int max_int then
    invalid_arg "value too big for this platform" ;
  Int64.to_int r

let to_us_64 t = to_int64 t 1_000L

let to_us t = to_int t 1_000L

let to_ms_64 t = to_int64 t 1_000_000L

let to_ms t = to_int t 1_000_000L

let to_sec_64 t = to_int64 t 1_000_000_000L

let to_sec t = to_int t 1_000_000_000L

let to_min t = to_int t 60_000_000_000L

let to_hour t = to_int t hour

let to_day t = to_int t day

let to_year t = to_int t year

let fields t =
  let sec = to_sec_64 t in
  let left = Int64.sub t (of_sec_64 sec) in
  let ms = to_ms_64 left in
  let left = Int64.sub left (of_ms_64 ms) in
  let us = to_us_64 left in
  let ns = Int64.(sub left (of_us_64 us)) in
  (sec, ms, us, ns)

let pp ppf t =
  let min = to_min t in
  if min > 0 then
    let y = to_year t in
    let left = Int64.rem t year in
    let d = to_day left in
    let left = Int64.rem left day in
    if y > 0 then
      Format.fprintf ppf "%da%dd" y d
    else
      let h = to_hour left in
      let left = Int64.rem left hour in
      if d > 0 then
        Format.fprintf ppf "%dd%02dh" d h
      else
        let min = to_min left in
        let left = Int64.sub t (of_min min) in
        let sec = to_sec left in
        if h > 0 then
          Format.fprintf ppf "%dh%02dm" h min
        else (* if m > 0 then *)
          Format.fprintf ppf "%dm%02ds" min sec
  else (* below one minute *)
    let s, ms, us, ns = fields t in
    if s > 0L then
      Format.fprintf ppf "%Ld.%03Lds" s ms
    else if ms > 0L then
      Format.fprintf ppf "%Ld.%03Ldms" ms us
    else (* if us > 0 then *)
      Format.fprintf ppf "%Ld.%03Ldus" us ns
