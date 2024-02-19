(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Time spans

   Time spans are in nanoseconds and we represent them by an unsigned
   64-bit integer. This allows to represent spans for:
   (2^64-1) / 1_000_000_000 / (24 * 3600 * 365.25) ≅ 584.5 Julian years *)

type span = int64 (* unsigned nanoseconds *)

module Span = struct
  type t = span
  let zero = 0L
  let one = 1L
  let min_span = zero
  let max_span = -1L

  (* Predicates *)

  let equal = Int64.equal
  let compare = Int64.unsigned_compare

  (* Arithmetic *)

  let add = Int64.add
  let abs_diff s0 s1 =
    if compare s0 s1 < 0 then Int64.sub s1 s0 else Int64.sub s0 s1

  (* Durations *)

  let ( * ) n s = Int64.mul (Int64.of_int n) s
  let ns   =                      1L
  let us   =                  1_000L
  let ms   =              1_000_000L
  let s    =          1_000_000_000L
  let min  =         60_000_000_000L
  let hour =       3600_000_000_000L
  let day  =      86400_000_000_000L
  let year = 31_557_600_000_000_000L

  (* Converting *)

  let to_uint64_ns s = s
  let of_uint64_ns ns = ns

  let max_float_int = 9007199254740992. (* 2^53. *)
  let int64_min_int_float = Int64.to_float Int64.min_int
  let int64_max_int_float = Int64.to_float Int64.max_int

  let of_float_ns sf =
    if sf < 0. || sf >= max_float_int || not (Float.is_finite sf)
    then None else Some (Int64.of_float sf)

  let to_float_ns s =
    if Int64.compare 0L s <= 0 then Int64.to_float s else
    int64_max_int_float +. (-. int64_min_int_float +. Int64.to_float s)

  let unsafe_of_uint64_ns_option nsopt = nsopt

  (* Formatting *)

  let pf = Format.fprintf

  let rec pp_si_span unit_str unit_str_len si_unit si_higher_unit ppf span =
    let geq x y = Int64.unsigned_compare x y >= 0 in
    let m = Int64.unsigned_div span si_unit in
    let n = Int64.unsigned_rem span si_unit in
    let pp_unit ppf () = Format.pp_print_as ppf unit_str_len unit_str in
    match m with
    | m when geq m 100L -> (* No fractional digit *)
        let m_up = if Int64.equal n 0L then m else Int64.succ m in
        let span' = Int64.mul m_up si_unit in
        if geq span' si_higher_unit then pp ppf span' else
        (pf ppf "%Ld" m_up; pp_unit ppf ())
    | m when geq m 10L -> (* One fractional digit w.o. trailing zero *)
        let f_factor = Int64.unsigned_div si_unit 10L in
        let f_m = Int64.unsigned_div n f_factor in
        let f_n = Int64.unsigned_rem n f_factor in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        begin match f_m_up with
        | 0L -> pf ppf "%Ld" m; pp_unit ppf ()
        | f when geq f 10L ->
            pp ppf Int64.(add (mul m si_unit) (mul f f_factor))
        | f -> pf ppf "%Ld.%Ld" m f; pp_unit ppf ()
        end
    | m -> (* Two or zero fractional digits w.o. trailing zero *)
        let f_factor = Int64.unsigned_div si_unit 100L in
        let f_m = Int64.unsigned_div n f_factor in
        let f_n = Int64.unsigned_rem n f_factor in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        match f_m_up with
        | 0L -> pf ppf "%Ld" m; pp_unit ppf ()
        | f when geq f 100L ->
            pp ppf Int64.(add (mul m si_unit) (mul f f_factor))
        | f when Int64.equal (Int64.rem f 10L) 0L ->
            pf ppf "%Ld.%Ld" m (Int64.div f 10L); pp_unit ppf ()
        | f ->
            pf ppf "%Ld.%02Ld" m f; pp_unit ppf ()

  and pp_non_si unit_str unit unit_lo_str unit_lo unit_lo_size ppf span =
    let geq x y = Int64.unsigned_compare x y >= 0 in
    let m = Int64.unsigned_div span unit in
    let n = Int64.unsigned_rem span unit in
    if Int64.equal n 0L then pf ppf "%Ld%s" m unit_str else
    let f_m = Int64.unsigned_div n unit_lo in
    let f_n = Int64.unsigned_rem n unit_lo in
    let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
    match f_m_up with
    | f when geq f unit_lo_size ->
        pp ppf Int64.(add (mul m unit) (mul f unit_lo))
    | f ->
        pf ppf "%Ld%s%Ld%s" m unit_str f unit_lo_str

  and pp ppf span =
    let geq x y = Int64.unsigned_compare x y >= 0 in
    let lt x y = Int64.unsigned_compare x y = -1 in
    match span with
    | sp when lt sp us -> pf ppf "%Ldns" sp
    | sp when lt sp ms -> pp_si_span "\xCE\xBCs" 2 us ms ppf sp
    | sp when lt sp s -> pp_si_span "ms" 2 ms s ppf sp
    | sp when lt sp min -> pp_si_span "s" 1 s min ppf sp
    | sp when lt sp hour -> pp_non_si "min" min "s" s 60L ppf sp
    | sp when lt sp day -> pp_non_si "h" hour "min" min 60L ppf sp
    | sp when lt sp year -> pp_non_si "d" day "h" hour 24L ppf sp    | sp ->
        let m = Int64.unsigned_div sp year in
        let n = Int64.unsigned_rem sp year in
        if Int64.equal n 0L then pf ppf "%Lda" m else
        let f_m = Int64.unsigned_div n day in
        let f_n = Int64.unsigned_rem n day in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        match f_m_up with
        | f when geq f 366L -> pf ppf "%Lda" (Int64.succ m)
        | f -> pf ppf "%Lda%Ldd" m f

  let dump ppf s = Format.fprintf ppf "%Lu" s
end

(* Monotonic timestamps *)

type t = int64

let to_uint64_ns s = s
let of_uint64_ns ns = ns
let min_stamp = 0L
let max_stamp = -1L

(* Predicates *)

let equal = Int64.equal
let compare = Int64.unsigned_compare
let is_earlier t ~than = compare t than < 0
let is_later t ~than = compare t than > 0

(* Arithmetic *)

let span t0 t1 = if compare t0 t1 < 0 then Int64.sub t1 t0 else Int64.sub t0 t1

let add_span t s =
  let sum = Int64.add t s in
  if compare t sum <= 0 then Some sum else None

let sub_span t s =
  if compare t s < 0 then None else Some (Int64.sub t s)

(* Formatters *)

let pp ppf ns = Format.fprintf ppf "%Luns" ns
let dump ppf ns = Format.fprintf ppf "%Lu" ns

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
