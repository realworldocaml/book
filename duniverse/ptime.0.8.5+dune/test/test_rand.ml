(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing

let strf = Printf.sprintf

(* Random state *)

let rstate = ref None
let set_seed seed =
  log "[RANDOM] Using random seed %d" seed;
  rstate := Some (Random.State.make [| seed |]);
  ()

let rec get_state () = match !rstate with
| Some s -> s
| None -> (* auto-seed *)
    let s = Random.State.make_self_init () in (* auto seed *)
    set_seed (Random.State.bits s);
    get_state ()

(* Random Ptime-valid stamps from floats *)

let float_stamp_range min max =
  let bound = max -. min in
  fun () ->
    let r = Random.State.float (get_state ()) bound (* inclusive *) in
    let stamp = min +. r in
    match Ptime.(of_float_s stamp) with
    | None -> failwith (strf "cannot convert valid random stamp %f" stamp)
    | Some t -> t

let float_stamp_32bits =
  let min_stamp = Int32.(to_float min_int) in
  let max_stamp = Int32.(to_float max_int) in
  float_stamp_range min_stamp max_stamp

let float_stamp : unit -> Ptime.t =
  let min_stamp = Ptime.(to_float_s min) in
  let max_stamp = Ptime.(to_float_s max) in
  float_stamp_range min_stamp max_stamp

(* Random Ptime-valid dates *)

let date : unit -> (int * int * int) =
  let month_len = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  let is_leap y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) in
  fun () ->
    let rstate = get_state () in
    let rint bound = Random.State.int rstate bound in
    let y = rint 10_000 in
    let m = 1 + rint 11 in
    let m_len = if (m = 2 && is_leap y) then 29 else month_len.(m - 1) in
    let d = 1 + rint m_len in
    (y, m, d)

(* Random times *)

let tz_interval_s = (1 lsl 30 - 1) (* max of Random.int *)
let tz_offset_s : unit -> int =
  fun () ->
    let rstate = get_state () in
    (* N.B. We don't cover the whole spectrum *)
    (Random.State.int rstate tz_interval_s) - (tz_interval_s / 2)

let min_tz_interval_s = 2000
let min_tz_offset_s : unit -> int =
  fun () ->
    let rstate = get_state () in
    ((Random.State.int rstate min_tz_interval_s) - (min_tz_interval_s / 2)) * 60

let time : unit -> (int * int * int) =
  fun () ->
    let rstate = get_state () in
    let rint bound = Random.State.int rstate bound in
    let hh = rint 24 in
    let mm = rint 60 in
    let ss = rint 61 in
    (hh, mm, ss)

(* Random loop length *)

let loop_len = ref 100_000
let set_loop_len len =
  log "[LOOP] Loop iterations: %d" len;
  loop_len := len

let loop_len () = !loop_len

(* Command line helpers *)

let seed_opt =
  "-seed", Arg.Int set_seed,
  "<nat> random seed to use (auto-seeded by default)."

let loop_len_opt =
  "-loop", Arg.Int set_loop_len,
  "<nat> iterations to perform in random run loop tests (defaults to 100_000)."

let cmdline ?(opts = []) () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "Usage: %s <options>\nOptions:" exec in
  let anon _ = raise (Arg.Bad "positional arguments unsupported") in
  Arg.parse (opts @ [seed_opt; loop_len_opt]) anon usage;


(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers

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
