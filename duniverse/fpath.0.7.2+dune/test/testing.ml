(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Value equality and pretty printing *)

type 'a eq = 'a -> 'a -> bool
type 'a pp = Format.formatter -> 'a -> unit

(* Pretty printers *)

let pp = Format.fprintf
let pp_exn ppf v = pp ppf "%s" (Printexc.to_string v)
let pp_bool ppf v = pp ppf "%b" v
let pp_char ppf v = pp ppf "%C" v
let pp_str ppf v = pp ppf "%S" v
let pp_int = Format.pp_print_int
let pp_float ppf v = pp ppf "%.10f" (* bof... *) v
let pp_int32 ppf v = pp ppf "%ld" v
let pp_int64 ppf v = pp ppf "%Ld" v
let pp_text = Format.pp_print_text
let pp_list pp_v ppf l =
  let pp_sep ppf () = pp ppf ";@," in
  pp ppf "@[<1>[%a]@]" (Format.pp_print_list ~pp_sep pp_v) l

let pp_option pp_v ppf = function
| None -> Format.fprintf ppf "None"
| Some v -> Format.fprintf ppf "Some %a" pp_v v

let pp_slot_loc ppf l =
  pp ppf "%s:%d.%d-%d:"
    l.Printexc.filename l.Printexc.line_number
    l.Printexc.start_char l.Printexc.end_char

let pp_bt ppf bt = match Printexc.backtrace_slots bt with
| None -> pp ppf "@,@[%a@]" pp_text "No backtrace. Did you compile with -g ?"
| Some slots ->
    let rec loop = function
    | [] -> assert false
    | s :: ss ->
        begin match Printexc.Slot.location s with
        | None -> ()
        | Some l when l.Printexc.filename = "test/testing.ml" ||
                      l.Printexc.filename = "test/test.ml" -> ()
        | Some l -> pp ppf "@,%a" pp_slot_loc l
        end;
        if ss <> [] then (loop ss) else ()
    in
    loop (Array.to_list slots)

(* Assertion counters *)

let fail_count = ref 0
let pass_count = ref 0

(* Logging *)

let log_part fmt = Format.printf fmt
let log ?header fmt = match header with
| Some h -> Format.printf ("[%s] " ^^ fmt ^^ "@.") h
| None -> Format.printf (fmt ^^ "@.")

let log_results () =
  let total = !pass_count + !fail_count in
  match !fail_count with
  | 0 -> log ~header:"OK" "All %d assertions succeeded !@." total; true
  | 1 -> log ~header:"FAIL" "1 failure out of %d assertions" total; false
  | n -> log ~header:"FAIL" "%d failures out of %d assertions"
           !fail_count total; false

let log_fail msg bt =
  log ~header:"FAIL" "@[<v>@[%a@]%a@]" pp_text msg pp_bt bt

let log_unexpected_exn ~header exn bt =
  log ~header:"SUITE" "@[<v>@[ABORTED: unexpected exception:@]@,%a%a@]"
    pp_exn exn pp_bt bt

(* Testing scopes *)

exception Fail
exception Fail_handled

let block f = try f () with
| Fail | Fail_handled -> ()
| exn ->
    let bt = Printexc.get_raw_backtrace () in
    incr fail_count;
    log_unexpected_exn ~header:"BLOCK" exn bt

type test = string * (unit -> unit)

let test n f = n, f
let run_test (n, f) =
  log "* %s" n;
  try f () with
  | Fail | Fail_handled ->
      log ~header:"TEST" "ABORTED: a test failure blew the test scope"
  | exn ->
      let bt = Printexc.get_raw_backtrace () in
      incr fail_count;
      log_unexpected_exn ~header:"TEST" exn bt

type suite = string * test list
let suite n ts = n, ts
let run_suite (n, ts) = try log "%s" n; List.iter run_test ts with
| exn ->
    let bt = Printexc.get_raw_backtrace () in
    incr fail_count;
    log_unexpected_exn ~header:"SUITE" exn bt

let run suites = List.iter run_suite suites

(* Passing and failing tests *)

let pass () = incr pass_count
let fail fmt =
  let bt = Printexc.get_callstack 10 in
  let fail _ = log_fail (Format.flush_str_formatter ()) bt in
  (incr fail_count; Format.kfprintf fail Format.str_formatter fmt)

(* Checking values *)

let pp_neq pp_v ppf (v, v') = pp ppf "@[%a@]@ <>@ @[%a@]@]" pp_v v pp_v v'

let fail_eq pp v v' = fail "%a" (pp_neq pp) (v, v')

let eq ~eq ~pp v v' = if eq v v' then pass () else fail_eq pp v v'
let eq_char = eq ~eq:(=) ~pp:pp_char
let eq_str = eq ~eq:(=) ~pp:pp_str
let eq_bool = eq ~eq:(=) ~pp:Format.pp_print_bool
let eq_int = eq ~eq:(=) ~pp:Format.pp_print_int
let eq_int32 = eq ~eq:(=) ~pp:pp_int32
let eq_int64 = eq ~eq:(=) ~pp:pp_int64
let eq_float = eq ~eq:(=) ~pp:pp_float
let eq_nan f =
  if f <> f then pass () else fail "@[%a@]@ is@ not a NaN" pp_float f

let eq_option ~eq:eq_v ~pp =
  let eq_opt v v' = match v, v' with
  | Some v, Some v' -> eq_v v v'
  | None, None -> true
  | _ -> false
  in
  let pp = pp_option pp in
  fun v v' -> eq ~eq:eq_opt ~pp v v'

let eq_some = function
| Some _ -> pass ()
| None -> fail "None <> Some _"

let eq_none ~pp = function
| None -> pass ()
| Some v -> fail "@[%a <>@ None@]" pp v

let eq_list ~eq:eq_v ~pp:pp_v =
  let eql l l' = try List.for_all2 eq_v l l' with Invalid_argument _ -> false in
  fun l l' -> eq ~eq:eql ~pp:(pp_list pp_v) l l'

(* Tracing and checking function applications. *)

type app = (* Gathers information about the application *)
  { fail_count : int; (* fail_count checkpoint when the app starts *)
    pp_args : Format.formatter -> unit -> unit; }

let ctx () = { fail_count = -1; pp_args = fun ppf () -> (); }

let log_app_raised app exn =
  log "@[<2>@[%a@]==> raised %a" app.pp_args () pp_exn exn

let pp_app app pp_v ppf v =
  pp ppf "@[<2>@[%a@]==>@ @[%a@]@]" app.pp_args () pp_v v

let log_app app pp_v v = log "%a" (pp_app app pp_v) v

let ( $ ) f k = k (ctx ()) f

let ( @-> ) (pp_v : 'a pp) k app f v =
  let pp_args ppf () = app.pp_args ppf (); pp ppf "%a@ " pp_v v in
  let fc = if app.fail_count = -1 then !fail_count else app.fail_count in
  let app = { fail_count = fc; pp_args } in
  try k app (f v) with
  | Fail ->
      log_app app pp_v v;
      raise Fail_handled
  | Fail_handled as e -> raise e
  | exn ->
      log_app_raised app exn;
      fail "unexpected exception %a raised" pp_exn exn;
      raise Fail_handled

let ret pp app v =
  if !fail_count <> app.fail_count then log_app app pp v;
  v

let ret_eq ~eq pp r app v =
  if eq r v then (pass (); ret pp app v) else
  (fail "@[<v>%a@,%a@]" (pp_neq pp) (r, v) (pp_app app pp) v;
   raise Fail_handled)

let ret_none pp app v = match v with
| None -> pass (); ret (pp_option pp) app v
| Some _ -> ret_eq ~eq:(=) (pp_option pp) None app v

let ret_some pp app v = match v with
| Some _ as v -> pass (); ret (pp_option pp) app v
| None as v ->
    fail "@[<v>Some _ <> None@,%a@]" (pp_app app (pp_option pp)) v;
    raise Fail_handled

let ret_get_option pp app v = match ret_some pp app v with
| Some v -> v
| None -> assert false

(* I think we could handle the following functions on app traced ones
   by enriching the app type and have alternate functions to $ for
   handling these cases. Note that the only place were we can check
   for these things are in the @-> combinator *)

let app_invalid ~pp f v =
  try
    let r = f v in
    fail "%a <> exception Invalid_arg _" pp r
  with
  | Invalid_argument _ -> pass ()
  | exn -> fail "exception %a <> exception Invalid_arg _" pp_exn exn

let app_exn ~pp e f v =
  try
    let r = f v in
    fail "%a <> exception %a" pp r pp_exn e
  with
  | exn when exn = e -> pass ()
  | exn -> fail "exception %a <> exception %a_" pp_exn exn pp_exn e

let app_raises ~pp f v =
  try
    let r = f v in
    fail "%a <> exception _ " pp r
  with
  | exn -> pass ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

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
