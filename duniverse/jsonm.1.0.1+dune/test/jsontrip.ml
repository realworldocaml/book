(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let pr = Format.fprintf
let pr_range ppf ((l0, c0), (l1, c1)) = pr ppf "%d.%d-%d.%d" l0 c0 l1 c1
let pr_decode ppf inf d v = pr ppf "%s:%a: %a@\n@?"
  inf pr_range (Jsonm.decoded_range d) Jsonm.Uncut.pp_decode v

let exec = Filename.basename Sys.executable_name
let log f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec
let log_error inf d e = Format.eprintf "%s:%a: %a@\n@?"
  inf pr_range (Jsonm.decoded_range d) Jsonm.pp_error e

(* IO tools *)

let io_buffer_size = 65536                          (* IO_BUFFER_SIZE 4.0.0 *)
let unix_buffer_size = 65536                      (* UNIX_BUFFER_SIZE 4.0.0 *)

let rec unix_read fd s j l = try Unix.read fd s j l with
| Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l

let rec unix_write fd s j l =
  let rec write fd s j l = try Unix.single_write fd s j l with
  | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l
  in
  let wc = write fd s j l in
  if wc < l then unix_write fd s (j + wc) (l - wc) else ()

let string_of_channel use_unix ic =
  let b = Buffer.create unix_buffer_size in
  let input, s =
    if use_unix
    then unix_read (Unix.descr_of_in_channel ic), Bytes.create unix_buffer_size
    else input ic, Bytes.create io_buffer_size
  in
  let rec loop b input s =
    let rc = input s 0 (Bytes.length s) in
    if rc = 0 then Buffer.contents b else
    let us = Bytes.unsafe_to_string s in
    (Buffer.add_substring b us 0 rc; loop b input s)
  in
  loop b input s

let string_to_channel use_unix oc s = match use_unix with
| false -> output_string oc s
| true ->
    let s = Bytes.unsafe_of_string s in
    unix_write (Unix.descr_of_out_channel oc) s 0 (Bytes.length s)

let dst_for sout = if sout then `Buffer (Buffer.create 512) else `Channel stdout
let src_for inf sin use_unix =
  try
    let ic = if inf = "-" then stdin else open_in inf in
    if sin then `String (string_of_channel use_unix ic) else `Channel ic
  with Sys_error e -> log "%s\n" e; exit 1

let close_src src =
  try match src with `Channel ic when ic <> stdin -> close_in ic | _ -> () with
  | Sys_error e -> log "%s\n" e; exit 1

let src_for_unix inf =
  try if inf = "-" then Unix.stdin else Unix.(openfile inf [O_RDONLY] 0) with
  | Unix.Unix_error (e, _, v) -> log "%s: %s\n" (Unix.error_message e) v; exit 1

let close_src_unix fd = try if fd <> Unix.stdin then Unix.close fd with
| Unix.Unix_error (e, _, v) -> log "%s: %s\n" (Unix.error_message e) v; exit 1

let rec encode_unix encode fd s e v = match encode e v with `Ok -> ()
| `Partial ->
    unix_write fd s 0 (Bytes.length s - Jsonm.Manual.dst_rem e);
    Jsonm.Manual.dst e s 0 (Bytes.length s);
    encode_unix encode fd s e `Await

(* Dump *)

let dump_ inf encoding uncut src =
  let decode = if uncut then Jsonm.Uncut.decode else Jsonm.decode in
  let rec loop decode d = match decode d with `Await -> assert false
  | v -> pr_decode Format.std_formatter inf d v; if v <> `End then loop decode d
  in
  loop decode (Jsonm.decoder ?encoding src);
  close_src src

let dump_unix inf encoding uncut usize fd =
  let decode = if uncut then Jsonm.Uncut.decode else Jsonm.decode in
  let rec loop decode fd s d = match decode d with
  | `Await ->
      let rc = unix_read fd s 0 (Bytes.length s) in
      Jsonm.Manual.src d s 0 rc; loop decode fd s d
  | v ->
      pr_decode Format.std_formatter inf d v;
      if v <> `End then loop decode fd s d
  in
  loop decode fd (Bytes.create usize) (Jsonm.decoder ?encoding `Manual);
  close_src_unix fd

let dump inf sin use_unix usize ie uncut =
  if sin || not use_unix then dump_ inf ie uncut (src_for inf sin use_unix)
  else dump_unix inf ie uncut usize (src_for_unix inf)

(* Guess encoding *)

let guess inf =
  let d = Jsonm.decoder (src_for inf false false) in
  ignore (Jsonm.decode d);
  Format.printf "%s@." (Uutf.encoding_to_string (Jsonm.decoder_encoding d))

(* Decode only *)

let decode_ inf encoding uncut src =
  let decode = if uncut then Jsonm.Uncut.decode else Jsonm.decode in
  let rec loop decode d = match decode d with
  | `Lexeme _ ->  loop decode d
  | `End -> ()
  | `Comment _ | `White _  -> loop decode d
  | `Error e -> log_error inf d e; loop decode d
  | `Await -> assert false
  in
  loop decode (Jsonm.decoder ?encoding src)

let decode_unix inf encoding uncut usize fd =
  let decode = if uncut then Jsonm.Uncut.decode else Jsonm.decode in
  let rec loop decode fd s d = match decode d with
  | `Lexeme _ -> loop decode fd s d
  | `End -> ()
  | `Comment _ | `White _  -> loop decode fd s d
  | `Error e -> log_error inf d e; loop decode fd s d
  | `Await ->
      let rc = unix_read fd s 0 (Bytes.length s) in
      Jsonm.Manual.src d s 0 rc; loop decode fd s d
  in
  loop decode fd (Bytes.create usize) (Jsonm.decoder ?encoding `Manual)

let decode inf sin use_unix usize ie uncut =
  if sin || not use_unix then decode_ inf ie uncut (src_for inf use_unix sin)
  else decode_unix inf ie uncut usize Unix.stdin

(* Random encode only *)

let r_ascii_letter () =
  Uchar.unsafe_of_int (0x0061 (* a *) + Random.int 26)

let r_general_scripts () =
  Uchar.unsafe_of_int (Random.int 0x2000 (* < U+2000 *))

let max_rint = 9007199254740993L (* 2 ^ 53 + 1 *)
let r_int () =    (* random integer exactly representable by an OCaml float. *)
  let i = Random.int64 max_rint in
  Int64.to_float (if Random.bool () then Int64.neg i else i)

let r_float () =                           (* generate all string notations. *)
  let f = if (Random.bool ()) then Random.float 1e-5 else Random.float 1.5e12 in
  if (Random.bool ()) then ~-. f else f

let r_name buf maxs =
  Buffer.clear buf;
  for i = 0 to Random.int (maxs + 1)
  do Uutf.Buffer.add_utf_8 buf (r_ascii_letter ()) done;
  `Name (Buffer.contents buf)

let r_string buf maxs =
  Buffer.clear buf;
  for i = 0 to Random.int (maxs + 1)
  do Uutf.Buffer.add_utf_8 buf (r_general_scripts ()) done;
  `String (Buffer.contents buf)

let r_comment buf =
  Buffer.clear buf;
  let style = if Random.bool () then `M else `S in
  for i = 0 to Random.int 64 do
    let c = r_general_scripts () in
    let ci = Uchar.to_int c in
    (* avoid any // and */ sequence and control chars *)
    if ci != 0x002F (* / *) && ci > 0x001F then Uutf.Buffer.add_utf_8 buf c
  done;
  `Comment (style, Buffer.contents buf)

let r_white buf =
  Buffer.clear buf;
  for i = 0 to Random.int 3 do match Random.int 100 with
  | n when n < 90 -> Buffer.add_char buf ' '
  | n when n < 94 -> Buffer.add_char buf '\t'
  | n when n < 98 -> Buffer.add_char buf '\n'
  | n when n < 100 -> Buffer.add_char buf '\r'
  | n -> assert false
  done;
  `White (Buffer.contents buf)

let rec r_value k enc buf count ri maxd maxl maxs =
  let kontinue () = k enc buf (count - 1) ri maxd maxl maxs in
  match (if maxd = 0 then Random.int 4 else Random.int 6) with
  | 0 -> enc `Null; kontinue ()
  | 1 -> enc (`Bool (Random.bool ())); kontinue ()
  | 2 -> enc (`Float (if ri then r_int () else r_float ())); kontinue ()
  | 3 -> enc (r_string buf maxs); kontinue ()
  | 4 | 5 ->
      let bound = Random.int maxl + 1 in
      r_json bound k enc buf (count - 1) ri maxd maxl maxs
  | n -> assert false

and r_obj_ms bound k enc buf count ri maxd maxl maxs =
  if count = 0 || bound = 0
  then (enc `Oe; k enc buf count ri (maxd + 1) maxl maxs) else
  begin
    enc (r_name buf maxs);
    r_value (r_obj_ms (bound - 1) k) enc buf count ri maxd maxl maxs
  end

and r_arr_vs bound k enc buf count ri maxd maxl maxs =
  if count = 0 || bound = 0
  then (enc `Ae; k enc buf count ri (maxd + 1) maxl maxs)
  else r_value (r_arr_vs (bound - 1) k) enc buf count ri maxd maxl maxs

and r_json bound k enc buf count ri maxd maxl maxs =
  if Random.bool ()
  then (enc `Os; r_obj_ms bound k enc buf count ri (maxd - 1) maxl maxs)
  else (enc `As; r_arr_vs bound k enc buf count ri (maxd - 1) maxl maxs)

let r_json_text enc buf vcount ri maxd maxl maxs =
  let stop _ _ _ _ _ _ _ = enc `End in
  let encl l = enc (`Lexeme l) in
  r_json max_int stop encl buf (vcount - 1) ri maxd maxl maxs

let r_uncut enc buf = match Random.int 100 with
| n when n < 50 -> ()
| n when n < 90 -> enc (r_white buf)
| n when n < 100 -> enc (r_comment buf)
| n -> assert false

let encode_f buf uncut minify dst =
  let e = Jsonm.encoder ~minify dst in
  if not uncut then (fun v -> ignore (Jsonm.encode e v)) else
  let enc v = ignore (Jsonm.Uncut.encode e v) in
  fun v -> r_uncut enc buf; enc v; r_uncut enc buf

let encode_f_unix usize buf uncut minify fd =
  let e, s = Jsonm.encoder ~minify `Manual, Bytes.create usize in
  Jsonm.Manual.dst e s 0 (Bytes.length s);
  if not uncut then (fun v -> encode_unix Jsonm.encode fd s e v) else
  let enc v = encode_unix Jsonm.Uncut.encode fd s e v in
  fun v -> r_uncut enc buf; enc v; r_uncut enc buf

let r_encode sout use_unix usize uncut indent rseed rcount ri maxd maxl maxs =
  let dst = dst_for sout in
  let buf = Buffer.create maxs in
  let encode_f =
    if sout || not use_unix then encode_f buf uncut indent dst else
    encode_f_unix usize buf uncut indent Unix.stdout
  in
  log "Encoding random JSON text with seed %d\n" rseed;
  Random.init rseed; r_json_text encode_f buf rcount ri maxd maxl maxs;
  match dst with `Channel _ -> ()
  | `Buffer b -> string_to_channel use_unix stdout (Buffer.contents b)

(* Trip *)

let trip_ inf uncut minify encoding src dst =
  let decode = if uncut then Jsonm.Uncut.decode else Jsonm.decode in
  let rec loop decode d e = match decode d with
  | `Lexeme _ as v -> ignore (Jsonm.encode e v); loop decode d e
  | `End -> ignore (Jsonm.encode e `End)
  | `Comment _ | `White _  as v ->
      if not minify then ignore (Jsonm.Uncut.encode e v); loop decode d e
  | `Error err -> log_error inf d err
  | `Await -> assert false
  in
  let d = Jsonm.decoder src in
  let e = Jsonm.encoder ~minify:(minify || uncut) dst in
  loop decode d e; close_src src

let trip_unix inf usize uncut minify encoding fdi fdo =
  let decode = if uncut then Jsonm.Uncut.decode else Jsonm.decode in
  let rec loop decode fdi fdo ds es d e = match decode d with
  | `Lexeme _ as v ->
      encode_unix Jsonm.encode fdo es e v; loop decode fdi fdo ds es d e
  | `End -> encode_unix Jsonm.encode fdo es e `End
  | `Comment _ | `White _ as v ->
      if not minify then ignore (encode_unix Jsonm.Uncut.encode fdo es e v);
      loop decode fdi fdo ds es d e
  | `Error err -> log_error inf d err
  | `Await ->
      let rc = unix_read fdi ds 0 (Bytes.length ds) in
      Jsonm.Manual.src d ds 0 rc; loop decode fdi fdo ds es d e
  in
  let d, ds = Jsonm.decoder ?encoding `Manual, Bytes.create usize in
  let e, es = Jsonm.encoder ~minify `Manual, Bytes.create usize in
  Jsonm.Manual.dst e es 0 (Bytes.length es);
  loop decode fdi fdo ds es d e; close_src_unix fdi

let trip inf sin sout use_unix usize ie uncut minify =
  let src = src_for inf use_unix sin in
  let dst = dst_for sout in
  if sin || sout || not use_unix then trip_ inf uncut minify ie src dst else
  trip_unix inf usize uncut minify ie (src_for_unix inf) Unix.stdout;
  match dst with `Channel _ -> ()
  | `Buffer b -> string_to_channel use_unix stdout (Buffer.contents b)

let main () =
 let usage = Printf.sprintf
    "Usage: %s [OPTION]... [INFILE]\n\
     \ Recode JSON from stdin to stdout.\n\
     Options:" exec
  in
  let cmd = ref `Trip in
  let set_cmd v () = cmd := v in
  let inf = ref "-" in
  let set_inf f =
    if !inf <> "-" then raise (Arg.Bad "only one file can be specified") else
    inf := f
  in
  let ie = ref None in
  let ie_fun e = match Uutf.encoding_of_string e with
  | None | Some (`US_ASCII | `ISO_8859_1) ->
    log "unsupported input encoding '%s', using UTF-8\n" e
  | (Some #Jsonm.encoding) as enc -> ie := enc
  in
  let uncut = ref false in
  let minify = ref true in
  let sin = ref false in
  let sout = ref false in
  let use_unix = ref false in
  let usize = ref unix_buffer_size in
  let rseed = ref (Random.self_init (); Random.int (1 lsl 30 - 1)) in
  let rcount = ref 560_000 in (* params for ~10Mo of JSON. *)
  let rint = ref false in
  let maxd = ref 5 in
  let maxl = ref 20 in
  let maxs = ref 15 in
  let nat s r v = if v > 0 then r := v else log "%s must be > 0, ignored\n" s in
  let options = [
    "-dump", Arg.Unit (set_cmd `Dump),
    " Dump lexemes and their position, one per line";
    "-guess", Arg.Unit (set_cmd `Guess), " Only guess the encoding";
    "-dec", Arg.Unit (set_cmd `Decode), " Decode only, no encoding";
    "-enc", Arg.Unit (set_cmd `Encode), " Encode only (random), no decoding";
    "-ie", Arg.String ie_fun,
    "<enc> Input encoding: UTF-8, UTF-16, UTF-16BE or UTF-16LE";
    "-uncut", Arg.Set uncut,
    " Use the uncut codec (allows comments in the input)";
    "-pp", Arg.Clear minify, " Pretty print output (minified by default)";
    "-sin", Arg.Set sin, " Input as string and decode the string";
    "-sout", Arg.Set sout, " Encode as string and output the string";
    "-unix", Arg.Set use_unix, " Use Unix IO";
    "-usize", Arg.Int (nat "-usize" usize),
    "<int> Unix IO buffer sizes in bytes";
    "-rseed", Arg.Int (nat "-rseed" rseed), "<int> Random seed";
    "-rcount", Arg.Int (nat "-rcount" rcount),
    "<int> Number of JSON values in random JSON text";
    "-rint", Arg.Set rint, " Generate only integer JSON numbers (no floats)";
    "-maxd", Arg.Int (nat "-maxd" maxd),
    "<int> Maximal depth in random JSON text";
    "-maxl", Arg.Int (nat "-maxl" maxl),
    "<int> Maximal inner array and object length in random JSON";
    "-maxs", Arg.Int (nat "-maxs" maxs),
    "<int> Maximal string length in random JSON text"; ]
  in
  Arg.parse (Arg.align options) set_inf usage;
  match !cmd with
  | `Dump -> dump !inf !sin !use_unix !usize !ie !uncut
  | `Guess -> guess !inf
  | `Trip -> trip !inf !sin !sout !use_unix !usize !ie !uncut !minify
  | `Decode -> decode !inf !sin !use_unix !usize !ie !uncut
  | `Encode ->
      r_encode !sout !use_unix !usize !uncut !minify !rseed !rcount
        !rint !maxd !maxl !maxs

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli

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
