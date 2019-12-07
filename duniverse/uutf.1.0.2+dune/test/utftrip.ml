(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let pp = Format.fprintf
let pp_pos ppf d = pp ppf "%d.%d:(%d,%06X) "
  (Uutf.decoder_line d) (Uutf.decoder_col d) (Uutf.decoder_count d)
  (Uutf.decoder_byte_count d)

let pp_decode inf d ppf v =
  pp ppf "@[<h>%s:%a%a@]@\n" inf pp_pos d Uutf.pp_decode v

let exec = Filename.basename Sys.executable_name
let log f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec

let input_malformed = ref false
let log_malformed inf d v =
  input_malformed := true; log "%a" (pp_decode inf d) v

(* IO tools  *)

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
    (Buffer.add_substring b (Bytes.unsafe_to_string s) 0 rc; loop b input s)
  in
  loop b input s

let string_to_channel use_unix oc s =
  if not use_unix then output_string oc s else
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

let rec encode_unix fd s e v = match Uutf.encode e v with `Ok -> ()
| `Partial ->
    unix_write fd s 0 (Bytes.length s - Uutf.Manual.dst_rem e);
    Uutf.Manual.dst e s 0 (Bytes.length s);
    encode_unix fd s e `Await

(* Dump *)

let dump_decode inf d v =
  (match v with `Malformed _ -> input_malformed := true | _ -> ());
  (pp_decode inf d) Format.std_formatter v

let dump_ inf encoding nln src =
  let rec loop inf d = match Uutf.decode d with `Await -> assert false
  | v ->
      dump_decode inf d v;
      if v <> `End then loop inf d
  in
  loop inf (Uutf.decoder ?nln ?encoding src)

let dump_unix inf encoding nln usize fd =
  let rec loop fd s d = match Uutf.decode d with
  | `Await ->
      let rc = unix_read fd s 0 (Bytes.length s) in
      Uutf.Manual.src d s 0 rc; loop fd s d
  | v -> dump_decode inf d v; if v <> `End then loop fd s d
  in
  loop fd (Bytes.create usize) (Uutf.decoder ?nln ?encoding `Manual)

let dump inf sin use_unix usize ie nln =
  if sin || not use_unix then dump_ inf ie nln (src_for inf sin use_unix) else
  dump_unix inf ie nln usize (src_for_unix inf)

(* Guess only *)

let guess inf =
  let d = Uutf.decoder (src_for inf false false) in
  ignore (Uutf.decode d);
  Format.printf "%s@." (Uutf.encoding_to_string (Uutf.decoder_encoding d))

(* Decode only *)

let decode_ inf encoding nln src =
  let malformed = log_malformed inf in
  let rec loop d = match Uutf.decode d with `Await -> assert false
  | `Uchar _ -> loop d
  | `End -> ()
  | `Malformed _ as v -> malformed d v; loop d
  in
  loop (Uutf.decoder ?nln ?encoding src); close_src src

let decode_unix inf encoding nln usize fd =
  let malformed = log_malformed inf in
  let rec loop fd s d = match Uutf.decode d with
  | `Uchar _ -> loop fd s d
  | `End -> ()
  | `Malformed _ as v -> malformed d v; loop fd s d
  | `Await ->
      let rc = unix_read fd s 0 (Bytes.length s) in
      Uutf.Manual.src d s 0 rc; loop fd s d
  in
  loop fd (Bytes.create usize) (Uutf.decoder ?nln ?encoding `Manual);
  close_src_unix fd

let decode inf sin use_unix usize ie nln =
  if sin || not use_unix then decode_ inf ie nln (src_for inf sin use_unix) else
  decode_unix inf ie nln usize (src_for_unix inf)

(* Random encode only *)

let u_surrogate_count = 0xDFFF - 0xD800 + 1
let uchar_count = (0x10FFFF + 1) - u_surrogate_count
let r_uchar () =
  let n = Random.int uchar_count in
  Uchar.of_int (if n > 0xD7FF then n + u_surrogate_count else n)

let r_text encoding encode_f rcount =
  encode_f (`Uchar Uutf.u_bom);
  for i = 1 to rcount do encode_f (`Uchar (r_uchar ())) done;
  encode_f `End

let encode_f encoding dst =
  let e = Uutf.encoder encoding dst in
  fun v -> match Uutf.encode e v with `Ok -> () | `Partial -> assert false

let encode_f_unix usize encoding fd =
  let e, s = Uutf.encoder encoding `Manual, Bytes.create usize in
  Uutf.Manual.dst e s 0 (Bytes.length s);
  encode_unix fd s e

let r_encode sout use_unix usize rseed rcount oe =
  let rseed = match rseed with
  | None -> Random.self_init (); Random.int (1 lsl 30 - 1)
  | Some rseed -> rseed
  in
  let dst = dst_for sout in
  let oe = match oe with None -> `UTF_8 | Some enc -> enc in
  let encode_f =
    if sout || not use_unix then encode_f oe dst else
    encode_f_unix usize oe Unix.stdout
  in
  log "Encoding %d random characters with seed %d\n" rcount rseed;
  Random.init rseed; r_text oe encode_f rcount;
  match dst with `Channel _ -> ()
  | `Buffer b -> string_to_channel use_unix stdout (Buffer.contents b)

(* Trip *)

let trip_ inf nln ie oe src dst =
  let malformed d v e =
    log_malformed inf d v; ignore (Uutf.encode e (`Uchar Uutf.u_rep))
  in
  let rec loop d e = function `Await -> assert false
  | `Uchar _ as v -> ignore (Uutf.encode e v); loop d e (Uutf.decode d)
  | `End -> ignore (Uutf.encode e `End)
  | `Malformed _ as v -> malformed d v e; loop d e (Uutf.decode d)
  in
  let d = Uutf.decoder ?nln ?encoding:ie src in
  let e, first = match oe with
  | Some enc -> Uutf.encoder enc dst, (Uutf.decode d)
  | None ->
      let v = Uutf.decode d in                          (* get the encoding. *)
      let enc = match Uutf.decoder_encoding d with
      | #Uutf.encoding as enc -> enc | `ISO_8859_1 | `US_ASCII -> `UTF_8
      in
      Uutf.encoder enc dst, v
  in
  if (Uutf.encoder_encoding e = `UTF_16 || Uutf.decoder_removed_bom d)
  then ignore (Uutf.encode e (`Uchar Uutf.u_bom));
  loop d e first; close_src src

let trip_unix inf usize nln ie oe fdi fdo =
  let malformed  d v e =
    log_malformed inf d v; ignore (Uutf.encode e (`Uchar Uutf.u_rep))
  in
  let rec loop fdi fdo ds es d e = function
  | `Uchar _ as v ->
      encode_unix fdo es e v; loop fdi fdo ds es d e (Uutf.decode d)
  | `End -> encode_unix fdo es e `End
  | `Malformed _ as v -> malformed d v e; loop fdi fdo ds es d e (Uutf.decode d)
  | `Await ->
      let rc = unix_read fdi ds 0 (Bytes.length ds) in
      Uutf.Manual.src d ds 0 rc; loop fdi fdo ds es d e (Uutf.decode d)
  in
  let d, ds = Uutf.decoder ?nln ?encoding:ie `Manual, Bytes.create usize in
  let e, es, first = match oe with
  | Some enc -> Uutf.encoder enc `Manual, Bytes.create usize, (Uutf.decode d)
  | None ->
      let rec decode_past_await d = match Uutf.decode d with
      | `Await ->
          let rc = unix_read fdi ds 0 (Bytes.length ds) in
          Uutf.Manual.src d ds 0 rc; decode_past_await d
      | v -> v
      in
      let v = decode_past_await d in                        (* get encoding. *)
      let enc = match Uutf.decoder_encoding d with
      | #Uutf.encoding as enc -> enc | `ISO_8859_1 | `US_ASCII -> `UTF_8
      in
      Uutf.encoder enc `Manual, Bytes.create usize, v
  in
  Uutf.Manual.dst e es 0 (Bytes.length es);
  if (Uutf.encoder_encoding e = `UTF_16 || Uutf.decoder_removed_bom d)
  then encode_unix fdo es e (`Uchar Uutf.u_bom);
  loop fdi fdo ds es d e first; close_src_unix fdi

let trip inf sin sout use_unix usize ie oe nln =
  let src = src_for inf sin use_unix in
  let dst = dst_for sout in
  if sin || sout || not use_unix then trip_ inf nln ie oe src dst else
  trip_unix inf usize nln ie oe (src_for_unix inf) Unix.stdout;
  match dst with `Channel _ -> ()
  | `Buffer b -> string_to_channel use_unix stdout (Buffer.contents b)

(* Cmd *)

let do_cmd cmd inf sin sout use_unix usize ie oe nln rseed rcount =
  match cmd with
  | `Ascii -> dump inf sin use_unix usize ie nln
  | `Guess -> guess inf
  | `Decode -> decode inf sin use_unix usize ie nln
  | `Encode -> r_encode sout use_unix usize rseed rcount oe
  | `Trip -> trip inf sin sout use_unix usize ie oe nln

(* Cmdline interface *)

open Cmdliner

let enc_enum =
  [ "UTF-8", `UTF_8; "UTF-16", `UTF_16; "UTF-16LE", `UTF_16LE;
    "UTF-16BE", `UTF_16BE; ]

let decode_enc_enum =
  ("ASCII", `US_ASCII) :: ("latin1", `ISO_8859_1) :: enc_enum

let ienc =
  let doc = str "Decoded (input) encoding, must %s. If unspecified the
                 encoding is guessed."
                (Arg.doc_alts_enum decode_enc_enum)
  in
  Arg.(value & opt (some (enum decode_enc_enum)) None &
       info ["d"; "input-encoding"] ~doc)

let oenc =
  let doc = str "Encoded (output) encoding, must %s. If unspecified the output
                 encoding is the same as the input encoding except for ASCII
                 and latin1 where UTF-8 is output." (Arg.doc_alts_enum enc_enum)
  in
  Arg.(value & opt (some (enum enc_enum)) None &
       info ["e"; "output-encoding"] ~doc)

let nln =
  let lf = Uchar.of_int 0x000A in
  let nln_enum = ["ascii", `ASCII lf; "nlf", `NLF lf; "readline", `Readline lf]
  in
  let doc = str "New line normalization to U+000A, must %s. ascii
                 normalizes CR (U+000D) and CRLF (<U+000D, U+000A>). nlf
                 normalizes like ascii plus NEL (U+0085). readline
                 normalizes like nlf plus FF (U+000C), LS (U+2028) and
                 PS (U+2029)."
      (Arg.doc_alts_enum nln_enum)
  in
  let vopt = Some (`Readline lf) in
  Arg.(value & opt ~vopt (some (enum nln_enum)) None & info ["nln"] ~doc)

let sin =
  let doc = "Input everything in a string and decode the string." in
  Arg.(value & flag & info [ "input-string" ] ~doc)

let sout =
  let doc = "Encode everything in a string and output the string." in
  Arg.(value & flag & info [ "output-string" ] ~doc)

let use_unix =
  let doc = "Use Unix IO." in
  Arg.(value & flag & info [ "use-unix" ] ~doc)

let usize =
  let doc = "Unix IO buffer sizes in bytes." in
  Arg.(value & opt int unix_buffer_size & info ["unix-size"] ~doc)

let nat =
  let parse s =
    try
      let v = int_of_string s in
      if v > 0 then `Ok v else failwith (str "%s must be > 0" s)
    with Failure e -> `Error e
  in
  parse, Format.pp_print_int

let rseed =
  let doc = "Random seed." in
  Arg.(value & opt (some nat) None & info ["rseed"] ~doc)

let rcount =
  let doc = "Number of random characters to generate." in
  Arg.(value & opt nat 1_000_000 & info ["rcount"] ~doc)

let file =
  let doc = "The input file. Reads from stdin if unspecified." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")

let cmd =
  let doc = "Output the input text as Unicode scalar values or malformed
             sequences, one per line, in the US-ASCII charset with their
             position (see POSITION INFORMATION for more details)."
  in
  let ascii = `Ascii, Arg.info ["a"; "ascii"] ~doc in
  let doc = "Only guess an UTF encoding. The result of a guess can only be
             UTF-8 or UTF-16{LE,BE}."
  in
  let guess = `Guess, Arg.info ["g"; "guess"] ~doc in
  let doc = "Decode only, no encoding." in
  let dec = `Decode, Arg.info ["decode"] ~doc in
  let doc = "Encode only (random), no decoding. See option $(b,--rcount)." in
  let enc = `Encode, Arg.info ["encode"] ~doc in
  Arg.(value & vflag `Trip [ascii; guess; dec; enc])

let cmd =
  let doc = "Recode UTF-{8,16,16LE,16BE} and latin1 from stdin to stdout." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) inputs Unicode text from stdin and rewrites it
        to stdout in various ways. If no input encoding is specified,
        it is guessed. If no output encoding is specified, the input
        encoding is used.";
    `P "Invalid byte sequences in the input are reported on stderr and
        replaced by the Unicode replacement character (U+FFFD) in the output.";
    `S "POSITION INFORMATION";
    `P "The format for position information is:";
    `P "filename:line.col:(count,byte)";
    `I ("line", "one-based line number that increments with each newline.
        A newline is always determined as being anything that would be
        normalized by the option `$(b,--nln)=readline`.");
    `I ("col", "zero-based column number that increment with each new
        decoded character and zeroes after a newline
        is decoded. Note that the column number may not correspond to
        user-perceived columns, as any Unicode scalar value, including
        combining characters are deemed to have a width of 1.");
    `I ("count", "the one-based Unicode scalar value count.");
    `I ("byte", "the zero-based end byte offset of the scalar value
                 in the input stream in hexadecimal.");
    `S "EXIT STATUS";
    `P "$(tname) exits with one of the following values:";
    `I ("0", "no error occured");
    `I ("1", "a command line parsing error occured");
    `I ("2", "the input text was malformed");
    `S "BUGS";
    `P "This program is distributed with the Uutf OCaml library.
        See http://erratique.ch/software/uutf for contact
        information."; ]
  in
  Term.(pure do_cmd $ cmd $ file $ sin $ sout $ use_unix $ usize $
        ienc $ oenc $ nln $ rseed $ rcount),
  Term.info "utftrip" ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval cmd with
| `Error _ -> exit 1
| _ -> if !input_malformed then exit 2 else exit 0

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
