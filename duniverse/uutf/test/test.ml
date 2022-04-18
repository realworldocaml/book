(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uutf programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let u_nl = Uchar.of_int 0x000A
let log f = Format.printf (f ^^ "@?")
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let fail_decode e f =
  fail "expected %a, decoded %a" Uutf.pp_decode e Uutf.pp_decode f

let uchar_succ u = if Uchar.equal u Uchar.max then u else Uchar.succ u
let iter_uchars f =
  for u = 0x0000 to 0xD7FF do f (Uchar.unsafe_of_int u) done;
  for u = 0xE000 to 0x10FFFF do f (Uchar.unsafe_of_int u) done

let codec_test () =
  let codec_uchars encoding s bsize =
    log "Codec every unicode scalar value in %s with buffer size %d.\n%!"
      (Uutf.encoding_to_string encoding) bsize;
    let encode_uchars encoding s bsize =
      let spos = ref 0 in
      let e = Uutf.encoder encoding `Manual in
      let rec encode e v = match Uutf.encode e v with `Ok -> ()
      | `Partial ->
          let brem = Bytes.length s - !spos in
          let drem = Uutf.Manual.dst_rem e in
          let bsize = min bsize brem in
          Uutf.Manual.dst e s !spos bsize;
          spos := !spos + bsize - drem;
          encode e `Await
      in
      let encode_u u = encode e (`Uchar u) in
      iter_uchars encode_u; encode e `End;
      !spos - Uutf.Manual.dst_rem e                       (* encoded length. *)
    in
    let decode_uchars encoding s slen bsize =
      let spos = ref 0 in
      let bsize = min bsize slen in
      let d = Uutf.decoder ~encoding `Manual in
      let rec decode d = match Uutf.decode d with
      | `Malformed _ | `Uchar _ | `End as v -> v
      | `Await ->
          let rem = slen - !spos in
          let bsize = min bsize rem in
          Uutf.Manual.src d s !spos bsize;
          spos := !spos + bsize;
          decode d
      in
      let decode_u u = match decode d with
      | `Uchar u' when u = u' -> ()
      | v -> fail_decode (`Uchar u) v
      in
      iter_uchars decode_u;
      match decode d with
      | `End -> () | v -> fail_decode `End v
    in
    let slen = encode_uchars encoding s bsize in
    decode_uchars encoding s slen bsize
  in
  let full = 4 * 0x10FFFF in        (* will hold everything in any encoding. *)
  let s = Bytes.create full in
  let test encoding =
    (* Test with various sizes to increase condition coverage. *)
    for i = 1 to 11 do codec_uchars encoding s i done;
    codec_uchars encoding s full;
  in
  test `UTF_8; test `UTF_16BE; test `UTF_16LE

let buffer_string_codec_test () =
  let codec_uchars encoding encode decode b =
    log "Buffer/String codec every unicode scalar value in %s.\n%!"
      (Uutf.encoding_to_string encoding);
    Buffer.clear b;
    iter_uchars (encode b);
    let s = Buffer.contents b in
    let check uchar _ = function
    | `Uchar u when Uchar.equal u uchar -> uchar_succ uchar
    | v -> fail_decode (`Uchar uchar) v
    in
    ignore (decode ?pos:None ?len:None check (Uchar.of_int 0x0000) s)
  in
  let b = Buffer.create (4 * 0x10FFFF) in
  codec_uchars `UTF_8 Uutf.Buffer.add_utf_8 Uutf.String.fold_utf_8 b;
  codec_uchars `UTF_16BE Uutf.Buffer.add_utf_16be Uutf.String.fold_utf_16be b;
  codec_uchars `UTF_16LE Uutf.Buffer.add_utf_16le Uutf.String.fold_utf_16le b

let pos_test () =
  let test encoding s =
    log "Test position tracking in %s.\n%!" (Uutf.encoding_to_string encoding);
    let pos d (l, c, k) =
      match Uutf.decoder_line d, Uutf.decoder_col d, Uutf.decoder_count d with
      | (l', c', k') when l = l' && c = c' && k = k' -> ignore (Uutf.decode d)
      | (l', c', k') ->
          fail "Expected position (%d,%d,%d) found (%d,%d,%d)." l c k l' c' k'
    in
    let e = Uutf.decoder ~encoding (`String s) in
    pos e (1, 0, 0); pos e (1, 1, 1); pos e (1, 2, 2); pos e (2, 0, 3);
    pos e (2, 1, 4); pos e (3, 0, 5); pos e (3, 0, 6); pos e (3, 1, 7);
    pos e (3, 2, 8); pos e (4, 0, 9); pos e (4, 0, 10); pos e (5, 0, 11);
    pos e (6, 0, 12); pos e (6, 0, 12); pos e (6, 0, 12);
    let e = Uutf.decoder ~nln:(`ASCII u_nl) ~encoding (`String s) in
    pos e (1, 0, 0); pos e (1, 1, 1); pos e (1, 2, 2); pos e (2, 0, 3);
    pos e (2, 1, 4); pos e (3, 0, 5); pos e (3, 1, 6); pos e (3, 2, 7);
    pos e (4, 0, 8); pos e (5, 0, 9); pos e (6, 0, 10); pos e (6, 0, 10);
    pos e (6, 0, 10);
    let e = Uutf.decoder ~nln:(`NLF u_nl) ~encoding (`String s) in
    pos e (1, 0, 0); pos e (1, 1, 1); pos e (1, 2, 2); pos e (2, 0, 3);
    pos e (2, 1, 4); pos e (3, 0, 5); pos e (3, 1, 6); pos e (3, 2, 7);
    pos e (4, 0, 8); pos e (5, 0, 9); pos e (6, 0, 10); pos e (6, 0, 10);
    pos e (6, 0, 10);
    let e = Uutf.decoder ~nln:(`Readline u_nl) ~encoding (`String s) in
    pos e (1, 0, 0); pos e (1, 1, 1); pos e (1, 2, 2); pos e (2, 0, 3);
    pos e (2, 1, 4); pos e (3, 0, 5); pos e (3, 1, 6); pos e (3, 2, 7);
    pos e (4, 0, 8); pos e (5, 0, 9); pos e (6, 0, 10); pos e (6, 0, 10);
    pos e (6, 0, 10);
  in
  test `UTF_8 "LL\nL\r\nLL\r\n\n\x0C";
  test `UTF_16BE
    "\x00\x4C\x00\x4C\x00\x0A\x00\x4C\x00\x0D\x00\x0A\x00\x4C\x00\x4C\
     \x00\x0D\x00\x0A\x00\x0A\x00\x0C";
  test `UTF_16LE
    "\x4C\x00\x4C\x00\x0A\x00\x4C\x00\x0D\x00\x0A\x00\x4C\x00\x4C\x00\
     \x0D\x00\x0A\x00\x0A\x00\x0C\x00";
  ()

let guess_test () =
  log "Test encoding guessing.\n%!";
  let test (s, enc, removed_bom, seq) =
    let d = Uutf.decoder (`String s) in
    let rec test_seq seq d = match seq, Uutf.decode d with
    | `Uchar u :: vs, `Uchar u' when Uchar.equal u u' -> test_seq vs d
    | `Malformed bs :: vs, `Malformed bs' when bs = bs' -> test_seq vs d
    | [], `End -> ()
    | v :: _, v' -> fail_decode v v'
    | _ , _ -> assert false
    in
    test_seq seq d;
    let guess = Uutf.decoder_encoding d in
    if guess <> enc then fail "expected encoding: %s guessed: %s"
      (Uutf.encoding_to_string enc) (Uutf.encoding_to_string guess);
    let rem_bom = Uutf.decoder_removed_bom d in
    if rem_bom <> removed_bom then
      fail "expected removed bom: %b found: %b" removed_bom rem_bom
  in
  let uchar u = `Uchar (Uchar.unsafe_of_int u) in
  (* UTF-8 guess *)
  test ("", `UTF_8, false, []);
  test ("\xEF", `UTF_8, false, [`Malformed "\xEF";]);
  test ("\xEF\xBB", `UTF_8, false, [`Malformed "\xEF\xBB";]);
  test ("\xEF\xBB\x00", `UTF_8, false, [`Malformed "\xEF\xBB\x00";]);
  test ("\xEF\xBB\xBF\xEF\xBB\xBF", `UTF_8, true, [`Uchar Uutf.u_bom;]);
  test ("\n\r\n", `UTF_8, false, [`Uchar u_nl; uchar 0x0D; `Uchar u_nl;]);
  test ("\n\x80\xEF\xBB\xBF\n", `UTF_8, false,
        [`Uchar u_nl; `Malformed "\x80"; `Uchar Uutf.u_bom; `Uchar u_nl]);
  test ("\n\n\xEF\xBB\x00\n", `UTF_8, false,
        [`Uchar u_nl; `Uchar u_nl; `Malformed "\xEF\xBB\x00"; `Uchar u_nl;]);
  test ("\n\xC8\x99", `UTF_8, false, [`Uchar u_nl; uchar 0x0219;]);
  test ("\xC8\x99\n", `UTF_8, false, [uchar 0x0219; `Uchar u_nl;]);
  test ("\xC8\x99\n\n", `UTF_8, false,
        [uchar 0x0219; `Uchar u_nl; `Uchar u_nl]);
  test ("\xC8\x99\xC8\x99", `UTF_8, false, [uchar 0x0219; uchar 0x0219]);
  test ("\xC8\x99\xF0\x9F\x90\xAB", `UTF_8, false,
        [uchar 0x0219; uchar 0x1F42B]);
  test ("\xF0\x9F\x90\xAB\n", `UTF_8, false, [uchar 0x1F42B; `Uchar u_nl ]);
  (* UTF-16BE guess *)
  test ("\xFE\xFF\xDB\xFF\xDF\xFF\x00\x0A", `UTF_16BE, true,
        [uchar 0x10FFFF; `Uchar u_nl;]);
  test ("\xFE\xFF\xDB\xFF\x00\x0A\x00\x0A", `UTF_16BE, true,
       [`Malformed "\xDB\xFF\x00\x0A"; `Uchar u_nl;]);
  test ("\xFE\xFF\xDB\xFF\xDF", `UTF_16BE, true,
        [`Malformed "\xDB\xFF\xDF";]);
  test ("\x80\x81\xDB\xFF\xDF\xFF\xFE\xFF\xDF\xFF\xDB\xFF", `UTF_16BE, false,
        [uchar 0x8081; uchar 0x10FFFF; `Uchar Uutf.u_bom;
          `Malformed "\xDF\xFF"; `Malformed "\xDB\xFF"]);
  test ("\x80\x81\xDF\xFF\xDB\xFF\xFE", `UTF_16BE, false,
        [uchar 0x8081; `Malformed "\xDF\xFF"; `Malformed "\xDB\xFF\xFE";]);
  test ("\x00\x0A", `UTF_16BE, false, [`Uchar u_nl]);
  test ("\x00\x0A\xDB", `UTF_16BE, false, [`Uchar u_nl; `Malformed "\xDB"]);
  test ("\x00\x0A\xDB\xFF", `UTF_16BE, false,
        [`Uchar u_nl; `Malformed "\xDB\xFF"]);
  test ("\x00\x0A\xDB\xFF\xDF", `UTF_16BE, false,
        [`Uchar u_nl; `Malformed "\xDB\xFF\xDF"]);
  test ("\x00\x0A\xDB\xFF\xDF\xFF", `UTF_16BE, false,
        [`Uchar u_nl; uchar 0x10FFFF]);
  test ("\x00\x0A\x00\x0A", `UTF_16BE, false,
        [`Uchar u_nl; `Uchar u_nl]);
  (* UTF-16LE guess *)
  test ("\xFF\xFE\xFF\xDB\xFF\xDF\x0A\x00", `UTF_16LE, true,
        [uchar 0x10FFFF; `Uchar u_nl;]);
  test ("\xFF\xFE\xFF\xDB\x0A\x00\x0A\x00", `UTF_16LE, true,
       [`Malformed "\xFF\xDB\x0A\x00"; `Uchar u_nl;]);
  test ("\xFF\xFE\xFF\xDB\xDF", `UTF_16LE, true,
        [`Malformed "\xFF\xDB\xDF";]);
  test ("\x0A\x00", `UTF_16LE, false, [`Uchar u_nl]);
  test ("\x0A\x00\xDB", `UTF_16LE, false, [`Uchar u_nl; `Malformed "\xDB"]);
  test ("\x0A\x00\xFF\xDB", `UTF_16LE, false,
        [`Uchar u_nl; `Malformed "\xFF\xDB"]);
  test ("\x0A\x00\xFF\xDB\xDF", `UTF_16LE, false,
        [`Uchar u_nl; `Malformed "\xFF\xDB\xDF"]);
  test ("\x0A\x00\xFF\xDB\xFF\xDF", `UTF_16LE, false,
        [`Uchar u_nl; uchar 0x10FFFF]);
  test ("\x0A\x00\x0A\x00", `UTF_16LE, false,
        [`Uchar u_nl; `Uchar u_nl]);
  ()

let test_sub () =
  log "Test Uutf.String.fold_utf_8 substring";
  let trip fold ~pos ~len s =
    let b = Buffer.create 100 in
    let add _ _ = function
    | `Uchar u -> Uutf.Buffer.add_utf_8 b u
    | `Malformed _ -> assert false
    in
    fold ?pos:(Some pos) ?len:(Some len) add () s;
    assert (String.sub s pos len = Buffer.contents b);
  in
  trip Uutf.String.fold_utf_8 ~pos:4 ~len:4 "hop hap mop";
  trip Uutf.String.fold_utf_8 ~pos:0 ~len:1 "hop hap mop";
  trip Uutf.String.fold_utf_8 ~pos:2 ~len:1 "hop";
  ()

module Int = struct type t = int let compare : int -> int -> int = compare end
module Umap = Map.Make (Uchar)
module Bmap = Map.Make (Bytes)

(* Constructs from the specification, the map from uchars to their valid
   UTF-8 byte sequence and the map reverse map from valid UTF-8 byte sequences
   to their uchar.  *)
let utf8_maps () =
  log "Building UTF-8 codec maps from specification.\n";
  let spec = [        (* UTF-8 byte sequences cf. table 3.7 p. 94 Unicode 6. *)
    (0x0000,0x007F),     [|(0x00,0x7F)|];
    (0x0080,0x07FF),     [|(0xC2,0xDF); (0x80,0xBF)|];
    (0x0800,0x0FFF),     [|(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)|];
    (0x1000,0xCFFF),     [|(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)|];
    (0xD000,0xD7FF),     [|(0xED,0xED); (0x80,0x9F); (0x80,0xBF)|];
    (0xE000,0xFFFF),     [|(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)|];
    (0x10000,0x3FFFF),   [|(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)|];
    (0x40000,0xFFFFF),   [|(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)|];
    (0x100000,0x10FFFF), [|(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)|]]
  in
  let add_range (umap, bmap) ((umin, umax), bytes) =
    let len = Array.length bytes in
    let bmin i = if i < len then fst bytes.(i) else max_int in
    let bmax i = if i < len then snd bytes.(i) else min_int in
    let umap = ref umap in
    let bmap = ref bmap in
    let uchar = ref umin in
    let buf = Bytes.create len in
    let add len' =
      if len <> len' then () else
      begin
        let bytes = Bytes.copy buf in
        let u = Uchar.of_int !uchar in
        umap := Umap.add u bytes !umap;
        bmap := Bmap.add bytes u !bmap;
        incr uchar;
      end
    in
    for b0 = bmin 0 to bmax 0 do
      Bytes.unsafe_set buf 0 (Char.chr b0);
      for b1 = bmin 1 to bmax 1 do
        Bytes.unsafe_set buf 1 (Char.chr b1);
        for b2 = bmin 2 to bmax 2 do
          Bytes.unsafe_set buf 2 (Char.chr b2);
          for b3 = bmin 3 to bmax 3 do
            Bytes.unsafe_set buf 3 (Char.chr b3);
            add 4;
          done;
          add 3;
        done;
        add 2;
      done;
      add 1;
    done;
    assert (!uchar - 1 = umax);
    (!umap, !bmap)
  in
  List.fold_left add_range (Umap.empty, Bmap.empty)  spec

let utf8_encode_test umap =
  log "Testing UTF-8 encoding of every unicode scalar value against spec.\n";
  let buf = Buffer.create 4 in
  let test u =
    let u = Uchar.unsafe_of_int u in
    let bytes = try Umap.find u umap with Not_found -> assert false in
    let bytes = Bytes.unsafe_to_string bytes in
    Buffer.clear buf; Uutf.Buffer.add_utf_8 buf u;
    if bytes = Buffer.contents buf then () else
    fail "UTF-8 encoding error (U+%04X)" (Uchar.to_int u)
  in
  for i = 0x0000 to 0xD7FF do test i done;
  for i = 0xE000 to 0x10FFFF do test i done

let utf8_decode_test bmap =
  log "Testing the UTF-8 decoding of all <= 4 bytes sequences (be patient).\n";
  let spec seq = try `Uchar (Bmap.find seq bmap) with
  | Not_found -> `Malformed (Bytes.unsafe_to_string seq)
  in
  let test seq =
    let sseq = Bytes.unsafe_to_string seq in
    let dec = List.rev (Uutf.String.fold_utf_8 (fun a _ c -> c :: a) [] sseq) in
    match spec seq, dec with
    | `Uchar u, [ `Uchar u' ] when u = u' -> `Decoded
    | `Malformed _, (`Malformed _) :: _ -> `Malformed
    | v, v' :: _ -> fail_decode v v'
    | _ -> fail "This should not have happened on specification '%S'." sseq
  in
  let s1 = Bytes.create 1
  and s2 = Bytes.create 2
  and s3 = Bytes.create 3
  and s4 = Bytes.create 4
  in
  for b0 = 0x00 to 0xFF do
    Bytes.unsafe_set s1 0 (Char.unsafe_chr b0);
    if test s1 = `Decoded then ()
    else begin
      Bytes.unsafe_set s2 0 (Char.unsafe_chr b0);
      for b1 = 0x00 to 0xFF do
        Bytes.unsafe_set s2 1 (Char.unsafe_chr b1);
	if test s2 = `Decoded then ()
        else begin
          Bytes.unsafe_set s3 0 (Char.unsafe_chr b0);
          Bytes.unsafe_set s3 1 (Char.unsafe_chr b1);
	  for b2 = 0x00 to 0xFF do
            Bytes.unsafe_set s3 2 (Char.unsafe_chr b2);
	    if test s3 = `Decoded then ()
            else begin
              Bytes.unsafe_set s4 0 (Char.unsafe_chr b0);
              Bytes.unsafe_set s4 1 (Char.unsafe_chr b1);
              Bytes.unsafe_set s4 2 (Char.unsafe_chr b2);
	      for b3 = 0x00 to 0xFF do
                Bytes.unsafe_set s4 3 (Char.unsafe_chr b3);
		ignore (test s4)
	      done;
	    end
	  done;
	end
      done;
    end
  done

let utf8_test () =                             (* Proof by exhaustiveness... *)
  let umap, bmap = utf8_maps () in
  utf8_encode_test umap;
(*  utf8_decode_test bmap; *)                        (* too long, commented. *)
  ()

let is_uchar_test () =
  log "Testing Uchar.is_valid.\n";
  let test cp expected =
    let is = Uchar.is_valid cp in
    if is <> expected then
    fail "Uutf.is_uchar %04X = %b, expected %b" cp is expected
  in
  for cp = 0x0000 to 0xD7FF do test cp true done;
  for cp = 0xD800 to 0xDFFF do test cp false done;
  for cp = 0xE000 to 0x10FFFF do test cp true done;
  for cp = 0x110000 to 0x120000 do test cp false done

let test () =
  Printexc.record_backtrace true;
  codec_test ();
  buffer_string_codec_test ();
  pos_test ();
  guess_test ();
  test_sub ();
  utf8_test ();
  is_uchar_test ();
  log "All tests succeeded.\n"

let () = if not (!Sys.interactive) then test ()

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uutf programmers

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
