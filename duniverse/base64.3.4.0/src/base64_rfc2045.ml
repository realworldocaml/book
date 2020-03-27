(*
 * Copyright (c) 2018 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let default_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let io_buffer_size = 65536
let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len =
  invalid_arg "Invalid bounds (off: %d, len: %d)" off len

let malformed chr =
  `Malformed (String.make 1 chr)

let unsafe_byte source off pos = Bytes.unsafe_get source (off + pos)
let unsafe_blit = Bytes.unsafe_blit
let unsafe_chr = Char.unsafe_chr
let unsafe_set_chr source off chr = Bytes.unsafe_set source off chr

type state = {quantum: int; size: int; buffer: Bytes.t}

let continue state (quantum, size) = `Continue {state with quantum; size}
let flush state = `Flush {state with quantum= 0; size= 0}

let table =
  "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\062\255\255\255\063\052\053\054\055\056\057\058\059\060\061\255\255\255\255\255\255\255\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\255\255\255\255\255\255\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\041\042\043\044\045\046\047\048\049\050\051\255\255\255\255\255"

let r_repr ({quantum; size; _} as state) chr =
  (* assert (0 <= off && 0 <= len && off + len <= String.length source); *)
  (* assert (len >= 1); *)
  let code = Char.code table.[Char.code chr] in
  match size with
  | 0 -> continue state (code, 1)
  | 1 -> continue state ((quantum lsl 6) lor code, 2)
  | 2 -> continue state ((quantum lsl 6) lor code, 3)
  | 3 ->
      unsafe_set_chr state.buffer 0 (unsafe_chr ((quantum lsr 10) land 255)) ;
      unsafe_set_chr state.buffer 1 (unsafe_chr ((quantum lsr 2) land 255)) ;
      unsafe_set_chr state.buffer 2
        (unsafe_chr ((quantum lsl 6) lor code land 255)) ;
      flush state
  | _ -> malformed chr

type src = [`Channel of in_channel | `String of string | `Manual]

type decode =
  [`Await | `End | `Wrong_padding | `Malformed of string | `Flush of string]

type input =
  [`Line_break | `Wsp | `Padding | `Malformed of string | `Flush of state]

type decoder =
  { src: src
  ; mutable i: Bytes.t
  ; mutable i_off: int
  ; mutable i_pos: int
  ; mutable i_len: int
  ; mutable s: state
  ; mutable padding: int
  ; mutable unsafe: bool
  ; mutable byte_count: int
  ; mutable limit_count: int
  ; mutable pp: decoder -> input -> decode
  ; mutable k: decoder -> decode }

let i_rem decoder = decoder.i_len - decoder.i_pos + 1

let end_of_input decoder =
  decoder.i <- Bytes.empty ;
  decoder.i_off <- 0 ;
  decoder.i_pos <- 0 ;
  decoder.i_len <- min_int

let src decoder source off len =
  if off < 0 || len < 0 || off + len > Bytes.length source then
    invalid_bounds off len
  else if len = 0 then end_of_input decoder
  else (
    decoder.i <- source ;
    decoder.i_off <- off ;
    decoder.i_pos <- 0 ;
    decoder.i_len <- len - 1 )

let refill k decoder =
  match decoder.src with
  | `Manual ->
      decoder.k <- k ;
      `Await
  | `String _ -> end_of_input decoder ; k decoder
  | `Channel ic ->
      let len = input ic decoder.i 0 (Bytes.length decoder.i) in
      src decoder decoder.i 0 len ;
      k decoder

let dangerous decoder v = decoder.unsafe <- v
let reset decoder = decoder.limit_count <- 0

let ret k v byte_count decoder =
  decoder.k <- k ;
  decoder.byte_count <- decoder.byte_count + byte_count ;
  decoder.limit_count <- decoder.limit_count + byte_count ;
  if decoder.limit_count > 78 then dangerous decoder true ;
  decoder.pp decoder v

type flush_and_malformed = [`Flush of state | `Malformed of string]

let padding {size; _} padding =
  match (size, padding) with
  | 0, 0 -> true
  | 1, _ -> false
  | 2, 2 -> true
  | 3, 1 -> true
  | _ -> false

let t_flush {quantum; size; buffer} =
  match size with
  | 0 | 1 -> `Flush {quantum; size; buffer= Bytes.empty}
  | 2 ->
      let quantum = quantum lsr 4 in
      `Flush
        { quantum
        ; size
        ; buffer= Bytes.make 1 (unsafe_chr (quantum land 255)) }
  | 3 ->
      let quantum = quantum lsr 2 in
      unsafe_set_chr buffer 0 (unsafe_chr ((quantum lsr 8) land 255)) ;
      unsafe_set_chr buffer 1 (unsafe_chr (quantum land 255)) ;
      `Flush {quantum; size; buffer= Bytes.sub buffer 0 2}
  | _ -> assert false (* this branch is impossible, size can only ever be in the range [0..3]. *)

let wrong_padding decoder =
  let k _ = `End in
  decoder.k <- k ; `Wrong_padding

let rec t_decode_base64 chr decoder =
  if decoder.padding = 0 then
    let rec go pos = function
      | `Continue state ->
          if decoder.i_len - (decoder.i_pos + pos) + 1 > 0
          then (
            match unsafe_byte decoder.i decoder.i_off (decoder.i_pos + pos) with
            | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/') as chr -> go (succ pos) (r_repr state chr)
            | '=' ->
                decoder.padding <- decoder.padding + 1 ;
                decoder.i_pos <- decoder.i_pos + pos + 1 ;
                decoder.s <- state ;
                ret decode_base64 `Padding (pos+1)  decoder
            | ' ' | '\t' ->
                decoder.i_pos <- decoder.i_pos + pos + 1 ;
                decoder.s <- state ;
                ret decode_base64 `Wsp (pos + 1) decoder
            | '\r' ->
                decoder.i_pos <- decoder.i_pos + pos + 1 ;
                decoder.s <- state ;
                decode_base64_lf_after_cr decoder
            | chr ->
                decoder.i_pos <- decoder.i_pos + pos + 1 ;
                decoder.s <- state ;
                ret decode_base64 (malformed chr) (pos+1) decoder
          ) else (
            decoder.i_pos <- decoder.i_pos + pos ;
            decoder.byte_count <- decoder.byte_count + pos ;
            decoder.limit_count <- decoder.limit_count + pos ;
            decoder.s <- state ;
            refill decode_base64 decoder )
      | #flush_and_malformed as v ->
          decoder.i_pos <- decoder.i_pos + pos ;
          ret decode_base64 v pos decoder
    in
    go 1 (r_repr decoder.s chr)
  else (
    decoder.i_pos <- decoder.i_pos + 1 ;
    ret decode_base64 (malformed chr) 1 decoder)

and decode_base64_lf_after_cr decoder =
  let rem = i_rem decoder in
  if rem < 0 then
    ret decode_base64 (malformed '\r') 1 decoder
  else if rem = 0 then refill decode_base64_lf_after_cr decoder
  else
    match unsafe_byte decoder.i decoder.i_off decoder.i_pos with
    | '\n' ->
      decoder.i_pos <- decoder.i_pos + 1 ;
      ret decode_base64 `Line_break 2 decoder
    | _ ->
      ret decode_base64 (malformed '\r') 1 decoder

and decode_base64 decoder =
  let rem = i_rem decoder in
  if rem <= 0 then
    if rem < 0 then
      ret
        (fun decoder ->
          if padding decoder.s decoder.padding then `End else wrong_padding decoder )
        (t_flush decoder.s) 0 decoder
    else refill decode_base64 decoder
  else
    match unsafe_byte decoder.i decoder.i_off decoder.i_pos with
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/') as chr ->
        t_decode_base64 chr decoder
    | '=' ->
        decoder.padding <- decoder.padding + 1 ;
        decoder.i_pos <- decoder.i_pos + 1 ;
        ret decode_base64 `Padding 1 decoder
    | ' ' | '\t' ->
        decoder.i_pos <- decoder.i_pos + 1 ;
        ret decode_base64 `Wsp 1 decoder
    | '\r' ->
        decoder.i_pos <- decoder.i_pos + 1 ;
        decode_base64_lf_after_cr decoder
    | chr ->
        decoder.i_pos <- decoder.i_pos + 1 ;
        ret decode_base64 (malformed chr) 1 decoder

let pp_base64 decoder = function
  | `Line_break -> reset decoder ; decoder.k decoder
  | `Wsp | `Padding -> decoder.k decoder
  | `Flush state ->
      decoder.s <- state ;
      `Flush (Bytes.to_string state.buffer)
  | `Malformed _ as v -> v

let decoder src =
  let pp = pp_base64 in
  let k = decode_base64 in
  let i, i_off, i_pos, i_len =
    match src with
    | `Manual -> (Bytes.empty, 0, 1, 0)
    | `Channel _ -> (Bytes.create io_buffer_size, 0, 1, 0)
    | `String s -> (Bytes.unsafe_of_string s, 0, 0, String.length s - 1)
  in
  { src
  ; i_off
  ; i_pos
  ; i_len
  ; i
  ; s= {quantum= 0; size= 0; buffer= Bytes.create 3}
  ; padding= 0
  ; unsafe= false
  ; byte_count= 0
  ; limit_count= 0
  ; pp
  ; k }

let decode decoder = decoder.k decoder
let decoder_byte_count decoder = decoder.byte_count
let decoder_src decoder = decoder.src
let decoder_dangerous decoder = decoder.unsafe

(* / *)

let invalid_encode () = invalid_arg "Expected `Await encode"

type dst = [`Channel of out_channel | `Buffer of Buffer.t | `Manual]
type encode = [`Await | `End | `Char of char]

type encoder =
  { dst: dst
  ; mutable o: Bytes.t
  ; mutable o_off: int
  ; mutable o_pos: int
  ; mutable o_len: int
  ; mutable c_col: int
  ; i: Bytes.t
  ; mutable s: int
  ; t: Bytes.t
  ; mutable t_pos: int
  ; mutable t_len: int
  ; mutable k: encoder -> encode -> [`Ok | `Partial] }

let o_rem encoder = encoder.o_len - encoder.o_pos + 1

let dst encoder source off len =
  if off < 0 || len < 0 || off + len > Bytes.length source then
    invalid_bounds off len ;
  encoder.o <- source ;
  encoder.o_off <- off ;
  encoder.o_pos <- 0 ;
  encoder.o_len <- len - 1

let dst_rem = o_rem

let partial k encoder = function
  | `Await -> k encoder
  | `Char _ | `End -> invalid_encode ()

let flush k encoder =
  match encoder.dst with
  | `Manual ->
      encoder.k <- partial k ;
      `Partial
  | `Channel oc ->
      output oc encoder.o encoder.o_off encoder.o_pos ;
      encoder.o_pos <- 0 ;
      k encoder
  | `Buffer b ->
      let o = Bytes.unsafe_to_string encoder.o in
      Buffer.add_substring b o encoder.o_off encoder.o_pos ;
      encoder.o_pos <- 0 ;
      k encoder

let t_range encoder len =
  encoder.t_pos <- 0 ;
  encoder.t_len <- len

let rec t_flush k encoder =
  let blit encoder len =
    unsafe_blit encoder.t encoder.t_pos encoder.o encoder.o_pos len ;
    encoder.o_pos <- encoder.o_pos + len ;
    encoder.t_pos <- encoder.t_pos + len
  in
  let rem = o_rem encoder in
  let len = encoder.t_len - encoder.t_pos + 1 in
  if rem < len then (
    blit encoder rem ;
    flush (t_flush k) encoder )
  else ( blit encoder len ; k encoder )

let rec encode_line_break k encoder =
  let rem = o_rem encoder in
  let s, j, k =
    if rem < 2 then (
      t_range encoder 2 ;
      (encoder.t, 0, t_flush k) )
    else
      let j = encoder.o_pos in
      encoder.o_pos <- encoder.o_pos + 2 ;
      (encoder.o, encoder.o_off + j, k)
  in
  unsafe_set_chr s j '\r' ;
  unsafe_set_chr s (j + 1) '\n' ;
  encoder.c_col <- 0 ;
  k encoder

and encode_char chr k (encoder : encoder) =
  if encoder.s >= 2 then (
    let a, b, c =
      (unsafe_byte encoder.i 0 0, unsafe_byte encoder.i 0 1, chr)
    in
    encoder.s <- 0 ;
    let quantum = (Char.code a lsl 16) + (Char.code b lsl 8) + Char.code c in
    let a = quantum lsr 18 in
    let b = (quantum lsr 12) land 63 in
    let c = (quantum lsr 6) land 63 in
    let d = quantum land 63 in
    let rem = o_rem encoder in
    let s, j, k =
      if rem < 4 then (
        t_range encoder 4 ;
        (encoder.t, 0, t_flush (k 4)) )
      else
        let j = encoder.o_pos in
        encoder.o_pos <- encoder.o_pos + 4 ;
        (encoder.o, encoder.o_off + j, k 4)
    in
    unsafe_set_chr s j default_alphabet.[a] ;
    unsafe_set_chr s (j + 1) default_alphabet.[b] ;
    unsafe_set_chr s (j + 2) default_alphabet.[c] ;
    unsafe_set_chr s (j + 3) default_alphabet.[d] ;
    flush k encoder )
  else (
    unsafe_set_chr encoder.i encoder.s chr ;
    encoder.s <- encoder.s + 1 ;
    k 0 encoder )

and encode_trailing k encoder =
  match encoder.s with
  | 2 ->
      let b, c = (unsafe_byte encoder.i 0 0, unsafe_byte encoder.i 0 1) in
      encoder.s <- 0 ;
      let quantum = (Char.code b lsl 10) + (Char.code c lsl 2) in
      let b = (quantum lsr 12) land 63 in
      let c = (quantum lsr 6) land 63 in
      let d = quantum land 63 in
      let rem = o_rem encoder in
      let s, j, k =
        if rem < 4 then (
          t_range encoder 4 ;
          (encoder.t, 0, t_flush (k 4)) )
        else
          let j = encoder.o_pos in
          encoder.o_pos <- encoder.o_pos + 4 ;
          (encoder.o, encoder.o_off + j, k 4)
      in
      unsafe_set_chr s j default_alphabet.[b] ;
      unsafe_set_chr s (j + 1) default_alphabet.[c] ;
      unsafe_set_chr s (j + 2) default_alphabet.[d] ;
      unsafe_set_chr s (j + 3) '=' ;
      flush k encoder
  | 1 ->
      let c = unsafe_byte encoder.i 0 0 in
      encoder.s <- 0 ;
      let quantum = Char.code c lsl 4 in
      let c = (quantum lsr 6) land 63 in
      let d = quantum land 63 in
      let rem = o_rem encoder in
      let s, j, k =
        if rem < 4 then (
          t_range encoder 4 ;
          (encoder.t, 0, t_flush (k 4)) )
        else
          let j = encoder.o_pos in
          encoder.o_pos <- encoder.o_pos + 4 ;
          (encoder.o, encoder.o_off + j, k 4)
      in
      unsafe_set_chr s j default_alphabet.[c] ;
      unsafe_set_chr s (j + 1) default_alphabet.[d] ;
      unsafe_set_chr s (j + 2) '=' ;
      unsafe_set_chr s (j + 3) '=' ;
      flush k encoder
  | 0 -> k 0 encoder
  | _ -> assert false

and encode_base64 encoder v =
  let k col_count encoder =
    encoder.c_col <- encoder.c_col + col_count ;
    encoder.k <- encode_base64 ;
    `Ok
  in
  match v with
  | `Await -> k 0 encoder
  | `End ->
      if encoder.c_col = 76 then
        encode_line_break (fun encoder -> encode_base64 encoder v) encoder
      else encode_trailing k encoder
  | `Char chr ->
      let rem = o_rem encoder in
      if rem < 1 then flush (fun encoder -> encode_base64 encoder v) encoder
      else if encoder.c_col = 76 then
        encode_line_break (fun encoder -> encode_base64 encoder v) encoder
      else encode_char chr k encoder

let encoder dst =
  let o, o_off, o_pos, o_len =
    match dst with
    | `Manual -> (Bytes.empty, 1, 0, 0)
    | `Buffer _ | `Channel _ ->
        (Bytes.create io_buffer_size, 0, 0, io_buffer_size - 1)
  in
  { dst
  ; o_off
  ; o_pos
  ; o_len
  ; o
  ; t= Bytes.create 4
  ; t_pos= 1
  ; t_len= 0
  ; c_col= 0
  ; i= Bytes.create 3
  ; s= 0
  ; k= encode_base64 }

let encode encoder = encoder.k encoder
let encoder_dst encoder = encoder.dst
