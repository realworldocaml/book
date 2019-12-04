type t = {
  mutable o_s : bytes;
  mutable o_max_len : int;
  mutable o_len : int;
  mutable o_offs : int;
  o_init_len : int;
  o_make_room : (t -> int -> unit);
  mutable o_shared : Bi_share.Wr.tbl;
  o_shared_init_len : int;
}

let really_extend b n =
  let slen0 = b.o_max_len in
  let reqlen = b.o_len + n in
  let slen =
    let x = max reqlen (2 * slen0) in
    if x <= Sys.max_string_length then x
    else
      if Sys.max_string_length < reqlen then
	invalid_arg "Buf.extend: reached Sys.max_string_length"
      else
	Sys.max_string_length
  in
  let s = Bytes.create slen in
  Bytes.blit b.o_s 0 s 0 b.o_len;
  b.o_s <- s;
  b.o_max_len <- slen

let flush_to_output abstract_output b n =
  abstract_output (Bytes.to_string b.o_s) 0 b.o_len;
  b.o_offs <- b.o_offs + b.o_len;
  b.o_len <- 0;
  if n > b.o_max_len then
    really_extend b n

let flush_to_channel oc = flush_to_output (output_substring oc)


let create ?(make_room = really_extend) ?(shrlen = 16) n = {
  o_s = Bytes.create n;
  o_max_len = n;
  o_len = 0;
  o_offs = 0;
  o_init_len = n;
  o_make_room = make_room;
  o_shared = Bi_share.Wr.create shrlen;
  o_shared_init_len = shrlen;
}

let create_channel_writer ?(len = 4096) ?shrlen oc =
  create ~make_room:(flush_to_channel oc) ?shrlen len

let flush_channel_writer b =
  b.o_make_room b 0

let create_output_writer ?(len = 4096) ?shrlen out =
  create ~make_room:(flush_to_output out#output) ?shrlen len

let flush_output_writer = flush_channel_writer


(*
  Guarantee that the buffer string has enough room for n additional bytes.
*)
let extend b n =
  if b.o_len + n > b.o_max_len then
    b.o_make_room b n

let alloc b n =
  extend b n;
  let pos = b.o_len in
  b.o_len <- pos + n;
  pos

let add_sub blit b s pos len =
  extend b len;
  blit s pos b.o_s b.o_len len;
  b.o_len <- b.o_len + len

let add_substring = add_sub String.blit
let add_subbytes = add_sub Bytes.blit

let add_string b s =
  add_substring b s 0 (String.length s)

let add_bytes b s =
  add_subbytes b s 0 (Bytes.length s)


let add_char b c =
  let pos = alloc b 1 in
  Bytes.set b.o_s pos c

let unsafe_add_char b c =
  let len = b.o_len in
  Bytes.set b.o_s len c;
  b.o_len <- len + 1

let add_char2 b c1 c2 =
  let pos = alloc b 2 in
  let s = b.o_s in
  Bytes.unsafe_set s pos c1;
  Bytes.unsafe_set s (pos+1) c2

let add_char4 b c1 c2 c3 c4 =
  let pos = alloc b 4 in
  let s = b.o_s in
  Bytes.unsafe_set s pos c1;
  Bytes.unsafe_set s (pos+1) c2;
  Bytes.unsafe_set s (pos+2) c3;
  Bytes.unsafe_set s (pos+3) c4



let clear b =
  b.o_offs <- 0;
  b.o_len <- 0;
  Bi_share.Wr.clear b.o_shared

let reset b =
  if Bytes.length b.o_s <> b.o_init_len then
    b.o_s <- Bytes.create b.o_init_len;
  b.o_offs <- 0;
  b.o_len <- 0;
  b.o_shared <- Bi_share.Wr.create b.o_shared_init_len

let contents b = Bytes.sub_string b.o_s 0 b.o_len
