type t = {
  mutable i_s : bytes;
  mutable i_pos : int;
  mutable i_len : int;
  mutable i_offs : int;
  mutable i_max_len : int;
  i_refill : (t -> int -> unit);
  i_shared : Bi_share.Rd.tbl;
}

exception End_of_input

let try_preread ib n =
  if ib.i_len - ib.i_pos < n then (
    ib.i_refill ib n;
    min (ib.i_len - ib.i_pos) n
  )
  else
    n

let read ib n =
  let pos = ib.i_pos in
  if ib.i_len - pos >= n then (
    ib.i_pos <- pos + n;
    pos
  )
  else
    if try_preread ib n >= n then
      let pos = ib.i_pos in
      ib.i_pos <- ib.i_pos + n;
      pos
    else
      raise End_of_input

let read_char ib =
  let pos = ib.i_pos in
  if ib.i_len - pos > 0 then (
    let c = Bytes.unsafe_get ib.i_s pos in
    ib.i_pos <- pos + 1;
    c
  )
  else
    if try_preread ib 1 > 0 then
      let pos = ib.i_pos in
      let c = Bytes.unsafe_get ib.i_s pos in
      ib.i_pos <- pos + 1;
      c
    else
      raise End_of_input

let peek ib =
  let pos = ib.i_pos in
  if ib.i_len - pos > 0 then (
    Bytes.unsafe_get ib.i_s pos
  )
  else
    if try_preread ib 1 > 0 then
      Bytes.unsafe_get ib.i_s ib.i_pos
    else
      raise End_of_input

let from_bytes ?(pos = 0) ?(shrlen = 16) s = {
  i_s = s;
  i_pos = pos;
  i_len = Bytes.length s;
  i_offs = -pos;
  i_max_len = Bytes.length s;
  i_refill = (fun ib n -> ());
  i_shared = Bi_share.Rd.create shrlen;
}

let from_string ?pos ?shrlen s = from_bytes ?pos ?shrlen (Bytes.of_string s)

(*
  Like Pervasives.really_input but returns the number of bytes
  read instead of raising End_of_file when the end of file is reached.
*)
let rec not_really_input ic s pos len accu =
  let n = input ic s pos len in
  if n < len && n > 0 then
    not_really_input ic s (pos + n) (len - n) (accu + n)
  else
    accu + n

let refill_from_channel ic ib n =
  if n > ib.i_max_len then
    invalid_arg "Bi_inbuf.refill_from_channel"
  else (
    let rem_len = ib.i_len - ib.i_pos in
    if rem_len < n then
      let s = ib.i_s in
      Bytes.blit s ib.i_pos s 0 rem_len;
      let to_read = n - rem_len in
      let really_read = not_really_input ic s rem_len to_read 0 in
      ib.i_offs <- ib.i_offs + ib.i_pos;
      ib.i_pos <- 0;
      ib.i_len <- rem_len + really_read
  )

let from_channel ?(len = 4096) ?(shrlen = 16) ic = {
  i_s = Bytes.create len;
  i_pos = 0;
  i_len = 0;
  i_offs = 0;
  i_max_len = len;
  i_refill = refill_from_channel ic;
  i_shared = Bi_share.Rd.create shrlen;
}
