(* Variable-byte encoding of 8-byte integers (starting from 0). *)

open Printf
open Bi_outbuf
open Bi_inbuf

type uint = int

(* Maximum length of a vint decodable into an OCaml int,
   maximum value of the highest byte of the largest vint supported *)
let max_vint_bytes, max_highest_byte =
  if Bi_util.int_size mod 7 = 0 then
    let m = Bi_util.int_size / 7 in
    let h = 1 lsl 7 - 1 in
    m, h
  else
    let m = Bi_util.int_size / 7 + 1 in
    let h = 1 lsl (Bi_util.int_size mod 7) - 1 in
    m, h

let check_highest_byte x =
  if x > max_highest_byte then
    Bi_util.error "Vint exceeding range of OCaml ints"


let unsigned_of_signed i =
  if i >= 0 then
    (*
      0 -> 0
      1 -> 2
      2 -> 4
      3 -> 6
    *)
    i lsl 1
  else
    (*
      -1 -> 1
      -2 -> 3
      -3 -> 5
    *)
    ((-1-i) lsl 1) lor 1

let signed_of_unsigned i =
  if i land 1 = 0 then i lsr 1
  else -1 - (i lsr 1)

let write_uvint buf i  =
  Bi_outbuf.extend buf max_vint_bytes;

  let x = ref i in
  while !x lsr 7 <> 0 do
    let byte = 0x80 lor (!x land 0x7f) in
    Bi_outbuf.unsafe_add_char buf (Char.chr byte);
    x := !x lsr 7;
  done;
  Bi_outbuf.unsafe_add_char buf (Char.chr !x)

let write_svint buf i =
  write_uvint buf (unsigned_of_signed i)

(* convenience *)
let uvint_of_uint ?buf i =
  let buffer =
    match buf with
      | None -> Bi_outbuf.create 10
      | Some b -> b
  in
  Bi_outbuf.clear buffer;
  write_uvint buffer i;
  Bi_outbuf.contents buffer

let svint_of_int ?buf i =
  uvint_of_uint ?buf (unsigned_of_signed i)


let read_uvint ib =
  let avail = Bi_inbuf.try_preread ib max_vint_bytes in
  let s = ib.i_s in
  let pos = ib.i_pos in
  let x = ref 0 in
  (try
     for i = 0 to avail - 1 do
       let b = Char.code (Bytes.get s (pos+i)) in
       x := ((b land 0x7f) lsl (7*i)) lor !x;
       if b < 0x80 then (
	 ib.i_pos <- pos + i + 1;
	 if i + 1 = max_vint_bytes then
	   check_highest_byte b;
	 raise Exit
       )
     done;
     Bi_util.error "Unterminated vint or vint exceeding range of OCaml ints"
   with Exit -> ()
  );
  !x


let read_svint ib =
  signed_of_unsigned (read_uvint ib)

(* convenience *)

let check_end_of_input ib =
  if Bi_inbuf.try_preread ib 1 > 0 then
    Bi_util.error "Junk input after end of vint"

let uint_of_uvint s =
  let ib = Bi_inbuf.from_string s in
  let x = read_uvint ib in
  check_end_of_input ib;
  x

let int_of_svint s =
  let ib = Bi_inbuf.from_string s in
  let x = read_svint ib in
  check_end_of_input ib;
  x


(*
  Testing
*)

let string_of_list l =
  let ob = Bi_outbuf.create 100 in
  List.iter (write_uvint ob) l;
  Bi_outbuf.contents ob

let rec read_list ib =
  if ib.i_pos < ib.i_len then
    let x = read_uvint ib in
    x :: read_list ib
  else
    []

let list_of_string s =
  read_list (Bi_inbuf.from_string s)

let print_list l =
  List.iter (
    fun i ->
      printf "dec %i\nhex %x\nbin %s\n" i i
	(Bi_util.print_bits (Bi_util.string8_of_int i))
  ) l

let test () =
  let l = [
    0;
    0xfffffff;
    (0x01020304 lsl 32) lor 0x05060708;
    max_int;
    min_int
  ] in
  printf "Input:\n";
  print_list l;
  let l' = list_of_string (string_of_list l) in
  printf "Output:\n";
  print_list l';
  if l = l' then
    print_endline "SUCCESS"
  else
    print_endline "FAILURE"
