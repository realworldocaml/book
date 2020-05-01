open Printf
open Bin_prot
open Common
open Bin_prot.Std


(* Checks correct behavior for empty types *)
type empty [@@deriving bin_io]

module type S = sig
  (* Checks correct behavior for type signatures with variance annotations. *)
  type +'a t [@@deriving bin_io]
end

module PolyInhTest = struct
  type x = [ `X1 | `X2 ] [@@deriving bin_io]
  type y = [ `Y1 | `Y2 ] [@@deriving bin_io]
  type xy = [ x | `FOO | `Bar of int * float | y ] [@@deriving bin_io]
end

type tuple = float * string * int64
[@@deriving bin_io]

type 'a record = { a : int; b : 'a; c : 'a option }
[@@deriving bin_io]

type 'a singleton_record = { y : 'a }
[@@deriving bin_io]

type 'a inline_record =
  | IR of { ir_a : int; ir_b : 'a; ir_c : 'a option }
  | Other of int
[@@deriving bin_io]

type 'a sum = Foo | Bar of int | Bla of 'a * string
[@@deriving bin_io]

type 'a variant = [ `Foo | `Bar of int | `Bla of 'a * string ]
[@@deriving bin_io]

type 'a poly_app = (tuple * int singleton_record * 'a record * 'a inline_record) variant sum list
[@@deriving bin_io]

type 'a rec_t1 = RecFoo1 of 'a rec_t2
and 'a rec_t2 = RecFoo2 of 'a poly_app * 'a rec_t1 | RecNone
[@@deriving bin_io]

type 'a poly_id = 'a rec_t1
[@@deriving bin_io]

type el = float poly_id
[@@deriving bin_io]

type els = el array
[@@deriving bin_io]

let mb = 1024. *. 1024.

let main () =
  (* Allocate buffer (= bigstring) *)
  let buf = create_buf 10000 in

  (* Define array of dummy elements to be marshalled *)
  let el =
    let record = { a = 17; b = 2.78; c = None } in
    let inline_record = IR {ir_a = 18; ir_b = 43210.; ir_c = None} in
    let arg = (3.1, "foo", 42L), { y = 4321 }, record, inline_record in
    let variant = `Bla (arg, "fdsa") in
    let sum = Bla (variant, "asdf") in
    let poly_app = [ sum ] in
    RecFoo1 (RecFoo2 (poly_app, RecFoo1 RecNone))
  in
  let x = Array.make 10 el in

  let n = 100_000 in

  (* Write n times *)
  let t1 = Unix.gettimeofday () in
  for _ = 1 to n do
    ignore (bin_write_els buf ~pos:0 x)
  done;
  let t2 = Unix.gettimeofday () in
  let write_time = t2 -. t1 in

  (* Read n times *)
  let t1 = Unix.gettimeofday () in
  for _ = 1 to n do
    let pos_ref = ref 0 in
    ignore (bin_read_els buf ~pos_ref)
  done;
  let t2 = Unix.gettimeofday () in
  let read_time = t2 -. t1 in

  (* Write, read, and verify *)
  let end_pos = bin_write_els buf ~pos:0 x in
  let pos_ref = ref 0 in
  let y = bin_read_els buf ~pos_ref in
  assert (!pos_ref = end_pos && x = y);

  (* Print result *)
  let f_n = float n in
  let msg_size = float (n * end_pos) in
  printf
    "msgs: %d  msg length: %d\n\
    write time: %.3fs  write rate: %9.2f msgs/s  write throughput: %.2f MB/s\n\
    \ read time: %.3fs   read rate: %9.2f msgs/s   read throughput: %.2f MB/s\n%!"
    n end_pos
    write_time (f_n /. write_time) (msg_size /. write_time /. mb)
    read_time (f_n /. read_time) (msg_size /. read_time /. mb)

let () =
  try main ()
  with Read_error (err, pos) ->
    eprintf "Uncaught exception: %s: %d\n%!" (ReadError.to_string err) pos
