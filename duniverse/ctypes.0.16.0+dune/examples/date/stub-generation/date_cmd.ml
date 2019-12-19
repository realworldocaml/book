(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open PosixTypes
open Date_stubs
module D = Bindings(Date_generated)
open D

let () = begin
  let timep = allocate_n ~count:1 time_t in
  let time = time timep in
  assert (time = !@timep);
  let tm = localtime timep in
  Printf.printf "tm.tm_mon  = %d\n" (getf !@tm tm_mon);
  Printf.printf "tm.tm_year = %d\n" (getf !@tm tm_year);
  print_endline (asctime tm)
end
