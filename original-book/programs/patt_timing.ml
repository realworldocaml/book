(*
 * Test the timing of pattern matches.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
let match_test1 i =
   match i with
      0 -> 0
    | 1 -> 2
    | 2 -> 4
    | 3 -> 6
    | 4 -> 8
    | 5 -> 10
    | 6 -> 12
    | 7 -> 14
    | 8 -> 16
    | 9 -> 18
    | 10 -> 20
    | 11 -> 22
    | 12 -> 24
    | 13 -> 26
    | 14 -> 28
    | 15 -> 30
    | 16 -> 32
    | 17 -> 34
    | 18 -> 36
    | 19 -> 38
    | _ -> i

let match_test2 i =
   match i with
      0 -> 0
    | 10 -> 3
    | 20 -> 6
    | 30 -> 9
    | 40 -> 12
    | 50 -> 15
    | 60 -> 18
    | 70 -> 21
    | 80 -> 24
    | 90 -> 27
    | 100 -> 30
    | 110 -> 33
    | 120 -> 36
    | 130 -> 39
    | 140 -> 42
    | 150 -> 45
    | 160 -> 48
    | 170 -> 51
    | 180 -> 54
    | 190 -> 57
    | _ -> i

let match_test3 i =
   match i with
      "b" -> 0
    | "ab" -> 1
    | "aab" -> 11
    | "aaab" -> 111
    | "aaaab" -> 1111
    | "aaaaab" -> 11111
    | "aaaaaab" -> 111111
    | "aaaaaaab" -> 1111111
    | "aaaaaaaab" -> 11111111
    | "aaaaaaaaab" -> 111111111
    | _ -> 0

let match_test4 i =
   match i with
    | "aaaaaaaaab" -> 111111111
    | "aaaaaaaab" -> 11111111
    | "aaaaaaab" -> 1111111
    | "aaaaaab" -> 111111
    | "aaaaab" -> 11111
    | "aaaab" -> 1111
    | "aaab" -> 111
    | "aab" -> 11
    | "ab" -> 1
    | "b" -> 1
    | _ -> 0

let rec repeat_test test_fun i count =
   if count <> 0 then
      let _ = test_fun i in
         repeat_test test_fun i (count - 1)

let int_test test_name test_fun i count =
   let time1 = Sys.time () in
   repeat_test test_fun i count;
   let time2 = Sys.time () in
      Printf.printf "%s %3d: %3.2f sec\n" test_name i (time2 -. time1);;

let string_test test_name test_fun s count =
   let time1 = Sys.time () in
   repeat_test test_fun s count;
   let time2 = Sys.time () in
      Printf.printf "%s %10s: %3.2f sec\n" test_name s (time2 -. time1);;

int_test "test1" match_test1 0  10000000;;
int_test "test1" match_test1 19 10000000;;
int_test "test1" match_test1 20 10000000;;

int_test "test2" match_test2 0   10000000;;
int_test "test2" match_test2 190 10000000;;
int_test "test2" match_test2 200 10000000;;

string_test "test3" match_test3 "b" 10000000;;
string_test "test3" match_test3 "aaaab" 10000000;;
string_test "test3" match_test3 "aaaaaaaaab" 10000000;;
string_test "test3" match_test3 "aaaaaaaaaa" 10000000;;
string_test "test3" match_test3 "caaaaaaaaa" 10000000;;

string_test "test4" match_test4 "b" 10000000;;
string_test "test4" match_test4 "aaaab" 10000000;;
string_test "test4" match_test4 "aaaaaaaaab" 10000000;;
string_test "test4" match_test4 "aaaaaaaaaa" 10000000;;
string_test "test4" match_test4 "caaaaaaaaa" 10000000;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
