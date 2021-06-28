(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open PosixTypes

type tm
val tm_sec : (int, tm structure) field
val tm_min : (int, tm structure) field
val tm_hour : (int, tm structure) field
val tm_mday : (int, tm structure) field
val tm_mon : (int, tm structure) field
val tm_year : (int, tm structure) field
val tm_wday : (int, tm structure) field
val tm_yday : (int, tm structure) field
val tm_isdst : (int, tm structure) field

val time : time_t ptr -> time_t
val asctime : tm structure ptr -> string
val localtime : time_t ptr -> tm structure ptr
