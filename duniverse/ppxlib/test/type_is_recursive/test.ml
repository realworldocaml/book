open Ppxlib

let test_is_recursive stri =
  match stri.pstr_desc with
  | Pstr_type (rf, tds) -> really_recursive rf tds
  | _ -> assert false

[%%expect{|
val test_is_recursive : structure_item -> rec_flag = <fun>
|}]

let loc = Location.none

[%%expect{|
val loc : location =
  {Ppxlib.Location.loc_start =
    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
   loc_end =
    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
   loc_ghost = true}
|}]

(* Should be Nonrecursive *)
let base_type = test_is_recursive [%stri type t = int]

[%%expect{|
val base_type : rec_flag = Ppxlib__.Import.Nonrecursive
|}]

(* Should be Nonrecursive *)
let looks_recursive_but_is_not = test_is_recursive [%stri type nonrec t = t]

[%%expect{|
val looks_recursive_but_is_not : rec_flag = Ppxlib__.Import.Nonrecursive
|}]

(* Should be Nonrecursive *)
let variant_non_rec = test_is_recursive [%stri type t = A of int | B of string]

[%%expect{|
val variant_non_rec : rec_flag = Ppxlib__.Import.Nonrecursive
|}]

(* Should be Nonrecursive *)
let record_non_rec = test_is_recursive [%stri type t = {a: int; b: string}]

[%%expect{|
val record_non_rec : rec_flag = Ppxlib__.Import.Nonrecursive
|}]

(* Should be Recursive *)
let actually_recursive = test_is_recursive [%stri type t = A of int | T of t]

[%%expect{|
val actually_recursive : rec_flag = Ppxlib__.Import.Recursive
|}]

(* Should be Nonrecursive *)
let ignore_attributes = test_is_recursive [%stri type t = int [@attr: t]]

[%%expect{|
val ignore_attributes : rec_flag = Ppxlib__.Import.Nonrecursive
|}]

(* Should be Recursive
   
   This is subject to debate. @ceastlund's intuition is that we should
   traverse extensions so we'll stick to this for now.

   It's less of a problem as it is likely that when [really_recursive] is called
   those will have been expanded anyway. *)
let extension_points = test_is_recursive [%stri type t = [%ext: t]]

[%%expect{|
val extension_points : rec_flag = Ppxlib__.Import.Recursive
|}]
