(**************************************************************************)
(*                                                                        *)
(*  PPrint                                                                *)
(*                                                                        *)
(*  François Pottier, Inria Paris                                         *)
(*  Nicolas Pouillard                                                     *)
(*                                                                        *)
(*  Copyright 2007-2019 Inria. All rights reserved. This file is          *)
(*  distributed under the terms of the GNU Library General Public         *)
(*  License, with an exception, as described in the file LICENSE.         *)
(**************************************************************************)

open Printf
open PPrintEngine
open PPrintCombinators

type constructor = string
type type_name = string
type record_field = string
type tag = int

(* ------------------------------------------------------------------------- *)

(* This internal [sprintf]-like function produces a document. We use [string],
   as opposed to [arbitrary_string], because the strings that we produce will
   never contain a newline character. *)

let dsprintf format =
  ksprintf string format

(* ------------------------------------------------------------------------- *)

(* Nicolas prefers using this code as opposed to just [sprintf "%g"] or
   [sprintf "%f"]. The latter print [inf] and [-inf], whereas OCaml
   understands [infinity] and [neg_infinity]. [sprintf "%g"] does not add a
   trailing dot when the number happens to be an integral number.  [sprintf
   "%F"] seems to lose precision and ignores the precision modifier. *)

let valid_float_lexeme (s : string) : string =
  let l = String.length s in
  let rec loop i =
    if i >= l then
      (* If we reach the end of the string and have found only characters in
	 the set '0' .. '9' and '-', then this string will be considered as an
	 integer literal by OCaml. Adding a trailing dot makes it a float
	 literal. *)
      s ^ "."
    else
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> s
  in loop 0

(* This function constructs a string representation of a floating point
   number. This representation is supposed to be accepted by OCaml as a
   valid floating point literal. *)

let float_representation (f : float) : string =
  match classify_float f with
  | FP_nan ->
    "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      (* Try increasing precisions and validate. *)
      let s = sprintf "%.12g" f in
      if f = float_of_string s then valid_float_lexeme s else
      let s = sprintf "%.15g" f in
      if f = float_of_string s then valid_float_lexeme s else
      sprintf "%.18g" f

(* ------------------------------------------------------------------------- *)

(* A few constants and combinators, used below. *)

let some =
  string "Some"

let none =
  string "None"

let lbracketbar =
  string "[|"

let rbracketbar =
  string "|]"

let seq1 opening separator closing =
  surround_separate 2 0 (opening ^^ closing) opening (separator ^^ break 1) closing

let seq2 opening separator closing =
  surround_separate_map 2 1 (opening ^^ closing) opening (separator ^^ break 1) closing

(* ------------------------------------------------------------------------- *)

(* The following functions are printers for many types of OCaml values. *)

(* There is no protection against cyclic values. *)

type representation =
    document

let tuple =
  seq1 lparen comma rparen

let variant _ cons _ args =
  match args with
  | [] ->
      !^cons
  | _ :: _ ->
      !^cons ^^ tuple args

let record _ fields =
  seq2 lbrace semi rbrace (fun (k, v) -> infix 2 1 equals !^k v) fields

let option f = function
  | None ->
      none
  | Some x ->
      some ^^ tuple [f x]

let list f xs =
  seq2 lbracket semi rbracket f xs

let flowing_list f xs =
  group (lbracket ^^ space ^^ nest 2 (
    flow_map (semi ^^ break 1) f xs
  ) ^^ space ^^ rbracket)

let array f xs =
  seq2 lbracketbar semi rbracketbar f (Array.to_list xs)

let flowing_array f xs =
  group (lbracketbar ^^ space ^^ nest 2 (
    flow_map (semi ^^ break 1) f (Array.to_list xs)
  ) ^^ space ^^ rbracketbar)

let ref f x =
  record "ref" ["contents", f !x]

let float f =
  string (float_representation f)

let int =
  dsprintf "%d"

let int32 =
  dsprintf "%ld"

let int64 =
  dsprintf "%Ld"

let nativeint =
  dsprintf "%nd"

let char =
  dsprintf "%C"

let bool =
  dsprintf "%B"

let string =
  dsprintf "%S"

let unknown tyname _ =
  dsprintf "<abstr:%s>" tyname
