(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

let compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  = fun f g x -> f (g (x))

let ( @* ) = compose

let default d = function Some x -> x | None -> d

let string_split char str =
  let rec aux acc pos =
    let i =
      try Some (String.rindex_from str pos char)
      with Not_found | Invalid_argument _ -> None
    in
    match i with
    | Some i -> aux (String.sub str (i + 1) (pos - i) :: acc) (pred i)
    | None -> String.sub str 0 (pos + 1) :: acc
  in
  aux [] (String.length str - 1)

let string_split_chars chars str =
  let len = String.length str in
  let rec split pos =
    let rec lookup i =
      if i >= len then raise Not_found
      else if String.contains chars str.[i] then i
      else lookup (succ i)
    in
    try
      let i = lookup pos in
      if i > pos then String.sub str pos (i - pos) :: split (succ i)
      else split (succ i)
    with Not_found | Invalid_argument _ ->
        [ String.sub str pos (len - pos) ]
  in
  split 0

let is_prefix pfx str =
  let pfxlen = String.length pfx in
  let rec check i = i >= pfxlen || pfx.[i] = str.[i] && check (i+1) in
  String.length str >= pfxlen && check 0

let ends_with_escape s =
  let rec aux n = n >= 0 && s.[n] = '\\' && not (aux (n-1))
  in aux (String.length s - 1)

let count_leading_spaces s =
  let rec aux i =
    if i >= String.length s || s.[i] <> ' ' then i
    else aux (i+1)
  in
  aux 0

let shorten_string n s =
  match string_split '\n' s with
  | [] -> ""
  | [s] ->
      if String.length s <= n then s
      else
        let n1 = (n - 3) / 2 in
        let n2 = n - 3 - n1 in
        String.sub s 0 n1
        ^ "..."
        ^ String.sub s (String.length s - n2) n2
  | s1::r1::r ->
      let s2 =
        let rec last x = function x::r -> last x r | [] -> x in
        last r1 r
      in
      let l1 = String.length s1 and l2 = String.length s2 in
      let n1 = min l1 (max ((n-3) / 2)  (n-3 - l2)) in
      let n2 = min l2 (n - 3 - n1) in
      String.sub s1 0 n1
      ^ "..."
      ^ String.sub s2 (l2 - n2) n2
