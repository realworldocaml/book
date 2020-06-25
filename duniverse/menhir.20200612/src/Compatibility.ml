(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

module Bytes = struct

  include Bytes

  let escaped s =
    let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
         | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | ' ' .. '~' -> 1
         | _ -> 4)
    done;
    if !n = length s then copy s else begin
      let s' = create !n in
      n := 0;
      for i = 0 to length s - 1 do
        begin match unsafe_get s i with
        | ('\"' | '\\') as c ->
            unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
        | '\n' ->
            unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
        | '\t' ->
            unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
        | '\r' ->
            unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
        | '\b' ->
            unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
        | (' ' .. '~') as c -> unsafe_set s' !n c
        | c ->
            let a = Char.code c in
            unsafe_set s' !n '\\';
            incr n;
            unsafe_set s' !n (Char.chr (48 + a / 100));
            incr n;
            unsafe_set s' !n (Char.chr (48 + (a / 10) mod 10));
            incr n;
            unsafe_set s' !n (Char.chr (48 + a mod 10));
        end;
        incr n
      done;
      s'
    end

end

module String = struct

  open String

  let escaped s =
    let rec escape_if_needed s n i =
      if i >= n then s else
        match unsafe_get s i with
        | '\"' | '\\' | '\000'..'\031' | '\127'.. '\255' ->
            Bytes.unsafe_to_string (Bytes.escaped (Bytes.unsafe_of_string s))
        | _ -> escape_if_needed s n (i+1)
    in
    escape_if_needed s (length s) 0

end
