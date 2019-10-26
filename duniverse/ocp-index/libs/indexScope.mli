(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

type t

(** The empty scope *)
val empty: t

(** The type of gathered information within a scope *)
type env = Alias of string * string list | Open of string list

(** Compute the environment from an input channel, at an optional position or at
    EOF *)
val read: ?line:int -> ?column:int -> in_channel -> t

(** Compute the environment from a string *)
val read_string: string -> t

(** Lower-level function for processing on the environment at every token.
    The position is given in the form [(line, column, length)] *)
val fold:
  ('a -> t -> Approx_lexer.token -> (int * int * int) -> 'a) -> 'a ->
  ?init:env list -> ?stop:(Lexing.position -> bool) -> in_channel
  -> 'a

(** The same from a string *)
val fold_string:
  ('a -> t -> Approx_lexer.token -> (int * int * int) -> 'a) -> 'a ->
  ?init:env list -> ?stop:(Lexing.position -> bool) -> string
  -> 'a

(** Returns the [env] declarations in scope [t], in source file order *)
val to_list: t -> env list
