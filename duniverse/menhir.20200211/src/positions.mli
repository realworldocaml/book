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

(* TEMPORARY clean up this over-complicated API? *)

(** Extension of standard library's positions. *)

(** {2 Extended lexing positions} *)

(** Abstract type for pairs of positions in the lexing stream. *)
type t

(** Decoration of a value with a position. *)
type 'a located =
    {
      value    : 'a;
      position : t;
    }

(** [value dv] returns the raw value that underlies the
    decorated value [dv]. *)
val value: 'a located -> 'a

(** [position dv] returns the position that decorates the
    decorated value [dv]. *)
val position: 'a located -> t

(** [decompose dv] returns a pair of the value and position. *)
val decompose: 'a located -> 'a * t

(** [with_pos p v] decorates [v] with a position [p]. *)
val with_pos : t -> 'a -> 'a located
val with_cpos: Lexing.lexbuf -> 'a -> 'a located
val with_loc : (Lexing.position * Lexing.position) -> 'a -> 'a located

val unknown_pos : 'a -> 'a located

(** [map f v] extends the decoration from [v] to [f v]. *)
val map: ('a -> 'b) -> 'a located -> 'b located
val pmap: (t -> 'a -> 'b) -> 'a located -> 'b located

(** [iter f dv] applies [f] to the value inside [dv]. *)
val iter: ('a -> unit) -> 'a located -> unit

(** [mapd f v] extends the decoration from [v] to both members of the pair [f v]. *)
val mapd: ('a -> 'b1 * 'b2) -> 'a located -> 'b1 located * 'b2 located

(** This value is used when an object does not come from
    a particular input location. *)
val dummy: t

(** {2 Accessors} *)

(** [column p] returns the number of characters from the
    beginning of the line of the Lexing.position [p]. *)
val column : Lexing.position -> int

(** [column p] returns the line number of to the Lexing.position [p]. *)
val line : Lexing.position -> int

(** [characters p1 p2] returns the character interval
    between [p1] and [p2] assuming they are located in the same
    line.
*)
val characters : Lexing.position -> Lexing.position -> int * int

val start_of_position: t -> Lexing.position

val end_of_position: t -> Lexing.position

val filename_of_position: t -> string

(** {2 Position handling} *)

(** [join p1 p2] returns a position that starts where [p1]
    starts and stops where [p2] stops. *)
val join : t -> t -> t

val import : Lexing.position * Lexing.position -> t

val ljoinf : ('a -> t) -> 'a list -> t

val joinf : ('a -> t) -> 'a -> 'a -> t

val join_located : 'a located -> 'b located -> ('a -> 'b -> 'c) -> 'c located

val join_located_list :
  ('a located) list -> ('a list -> 'b list) -> ('b list) located


(** [string_of_lex_pos p] returns a string representation for
    the lexing position [p]. *)
val string_of_lex_pos : Lexing.position -> string

(** [string_of_pos p] returns the standard (Emacs-like) representation
    of the position [p]. *)
val string_of_pos : t -> string

(** [pos_or_undef po] is the identity function except if po = None,
    in that case, it returns [undefined_position]. *)
val pos_or_undef : t option -> t

(** {2 Interaction with the lexer runtime} *)

(** [cpos lexbuf] returns the current position of the lexer. *)
val cpos : Lexing.lexbuf -> t

(** [string_of_cpos p] returns a string representation of
    the lexer's current position. *)
val string_of_cpos : Lexing.lexbuf -> string

(* The functions that print error messages and warnings require a list of
   positions. The following auxiliary functions help build such lists. *)

type positions =
    t list

val one: Lexing.position -> positions

val lexbuf: Lexing.lexbuf -> positions

(* Low-level printing function, for debugging. *)
val print: Lexing.position -> unit
