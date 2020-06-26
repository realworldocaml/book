(*
 * Copyright (c) 2015 Trevor Summers Smith <trevorsummerssmith@gmail.com>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Hexadecimal encoding.

    [Hex] defines hexadecimal encodings for {{!char}characters},
    {{!string}strings} and {{!cstruct}Cstruct.t} buffers. *)

type t = [`Hex of string]
(** The type var hexadecimal values. *)

(** {1:char Characters} *)

val of_char: char -> char * char
(** [of_char c] is the the hexadecimal encoding of the character
    [c]. *)

val to_char: char -> char -> char
(** [to_char x y] is the character correspondong to the [xy]
    hexadecimal encoding. *)

(** {1:string Strings} *)

val of_string: ?ignore:char list -> string -> t
(** [of_string s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_string ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)

val to_string: t -> string
(** [to_string t] is the binary string [s] such that [of_string s] is
    [t]. *)

(** {1:byte Bytes} *)

val of_bytes: ?ignore:char list -> bytes -> t
(** [of_bytes s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_bytes ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)

val to_bytes: t -> bytes
(** [to_bytes t] is the binary string [s] such that [of_bytes s] is
    [t]. *)

(** {1:cstruct Cstruct} *)

val of_cstruct: ?ignore:char list -> Cstruct.t -> t
(** [of_cstruct buf] is the hexadecimal representation of the buffer
    [buf]. *)

val to_cstruct: t -> Cstruct.t
(** [to_cstruct t] is the buffer [b] such that [of_cstruct b] is
    [t]. *)

(** {1:Bigstring Bigstring} *)

val of_bigstring: ?ignore:char list -> Cstruct.buffer -> t
(** [of_bigstring buf] is the hexadecimal representation of the buffer
    [buf]. *)

val to_bigstring: t -> Cstruct.buffer
(** [to_bigstring t] is the buffer [b] such that [of_bigstring b] is
    [t]. *)

(** {1 Debugging} *)

val hexdump: ?print_row_numbers:bool -> ?print_chars:bool -> t -> unit
(** [hexdump h] dumps the hex encoding to stdout in the following format:

    {v
       00000000: 6865 6c6c 6f20 776f 726c 6420 6865 6c6c  hello world hell
       00000010: 6f20 776f 726c 640a                      o world.
    v}

    This is the same format as emacs hexl-mode, and is a very similar
    format to hexdump -C. '\t' and '\n' are printed as '.'.in the char
    column.

    [print_row_numbers] and [print_chars] both default to
    [true]. Setting either to [false] does not print the column.
*)

val hexdump_s: ?print_row_numbers:bool -> ?print_chars:bool -> t -> string
(** Same as [hexdump] except returns a string. *)

(** {1 Pretty printing} *)

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
(** [pp fmt t] will output a human-readable hex representation of [t]
    to the formatter [fmt]. *)

val show : t -> string
(** [show t] will return a human-readable hex representation of [t] as
    a string. *)
