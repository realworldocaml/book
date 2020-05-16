(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

val cs_lex_compare : Cstruct.t -> Cstruct.t -> int

type t

val immediate : int -> (int -> Cstruct.t -> unit) -> t

val len    : t -> int
val empty  : t
val (<+>)  : t -> t -> t
val append : t -> t -> t
val concat : t list -> t

val of_list    : int list -> t
val of_string  : string -> t
val of_cstruct : Cstruct.t -> t
val of_byte    : int -> t

val to_cstruct : t -> Cstruct.t
val to_writer  : t -> int * (Cstruct.t -> unit)

