(** Supported versions of the OCaml AST *)

type t

val all : t list

val to_string : t -> string
(** Return a string such as "4.02" *)

val to_int : t -> int
(** Return an integer such as [402] *)

val of_string : string -> t option
(** Parse a string as reported by [ocamlc -version] *)
