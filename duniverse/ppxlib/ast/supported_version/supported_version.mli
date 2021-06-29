(** Supported versions of the OCaml AST *)

type t

val all : t list

(** Return a string such as "4.02" *)
val to_string : t -> string

(** Return an integer such as [402] *)
val to_int : t -> int

(** Parse a string as reported by [ocamlc -version] *)
val of_string : string -> t option
