type command = {
  name : string;
  info : string option;
}

and item = [
| `Command of command
| `Data of string
] [@@deriving sexp]

type t = item list [@@deriving sexp]

val to_string : t -> string

(******************************************************************************)
(** {Constructors} *)
(******************************************************************************)
val input : string -> item
val newpage : item
val part : string -> item
