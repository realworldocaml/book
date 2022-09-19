open! Import

type t =
  | Parsing_toplevel_whitespace
  | Parsing_nested_whitespace
  | Parsing_atom
  | Parsing_list
  | Parsing_sexp_comment
  | Parsing_block_comment
[@@deriving_inline sexp_of]

let sexp_of_t =
  (function
    | Parsing_toplevel_whitespace -> Sexplib0.Sexp.Atom "Parsing_toplevel_whitespace"
    | Parsing_nested_whitespace -> Sexplib0.Sexp.Atom "Parsing_nested_whitespace"
    | Parsing_atom -> Sexplib0.Sexp.Atom "Parsing_atom"
    | Parsing_list -> Sexplib0.Sexp.Atom "Parsing_list"
    | Parsing_sexp_comment -> Sexplib0.Sexp.Atom "Parsing_sexp_comment"
    | Parsing_block_comment -> Sexplib0.Sexp.Atom "Parsing_block_comment"
                               : t -> Sexplib0.Sexp.t)
;;

[@@@end]

let to_string t =
  match sexp_of_t t with
  | Atom s -> s
  | List _ -> failwith "BUG: [sexp_of_t] returned a [List _]"
;;
