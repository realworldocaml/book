open! Base
include State_intf

type t =
  | Whitespace
  | Error
  | After_cr
  | Unquoted_string of Unquoted_string.t
  | Line_comment
  | After_hash
  | Quoted_string of Quoted_string.t
  | Block_comment of Block_comment.t
[@@deriving enumerate, compare, sexp_of]

include Comparator.Make (struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end)

let to_int t =
  let rec loop i t l =
    match l with
    | [] -> assert false
    | x :: l -> if [%compare.equal: t] t x then i else loop (i + 1) t l
  in
  loop 0 t all
;;

let of_int i = List.nth_exn all i
let count = List.length all
let initial = Whitespace

(* This is assumed in parser_automaton_internal.ml *)

let old_parser_approx_cont_state = function
  | Whitespace -> "Parsing_toplevel_whitespace"
  | After_cr -> "Parsing_nested_whitespace"
  | Unquoted_string _ | Quoted_string _ -> "Parsing_atom"
  | After_hash -> "Parsing_atom"
  | Block_comment _ -> "Parsing_block_comment"
  | Line_comment -> "Parsing_toplevel_whitespace"
  (* This cannot happen with the old parser so the result is a dummy value *)
  | Error -> "Parsing_toplevel_whitespace"
;;
