open! Import
open! Ppx_sexp_conv_lib
include Sexp_prefix_intf

type t = Sexp.t list * in_sexp

and in_sexp =
  | Hole of Atom_prefix.t option
  | In_list of t
[@@deriving sexp_of]

let create (state : (_, _) Automaton.t) stack =
  let rec of_stack (stack : Automaton.Stack.t) acc =
    match stack with
    | Empty -> acc
    | Open stack -> of_stack stack ([], In_list acc)
    | Sexp (sexp, stack) ->
      let sexps, partial = acc in
      of_stack stack (sexp :: sexps, partial)
  in
  match state.mode with
  | Eager _ | Single -> failwith "The automaton must be in [Many] mode."
  | Many ->
    (match Atom_prefix.create state with
     | Awkward_position -> None
     | Whitespace -> Some (of_stack stack ([], Hole None))
     | Some atom -> Some (of_stack stack ([], Hole (Some atom))))
;;

let of_substring s ~pos ~len =
  let state, stack = Automaton.of_substring Many Sexp_with_positions s ~pos ~len in
  create state stack
;;

let get_a_signifier (t : t) ~parser_input =
  let buf = Buffer.create 128 in
  let rec add_in_sexp = function
    | Hole None -> ()
    | Hole (Some atom_prefix) ->
      Buffer.add_string buf (Atom_prefix.get_signifier atom_prefix ~parser_input)
    | In_list t ->
      Buffer.add_string buf "(";
      add_t t
  and add_t (sexps, in_sexp) =
    List.iter sexps ~f:(fun sexp ->
      Buffer.add_string buf (Sexp.to_string sexp);
      Buffer.add_char buf ' ');
    add_in_sexp in_sexp
  in
  add_t t;
  Buffer.contents buf
;;
