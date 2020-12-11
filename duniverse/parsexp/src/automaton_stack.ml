open! Import
include Automaton_stack_intf

module For_cst = struct
  type t =
    | Empty
    | T_or_comment of Cst.t_or_comment * t
    | Open of Positions.pos * t
    | In_sexp_comment of
        { hash_semi_pos : Positions.pos
        ; rev_comments : Cst.comment list
        ; stack : t
        }

  let empty = Empty

  let get_many =
    let rec loop acc = function
      | Empty -> acc
      | T_or_comment (t, stack) -> loop (t :: acc) stack
      | Open _ | In_sexp_comment _ -> failwith "Automaton_stack.For_cst.get_many"
    in
    fun stack -> loop [] stack
  ;;
end

module Just_positions = struct
  type t = unit

  let empty = ()
end

type t =
  | Empty
  | Open of t
  | Sexp of Sexp.t * t

let empty = Empty

let get_single = function
  | Sexp (sexp, Empty) -> sexp
  | _ -> failwith "Automaton_stack.get_single"
;;

let get_many =
  let rec loop acc = function
    | Empty -> acc
    | Open _ -> failwith "Automaton_stack.get_many"
    | Sexp (sexp, stack) -> loop (sexp :: acc) stack
  in
  fun stack -> loop [] stack
;;
