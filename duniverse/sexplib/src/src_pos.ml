(* for lexing positions:
   - lnum starts from 1
   - cnum starts from 0
   - pos_bol is the position of the first character of the line

   for absolute pos:
   - row starts from 1
   - col starts from 1

   for relative pos, obviously 0 means same position
*)

(* operations that make sense for both absolute and relative positions *)
module Pos = struct
  type t = { row : int; col : int }
  let sexp_of_t {row; col} = Type.Atom (Printf.sprintf "%d:%d" row col)

  let add t1 t2 = {
    row = t1.row + t2.row;
    col = t1.col + t2.col;
  }

  let sub t1 t2 = {
    row = t1.row - t2.row;
    col = t1.col - t2.col;
  }

  let compare {row = r1; col = c1} {row = r2; col = c2} =
    if r1 = r2 then c1 - c2 else r1 - r2

  let geq t1 t2 = compare t1 t2 >= 0
end

module Relative = struct
  include Pos
  let zero = {row = 0; col = 0}
end

module Absolute = struct
  include Pos

  let origin = {row = 1; col = 1}

  let of_lexing {Lexing.pos_lnum; pos_cnum; pos_bol; pos_fname = _ } = {
    row = pos_lnum;
    col = pos_cnum - pos_bol + 1;
  }

  let diff = sub
end

