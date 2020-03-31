open Lexing

type positions =
  position * position

type nothing =
  positions

type optional_dot =
  positions * nothing option

type optional_comma =
  positions * nothing option

type annotations =
  positions * position * position * int * position * optional_dot * optional_comma

type raw_expr =
  | EInt
  | EParen of annotations * nothing * expr * optional_dot * int
  | EBinOp of expr * expr
  | EUnOp of expr

and expr =
  positions * position * int * raw_expr

type main =
  positions * nothing * expr * position

let iter f = function
  | None ->
      ()
  | Some x ->
      f x

module Print = struct

  let position msg pos =
    Printf.printf "%s = %s/%03d,\n"
      msg
      pos.pos_fname
      pos.pos_cnum

  let offset msg ofs =
    Printf.printf "%s = %03d,\n"
      msg
      ofs

  let positions nt (startpos, endpos) =
    Printf.printf "%s: startpos = %s/%03d,\n%s:   endpos = %s/%03d\n"
      nt
      startpos.pos_fname
      startpos.pos_cnum
      nt
      endpos.pos_fname
      endpos.pos_cnum

  let nothing =
    positions "nothing"

  let optional_dot (poss, no) =
    positions "optional_dot" poss;
    iter nothing no

  let optional_comma (poss, no) =
    positions "optional_comma" poss;
    iter nothing no

  let annotations ((poss, pos1, pos2, ofs3, pos4, odot, ocomma) : annotations) =
    positions "annotations" poss;
    position  "annotations:     $endpos($1)" pos1;
    position  "annotations:   $startpos($2)" pos2;
    offset    "annotations:       $startofs" ofs3;
    position  "annotations: $symbolstartpos" pos4;
    optional_dot odot;
    optional_comma ocomma

  let rec raw_expr = function
    | EInt ->
        ()
    | EParen (a, n, e, o, ofs) ->
        annotations a;
        nothing n;
        expr e;
        optional_dot o;
        offset    "eparen: $symbolstartofs" ofs;
    | EBinOp (e1, e2) ->
        expr e1;
        expr e2
    | EUnOp e ->
        expr e

  and expr (poss, pos, ofs, e) =
    positions "expr" poss;
    position "expr: $endpos($0)" pos;
    offset   "expr: $endofs($0)" ofs;
    raw_expr e

  let main (poss, n, e, pos1) =
    positions "main" poss;
    nothing n;
    expr e;
    position "main: $symbolstartpos" pos1

end

