(* This module needs cleaning up. It is supposed to automatically
   produce a syntax error message, based on the current state and
   stack. *)

module Make
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (User : sig

    (* In order to submit artificial tokens to the parser, we need a function
       that converts a terminal symbol to a token. Unfortunately, we cannot
       (in general) auto-generate this code, because it requires making up
       semantic values of arbitrary OCaml types. *)

    val terminal2token: _ I.terminal -> I.token

  end)
: sig

  open I

  (* An explanation is a description of what the parser has recognized in the
     recent past and what it expects next. More precisely, an explanation is
     an LR(0) item, enriched with positions. Indeed, the past (the first half
     of the item's right-hand side, up to the bullet) corresponds to a part of
     the input that has been read already, so it can be annotated with
     positions. *)

  type explanation

  (* The LR(0) item. *)

  val item: explanation -> item

  (* The past. This is a non-empty sequence of (terminal and non-terminal)
     symbols, each of which corresponds to a range of the input file. These
     symbols correspond to the first half (up to the bullet) of the item's
     right-hand side. In short, they represent what (we think) we have
     recognized in the recent past. *)

  (* It is worth noting that, when an error occurs, we produce multiple
     explanations, which may have different pasts. Indeed, not only may
     these pasts have different lengths (one may be a suffix of another),
     but two pasts can in fact be incomparable. Indeed, different choices
     of the lookahead token may cause different reductions, hence different
     interpretations of what has been read in the past. *)

  val past: explanation -> (xsymbol * Lexing.position * Lexing.position) list

  (* The future. This is a non-empty sequence of (terminal and non-terminal)
     symbols. These symbols correspond to the second half (after the bullet)
     of the item's right-hand side. In short, they represent what we expect
     to recognize in the future, if this item is a good prediction. *)

  (* This information can be computed from [item]. This function is provided
     only for convenience. *)

  val future: explanation -> xsymbol list

  (* A goal. This is a non-terminal symbol. It is the item's left-hand side.
     In short, it represents the reduction that we will be able to perform if
     we successfully recognize this future. *)

  (* This information can be computed from [item]. This function is provided
     only for convenience. *)

  val goal: explanation -> xsymbol

  (* TEMPORARY *)

  (* We build lists of explanations. These explanations may originate in
     distinct LR(1) states. They may have different pasts, because  *)

  exception Error of (Lexing.position * Lexing.position) * explanation list

  (* TEMPORARY *)

  val entry: 'a I.checkpoint -> (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a

end
