(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module discovers and publishes information about the automaton.

   It determines the shape of the stack when a state is about to be
   entered, when a production is about to be reduced, and when a goto
   transition is about to be taken.

   It also determines which states should be represented (that is,
   need to physically exist on the stack at runtime) and which symbols
   need to keep track of (start or end) positions.

   It also determines which automaton states could have to deal with an
   [error] token.

   The information computed in this module is used in the code back-end,
   in the Coq back-end, and in the automated production of .messages files.
   It is not used in the table back-end. *)

open Grammar

(* ------------------------------------------------------------------------- *)
(* A representation of stack shapes. *)

(**A cell is a representation of a stack cell. *)
type cell

(**[symbol cell] is the symbol associated with the cell [cell]. This
   symbol determines the presence and the type of the semantic value stored
   in this cell. It also determines whether a start position and an end
   position are stored in this cell; see [track_startp] and [track_endp]. *)
val symbol: cell -> Symbol.t

(**[states cell] is the set of the states that might be stored at runtime
   in this cell. The states in this set have the property that either all
   of them are represented, in which case [holds_state] is [true], or none
   of them is represented, in which case [holds_state] is [false]. *)
val states: cell -> Lr1.NodeSet.t

(**[holds_semv cell] tells whether a semantic value is stored in the cell
   [cell]. By convention, if [symbol cell] is a nonterminal symbol, then a
   semantic value is stored. (We do not attempt to detect the situation where
   the semantic value could be omitted because it has type [unit], or the
   situation where it could be omitted because it is never used.) If [symbol
   cell] is a terminal symbol, then a semantic value is stored if and only if
   the [%token] declaration was annotated with a type. *)
val holds_semv: cell -> bool

(**[holds_state cell] tells whether a state is stored in the cell [cell]. *)
val holds_state: cell -> bool

(**[holds_startp cell] tells whether a start position is stored in the cell
   [cell]. It is equivalent to [track_startp (symbol cell)]. *)
val holds_startp: cell -> bool

(**[holds_endp cell] tells whether a start position is stored in the cell
   [cell]. It is equivalent to [track_endp (symbol cell)]. *)
val holds_endp: cell -> bool

(**[present cell] determines whether at least one of the four fields
   [state], [semv], [startp] or [endp] is present in the cell [cell]. *)
val present: cell -> bool

(**[similar] determines whether two stack cells have the same layout in
   memory, that is, the same OCaml type. This is equivalent to comparing
   all fields except [states]. *)
val similar: cell -> cell -> bool

(**A word is a representation of a stack suffix. A word is an immutable
   array of cells, whose right end represents the top of the stack. Thus,
   the index 0 in the array corresponds to the cell that lies deepest in
   the stack. Many of the functions that follow are just copies of the
   functions by the same names in the modules Array or MArray. *)
type word

(**[get w i] is the [i]-th cell in the word [w]. *)
val get: word -> int -> cell

(**[length w] is the length of the word [w]. *)
val length: word -> int

(**[fold_left] performs left-to-right iteration, with an accumulator,
   over a word. *)
val fold_left: ('a -> cell -> 'a) -> 'a -> word -> 'a

(**[append w1 w2] is the concatenation of the words [w1] and [w2]. *)
val append: word -> word -> word

(**[filter p w] is the subsequence of the cells in the word [w] that satisfy
   the predicate [p]. *)
val filter: (cell -> bool) -> word -> word

(**[suffix w k] is the suffix of length [k] of the word [w]. The length of
   [w] must be at least [k]. *)
val suffix: word -> int -> word

(**[push w cell] is the stack [w], onto which the cell [cell] has been
   pushed. *)
val push: word -> cell -> word

(**[pop w] is the stack [w], deprived of its top element. The stack [w]
   must be nonempty. *)
val pop: word -> word

(**[top w] is the top element of the stack [w]. The stack [w] must be
   nonempty. *)
val top: word -> cell

(**[split w k] splits the word [w] into two parts: a lower part of length
   [length w - k] and an upper part of length [k]. The length of [w] must be
   at least [k]. *)
val split: word -> int -> word * word

(**[meet w1 w2] is the meet of the two stacks [w1] and [w2], that is, the
   logical conjunction of the information contained in [w1] and [w2]. If [w1]
   and [w2] contradict one another, then their meet is bottom, and [meet w1
   w2] is [None]. Caveat: all [states] fields are ignored in this computation;
   the [states] fields of the result are not as accurate as they could be, and
   some opportunities to return [None], based on an inspection of the [states]
   fields, are missed. *)
val meet: word -> word -> word option

(**[print w] prints the sequence of symbols associated with the word [w].
   This is used in the code back-end to generate comments in the definition
   of the algebraic data type [state]. *)
val print: word -> string

(**[to_list w] converts the word [w] to a list. *)
val to_list: word -> cell list

(* ------------------------------------------------------------------------- *)
(* Information about the stack. *)

module type STACK = sig

  (**[stack s] is the known suffix of the stack at state [s]. *)
  val stack: Lr1.node -> word

  (**[prodstack prod] is the known suffix of the stack at a state where
     production [prod] can be reduced. In the short invariant, the length of
     this suffix is [Production.length prod]. In the long invariant, its
     length can be greater. If there are no states where [prod] can be
     reduced, then every cell contains an empty set of states. *)
  val prodstack: Production.index -> word

  (**[gotostack nt] is the known suffix of the stack at a state where an
     edge labeled [nt] has just been followed. In the short invariant, the
     length of this suffix is [1]: indeed, it consists of just one cell,
     associated with the symbol [nt]. In the long invariant, its length can
     be greater. *)
  val gotostack: Nonterminal.t -> word

end

(* In the short invariant, the length of the known suffix of the stack is
   the one reported by [StackSymbols.Short()]. *)

module Short : STACK

(* ------------------------------------------------------------------------- *)
(* Information about error handling. *)

type instruction =
  | Die
  | DownTo of word * state

and state =
  | Represented
  | UnRepresented of Lr1.node

(**[rewind s] explains how to rewind the stack when dealing with an
   error in state [s]. It produces an instruction to either die
   (because no state on the stack can handle errors) or pop a suffix
   of the stack. In the latter case, one reaches a state that is
   either represented (its identity is physically stored in the
   bottommost cell that is popped) or unrepresented (its identity is
   statically known). This function is used only in the [legacy]
   error-handling strategy. *)
val rewind: Lr1.node -> instruction

(**[errorpeeker s] tells whether state [s] can potentially peek at an
   error. This is the case if, in state [s], an error token may be on
   the stream. This function is used only in the [legacy]
   error-handling strategy. *)
val errorpeeker: Lr1.node -> bool

(* ------------------------------------------------------------------------- *)
(* Information about which states and positions need to physically
   exist on the stack. *)

(**[represented s] tells whether state [s] must have an explicit
   representation, that is, whether it is pushed onto the stack. *)
val represented: Lr1.node -> bool

(**[track_startp symbol] tells whether a start position must be
   recorded for the symbol [symbol]. *)
val track_startp: Symbol.t -> bool

(**[track_endp symbol] tells whether an end position must be recorded
   for the symbol [symbol]. *)
val track_endp: Symbol.t -> bool

(**[universal symbol] tells whether every represented state has an
   outgoing transition along [symbol]. *)
val universal: Symbol.t -> bool

(* ------------------------------------------------------------------------- *)
(* More information about the stack. *)

(**[Long()] computes a "long invariant" where the known suffix of the stack
   is as long as possible, based on an analysis of the LR(1) automaton. It
   is possibly longer than the suffix proposed in the "short invariant",
   whose length is always the maximum position of the bullet in the items
   of the state at hand. *)
module Long () : STACK

(* ------------------------------------------------------------------------- *)
(* Reachability from the entry states. *)

module Origin : sig

  (**The origin [SingleOrigin nt] indicates that the point of interest is
     reachable only via the start symbol [nt]. The origin [Dead] indicates
     that this point in unreachable. [MultipleOrigins] indicates that this
     point is reachable via several start symbols. *)
  type origin =
    | Dead
    | SingleOrigin of Nonterminal.t
    | MultipleOrigins

  (**[run s] determines via which start symbols the [run] function for state
     [s] is reachable. *)
  val run: Lr1.node -> origin

  (**[reduce prod] determines via which start symbols the [reduce] function
     for production [prod] is reachable. *)
  val reduce: Production.index -> origin

  (**[goto nt] determines via which start symbols the [goto] function for the
     nonterminal symbol [nt] is reachable. *)
  val goto: Nonterminal.t -> origin

end
