(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* The functor [Make] transforms an abstract syntax tree for the grammar into a
   rich internal representation of the grammar. *)

(* The reason why this is now a functor, and the reason why its verbosity can
   be controlled, is that we may wish to invoke it several times, e.g. on the
   grammar before %inlining, and on the grammar after %inlining. 2015/11/10 *)

module Make (G : sig

  (* An abstract syntax tree for the grammar. *)
  val grammar: BasicSyntax.grammar

  (* This flag indicates whether it is OK to produce warnings, verbose
     information, etc., when this functor is invoked. If it is set to
     [false], then only serious errors can be signaled. *)
  val verbose: bool

end) () : sig

(* ------------------------------------------------------------------------ *)
(* Nonterminals. *)

module Nonterminal : sig

  (* The type of nonterminals. *)

  type t

  (* Comparison. *)

  val compare: t -> t -> int

  (* The number of nonterminals. This includes the extra nonterminals
     that are internally generated for the grammar's entry points. *)

  val n: int

  (* [lookup] maps an identifier to a nonterminal, or raises [Not_found]. *)

  val lookup : string -> t

  (* Nonterminals can be converted to integers. This feature is
     exploited in the table-based back-end. *)

  val n2i: t -> int

  (* This produces a string representation of a nonterminal. It should
     in principle never be applied to one of the internally generated
     nonterminals, as we do not wish users to become aware of the
     existence of these extra nonterminals. However, we do sometimes
     violate this rule when it is difficult to do otherwise.

     The Boolean parameter tells whether the string representation
     should be normalized, that is, whether parentheses and commas
     should be eliminated. This is necessary if the string is intended
     for use as a valid nonterminal name or as a valid OCaml
     identifier. *)

  val print: bool -> t -> string

  (* This is the OCaml type associated with a nonterminal
     symbol. It is known only if a %type declaration was provided.
     This function is not applicable to the internally generated
     nonterminals. *)

  val ocamltype: t -> Stretch.ocamltype option

  (* A start symbol always has a type. This allows us to define
     a simplified version of [ocamltype] for start symbols. *)

  val ocamltype_of_start_symbol: t -> Stretch.ocamltype

  (* Creation of a table indexed by nonterminals. *)

  val init: (t -> 'a) -> 'a array

  (* Iteration over nonterminals. The order in which elements are
     examined, and the order of [map]'s output list, correspond to the
     numeric indices produced by [n2i] above. *)

  val iter: (t -> unit) -> unit
  val fold: (t -> 'a -> 'a) -> 'a -> 'a
  val map: (t -> 'a) -> 'a list

  (* Iteration over all nonterminals, except the start nonterminals. *)

  val iterx: (t -> unit) -> unit
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (* Tabulation of a function over nonterminals. *)

  val tabulate: (t -> 'a) -> (t -> 'a)

  (* [positions nt] is a list of the positions associated with the
     definition of [nt]. There can be more than one position because
     definitions can be split over multiple files. *)

  val positions: t -> Positions.t list

  (* This tells whether a non-terminal symbol is one of the start
     symbols. *)

  val is_start: t -> bool

  (* [attributes nt] is the list of attributes attached with the nonterminal
     symbol [nt]. *)

  val attributes: t -> Syntax.attribute list

end

(* ------------------------------------------------------------------------ *)
(* Sets of nonterminals. *)

module NonterminalMap : GMap.S with type key = Nonterminal.t

module NonterminalSet = NonterminalMap.Domain

(* ------------------------------------------------------------------------ *)
(* Terminals. *)

module Terminal : sig

  (* The type of terminals. *)

  type t

  (* The number of terminals. This includes the two pseudo-tokens
     [#] and [error]. *)

  val n: int

  (* Comparison. *)

  val equal: t -> t -> bool
  val compare: t -> t -> int

  (* [lookup] maps an identifier to a terminal, or raises [Not_found]. *)

  val lookup : string -> t

  (* Terminals can be converted to integers. This feature is exploited in the
     table-based back-end and in [LRijkstra]. The reverse conversion, [i2t],
     is unsafe and should not be used. [LRijkstra] uses it :-) *)

  val t2i: t -> int
  val i2t: int -> t (* unsafe! *)

  (* This produces a string representation of a terminal. *)

  val print: t -> string

  (* This is the OCaml type associated with a terminal
     symbol. It is known only if the %token declaration was
     accompanied with a type. *)

  val ocamltype: t -> Stretch.ocamltype option

  (* These are the two pseudo-tokens [#] and [error]. The former is
     used to denote the end of the token stream. The latter is
     accessible to the user and is used for handling errors. *)

  val sharp: t
  val error: t

  (* This is the programmer-defined [EOF] token, if there is one. It
     is recognized based solely on its name, which is fragile, but
     this behavior is documented. This token is assumed to represent
     [ocamllex]'s [eof] pattern. It is used only by the reference
     interpreter, and in a rather non-essential way. *)

  val eof: t option

  (* A terminal symbol is pseudo if it is [#] or [error]. It is real
     otherwise. *)

  val pseudo: t -> bool
  val real: t -> bool

  (* [non_error] returns [true] if its argument is not the [error] token. *)

  val non_error: t -> bool

  (* Creation of a table indexed by terminals. *)

  val init: (t -> 'a) -> 'a array

  (* Iteration over terminals. The order in which elements are
     examined, and the order of [map]'s output list, correspond to the
     numeric indices produced by [t2i] above. *)

  val iter: (t -> unit) -> unit
  val fold: (t -> 'a -> 'a) -> 'a -> 'a
  val map: (t -> 'a) -> 'a list

  (* Iteration over all terminals except [#]. *)

  val foldx: (t -> 'a -> 'a) -> 'a -> 'a
  val mapx: (t -> 'a) -> 'a list

  (* [iter_real] offers iteration over all real terminals. *)

  val iter_real: (t -> unit) -> unit

  (* [attributes t] is the list of attributes attached with the terminal
     symbol [t]. *)

  val attributes: t -> Syntax.attribute list

  (* The sub-module [Word] offers an implementation of words (that is,
     sequences) of terminal symbols. It is used by [LRijkstra]. We
     make it a functor, because it has internal state (a hash table)
     and a side effect (failure if there are more than 256 terminal
     symbols). *)

  (* The type [word] should be treated, as much as possible, as an
     abstract type. In fact, for efficiency reasons, we represent a
     word as a unique integer codes, and we allocate these integer
     codes sequentially, from 0 upwards. The conversion from [int]
     to [word] is of course unsafe and should be used wisely. *)

  module Word (X : sig end) : sig

    type word = int
    val epsilon: word
    val singleton: t -> word
    val append: word -> word -> word
    val length: word -> int
    (* [first w z] returns the first symbol of the word [w.z]. *)
    val first: word -> t -> t
    val elements: word -> t list
    val print: word -> string
    (* [verbose()] prints statistics about the use of the internal
       hash-consing table so far. *)
    val verbose: unit -> unit
    (* Lexicographic ordering. *)
    val compare: word -> word -> int
  end

end

(* ------------------------------------------------------------------------ *)
(* Sets and maps over terminals. *)

module TerminalSet : sig

  (* All of the operations documented in [GSet] are available. *)

  include GSet.S with type element = Terminal.t

  (* This offers a string representation of a set of terminals. The
     symbols are simply listed one after the other and separated with
     spaces. *)

  val print: t -> string

  (* This is the set of all terminal symbols except the pseudo-tokens
     [#] and [error]. *)

  val universe: t

end

(* All of the operations documented in [GMap] are available. *)

module TerminalMap : GMap.S with type key = Terminal.t

(* ------------------------------------------------------------------------ *)
(* Symbols. *)

module Symbol : sig

  (* A symbol is either a nonterminal or a terminal. *)

  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  val is_terminal: t -> bool

  (* [lookup] maps an identifier to a symbol, or raises [Not_found]. *)

  val lookup : string -> t

  (* Comparison. *)

  val equal: t -> t -> bool
  val lequal: t list -> t list -> bool

  (* [non_error] returns [true] if its argument is not the [error] token. *)

  val non_error: t -> bool

  (* These produce a string representation of a symbol, of a list of
     symbols, or of an array of symbols. The symbols are simply listed
     one after the other and separated with spaces. [printao] prints
     an array of symbols, starting at a particular offset. [printaod]
     is analogous, but can also print a single dot at a particular
     position between two symbols. *)

  val print: t -> string
  val printl: t list -> string
  val printa: t array -> string
  val printao: int -> t array -> string
  val printaod: int -> int -> t array -> string

end

(* ------------------------------------------------------------------------ *)
(* Sets and maps over symbols. *)

(* All of the operations documented in [Set] are available. *)

module SymbolSet : Set.S with type elt = Symbol.t

module SymbolMap : sig

  (* All of the operations documented in [Map] are available. *)

  include Map.S with type key = Symbol.t

  (* [domain m] is the domain of the map [m], that is, the list of keys
     for which an entry exists in the map [m]. *)

  val domain: 'a t -> key list

  (* [init f xs] creates a map whose keys are the elements [x] found in the
     list [xs] and the datum associated with [x] is [f x]. *)

  val init: (key -> 'a) -> key list -> 'a t

  (* This returns [true] if and only if all of the symbols in
     the domain of the map at hand are nonterminals. *)

  val purelynonterminal: 'a t -> bool

end

(* ------------------------------------------------------------------------ *)
(* Productions. *)

module Production : sig

  (* This is the type of productions. This includes user-defined
     productions as well as the internally generated productions
     associated with the start symbols. *)

  type index

  (* Comparison. *)

  val compare: index -> index -> int

  (* Productions can be converted to integers and back. This is unsafe
     and should be avoided as much as possible. This feature is
     exploited, for efficiency, in the encoding of items. *)

  val p2i: index -> int
  val i2p: int -> index

  (* The number of productions. *)

  val n: int

  (* These map a production index to the production's definition, that
     is, a nonterminal (the left-hand side) and an array of symbols
     (the right-hand side). *)

  val def: index -> Nonterminal.t * Symbol.t array
  val nt: index -> Nonterminal.t
  val rhs: index -> Symbol.t array
  val length: index -> int

  (* This maps a production index to an array of the identifiers that
     should be used for naming the semantic values of the symbols in
     the right-hand side. *)

  val identifiers: index -> Syntax.identifier array

  (* This maps a production index to the production's semantic action.
     This function is not applicable to a start production. *)

  val action: index -> Syntax.action

  (* [positions prod] is a list of the positions associated with
     production [prod]. This is usually a singleton list, but there
     can be more than one position for start productions when the
     definition of the corresponding start symbol is split over
     multiple files. *)

  val positions: index -> Positions.t list

  (* [lhs_attributes prod] returns the attributes attached with the
     head symbol of the production [prod]. It is equivalent to
     [Nonterminal.attributes (nt prod)]. [rhs_attributes prod] returns
     an array of the attributes attached with each element in the
     right-hand side of the production [prod]. *)

  val lhs_attributes: index -> Syntax.attributes
  val rhs_attributes: index -> Syntax.attributes array

  (* Creation of a table indexed by productions. *)

  val init: (index -> 'a) -> 'a array

  (* Iteration over all productions. The order in which elements
     are examined, and the order of [map]'s output list, correspond
     to the numeric indices produced by [p2i] above. *)

  val iter: (index -> unit) -> unit
  val fold: (index -> 'a -> 'a) -> 'a -> 'a
  val map: (index -> 'a) -> 'a list
  val amap: (index -> 'a) -> 'a array

  (* Iteration over all productions, except the start productions. *)

  val iterx: (index -> unit) -> unit
  val foldx: (index -> 'a -> 'a) -> 'a -> 'a
  val mapx: (index -> 'a) -> 'a list

  (* This maps a (user) non-terminal start symbol to the corresponding
     start production. *)

  val startsymbol2startprod: Nonterminal.t -> index

  (* Iteration over the productions associated with a specific
     nonterminal. *)

  val iternt: Nonterminal.t -> (index -> unit) -> unit
  val foldnt: Nonterminal.t -> 'a -> (index -> 'a -> 'a) -> 'a

  (* This allows determining whether a production is a start
     production. If it is a start production, the start symbol that it
     is associated with is returned. If it is a regular production,
     nothing is returned. *)

  val classify: index -> Nonterminal.t option

  (* [is_start] is easier to use than [classify] when the start symbol
     is not needed. *)

  val is_start: index -> bool

  (* The integer [start] is published so as to allow the table back-end
     to produce code for [is_start]. It should not be used otherwise. *)

  val start: int

  (* This produces a string representation of a production. It should
     never be applied to a start production, as we do not wish users
     to become aware of the existence of these extra productions. *)

  val print: index -> string

  (* Tabulation of a function over productions. *)

  val tabulate: (index -> 'a) -> (index -> 'a)

  (* Sum of an integer function over productions. *)

  val sum: (index -> int) -> int

end

(* ------------------------------------------------------------------------ *)
(* Maps over productions. *)

module ProductionMap : sig

  include GMap.S with type key = Production.index

  (* Iteration over the start productions only. *)

  val start: (Production.index -> 'a) -> 'a t

end

(* ------------------------------------------------------------------------ *)
(* This flag tells whether the [error] token appears in at least one
   production. *)

val grammar_uses_error_token: bool

(* ------------------------------------------------------------------------ *)
(* Analysis of the grammar. *)

module Analysis : sig

  (* [nullable nt] is the NULLABLE flag of the non-terminal symbol [nt].
     That is, it is true if and only if this symbol produces the empty
     word [epsilon]. *)

  val nullable: Nonterminal.t -> bool
  val nullable_symbol: Symbol.t -> bool

  (* [first nt] is the FIRST set of the non-terminal symbol [nt]. *)

  val first: Nonterminal.t -> TerminalSet.t
  val first_symbol: Symbol.t -> TerminalSet.t

  (* [nullable_first_prod prod i] considers the suffix of the production
     [prod] defined by offset [i]. It returns its NULLABLE flag as well
     as its FIRST set. The offset [i] must be contained between [0] and
     [n], inclusive, where [n] is the length of production [prod]. *)

  val nullable_first_prod: Production.index -> int -> bool * TerminalSet.t

  (* [first_prod_lookahead prod i t] computes [FIRST(alpha.t)], where [alpha]
     is the suffix of the production defined by offset [i], and [t] is a
     terminal symbol. The offset [i] must be contained between [0] and [n],
     inclusive, where [n] is the length of production [prod]. *)

  val first_prod_lookahead: Production.index -> int -> Terminal.t -> TerminalSet.t

  (* [explain_first_rhs tok rhs i] explains why the token [tok] appears
     in the FIRST set for the string of symbols found at offset [i] in
     the array [rhs]. *)

  val explain_first_rhs: Terminal.t -> Symbol.t array -> int -> string

  (* [follow nt] is the FOLLOW set of the non-terminal symbol [nt], that
     is, the set of terminal symbols that could follow an expansion of
     [nt] in a valid sentence. *)

  val follow: Nonterminal.t -> TerminalSet.t

  (* [attributes] are the attributes attached with the grammar. *)

  val attributes: Syntax.attributes

end

(* ------------------------------------------------------------------------ *)
(* Conflict resolution via precedences. *)

module Precedence : sig

  (* Shift/reduce conflicts require making a choice between shifting a
     token and reducing a production. How these choices are made is of
     no concern to the back-end, but here is a rough explanation.

     Shifting is preferred when the token has higher precedence than
     the production, or they have same precedence and the token is
     right-associative.

     Reducing is preferred when the token has lower precedence than
     the production, or they have same precedence and the token is
     left-associative.

     Neither is allowed when the token and the production have same
     precedence and the token is non-associative.

     No preference is explicitly specified when the token or the
     production has undefined precedence. In that case, the default
     choice is to prefer shifting, but a conflict will be reported. *)

  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  val shift_reduce: Terminal.t -> Production.index -> choice

  (* Reduce/reduce conflicts require making a choice between reducing
     two distinct productions. This is done by exploiting a partial
     order on productions.

     For compatibility with ocamlyacc, this order should be total and
     should correspond to textual order when the two productions
     originate in the same source file. When they originate in
     different source files, the two productions should be
     incomparable. *)

  val reduce_reduce: Production.index -> Production.index -> Production.index option

end

(* ------------------------------------------------------------------------ *)
(* [%on_error_reduce] declarations. *)

module OnErrorReduce : sig

  (* [reduce prod] tells whether the left-hand side of [prod] (a nonterminal
     symbol) appears in an [%on_error_reduce] declaration. *)

  val reduce: Production.index -> bool

  (* [iter f] applies the function [f] in turn, in an arbitrary order, to
     every nonterminal symbol that appears in an [%on_error_reduce]
     declaration. *)

  val iter: (Nonterminal.t -> unit) -> unit

  (* When two productions could be reduced, in a single state, due to
     [%on_error_reduce] declarations, these productions can be compared, using
     [preferable], to test if one of them takes precedence over the other.
     This is a partial order; two productions may be incomparable. *)

  val preferable: Production.index -> Production.index -> bool

end

(* ------------------------------------------------------------------------ *)
(* Diagnostics. *)

(* This function prints warnings about useless precedence declarations for
   terminal symbols (%left, %right, %nonassoc) and productions (%prec). It
   should be invoked after only the automaton has been constructed. *)

val diagnostics: unit -> unit

(* ------------------------------------------------------------------------ *)

end (* module Make *)
