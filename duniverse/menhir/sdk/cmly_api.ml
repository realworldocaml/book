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

(* The following signatures describe the API offered by the functor
   [Cfmly_read.Read]. This functor reads in a .cmly file and gives
   access to the description of the grammar and automaton contained
   in this file. *)

(* This API is currently entirely self-contained, except for a reference
   to the module [Keyword], which is also part of [MenhirSdk]. *)

(* The module type [INDEXED] describes a type [t] whose elements are
   in a bijection with an integer interval of the form [0..count). *)

module type INDEXED = sig
  type t
  val count : int
  val of_int : int -> t
  val to_int : t -> int
  val iter : (t -> unit) -> unit
  val fold : (t -> 'a -> 'a) -> 'a -> 'a
  val tabulate : (t -> 'a) -> t -> 'a
end

(* The module type [GRAMMAR] describes the grammar and automaton. *)

module type GRAMMAR = sig

  type terminal    = private int
  type nonterminal = private int
  type production  = private int
  type lr0         = private int
  type lr1         = private int
  type item        = production * int
  type ocamltype   = string
  type ocamlexpr   = string

  module Range : sig
    type t
    val startp: t -> Lexing.position
    val endp: t -> Lexing.position
  end

  module Attribute : sig
    type t
    val label        : t -> string
    val has_label    : string -> t -> bool
    val payload      : t -> string
    val position     : t -> Range.t
  end

  module Grammar : sig
    val basename     : string
    val preludes     : string list
    val postludes    : string list
    val parameters   : string list
    val entry_points : (nonterminal * production * lr1) list
    val attributes   : Attribute.t list
  end

  module Terminal : sig
    include INDEXED with type t = terminal
    val name         : t -> string
    val kind         : t -> [`REGULAR | `ERROR | `EOF | `PSEUDO]
    val typ          : t -> ocamltype option
    val attributes   : t -> Attribute.t list
  end

  module Nonterminal : sig
    include INDEXED with type t = nonterminal
    val name         : t -> string
    val mangled_name : t -> string
    val kind         : t -> [`REGULAR | `START]
    val typ          : t -> ocamltype option
    val positions    : t -> Range.t list
    val nullable     : t -> bool
    val first        : t -> terminal list
    val attributes   : t -> Attribute.t list
  end

  type symbol =
    | T of terminal
    | N of nonterminal

  val symbol_name : ?mangled:bool -> symbol -> string

  type identifier = string

  module Action : sig
    type t
    val expr         : t -> ocamlexpr
    val keywords     : t -> Keyword.keyword list
  end

  module Production : sig
    include INDEXED with type t = production
    val kind         : t -> [`REGULAR | `START]
    val lhs          : t -> nonterminal
    val rhs          : t -> (symbol * identifier * Attribute.t list) array
    val positions    : t -> Range.t list
    val action       : t -> Action.t option
    val attributes   : t -> Attribute.t list
  end

  module Lr0 : sig
    include INDEXED with type t = lr0
    val incoming     : t -> symbol option
    val items        : t -> item list
  end

  module Lr1 : sig
    include INDEXED with type t = lr1
    val lr0          : t -> lr0
    val transitions  : t -> (symbol * t) list
    val reductions   : t -> (terminal * production list) list
  end

  module Print : sig
    open Format
    val terminal            : formatter -> terminal -> unit
    val nonterminal         : formatter -> nonterminal -> unit
    val symbol              : formatter -> symbol -> unit
    val mangled_nonterminal : formatter -> nonterminal -> unit
    val mangled_symbol      : formatter -> symbol -> unit
    val production          : formatter -> production -> unit
    val item                : formatter -> item -> unit
    val itemset             : formatter -> item list -> unit
    val annot_item          : string      list -> formatter -> item      -> unit
    val annot_itemset       : string list list -> formatter -> item list -> unit
  end

end
