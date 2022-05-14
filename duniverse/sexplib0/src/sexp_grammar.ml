(** Representation of S-expression grammars *)

(** This module defines a representation for s-expression grammars. Using ppx_sexp_conv
    and [[@@deriving sexp_grammar]] produces a grammar that is compatible with the derived
    [of_sexp] for a given type.

    As with other derived definitions, polymorphic types derive a function that takes a
    grammar for each type argument and produces a grammar for the monomorphized type.

    Monomorphic types derive a grammar directly. To avoid top-level side effects,
    [[@@deriving sexp_grammar]] wraps grammars in the [Lazy] constructor as needed.

    This type may change over time as our needs for expressive grammars change. We will
    attempt to make changes backward-compatible, or at least provide a reasonable upgrade
    path. *)

[@@@warning "-30"] (* allow duplicate field names *)

(** Grammar of a sexp. *)
type grammar =
  | Any of string (** accepts any sexp; string is a type name for human readability *)
  | Bool (** accepts the atoms "true" or "false", modulo capitalization *)
  | Char (** accepts any single-character atom *)
  | Integer (** accepts any atom matching ocaml integer syntax, regardless of bit width *)
  | Float (** accepts any atom matching ocaml float syntax *)
  | String (** accepts any atom *)
  | Option of grammar (** accepts an option, both [None] vs [Some _] and [()] vs [(_)]. *)
  | List of list_grammar (** accepts a list *)
  | Variant of variant (** accepts clauses keyed by a leading or sole atom *)
  | Union of grammar list (** accepts a sexp if any of the listed grammars accepts it *)
  | Tagged of grammar with_tag
  (** annotates a grammar with a client-specific key/value pair *)
  | Tyvar of string
  (** Name of a type variable, e.g. [Tyvar "a"] for ['a]. Only meaningful when the body of
      the innermost enclosing [defn] defines a corresponding type variable. *)
  | Tycon of string * grammar list
  (** Type constructor applied to arguments. For example, [Tycon ("list", [ Integer ])]
      represents [int list]. Only meaningful when the innermost enclosing [Recursive]
      grammar defines a corresponding type constructor. *)
  | Recursive of grammar * defn list
  (** [Recursive (grammar, definitions)] allows [grammar] to refer to type constructors
      from the mutually recursive [definitions]. The definitions may also refer to each
      others' type constructors.

      Ordinarily, [grammar] itself is just a [Tycon] argument, although technically it can
      be any grammar.

      For example, the following definitions define a binary tree parameterized by a type
      stored at its leaves.

      {[
        let defns =
          [ { tycon = "tree"
            ; tyvars = ["a"]
            ; grammar =
                Variant
                  { name_kind = Capitalized
                  ; clauses =
                      [ { name = "Node"
                        ; args = Cons (Tycon ("node", [Tyvar "a"]), Empty)
                        }
                      ; { name = "Tree"
                        ; args = Cons (Tycon ("leaf", [Tyvar "a"]), Empty)
                        }
                      ]
                  }
            }
          ; { tycon = "node"
            ; tyvars = ["a"]
            ; grammar = List (Many (Tycon "tree", [Tyvar "a"]))
            }
          ; { tycon = "leaf"
            ; tyvars = ["a"]
            ; grammar = [Tyvar "a"]
            }
          ]
        ;;
      ]}

      Normally, the type of a tree storing integers would be written like this:

      {[
        Recursive (Tycon ("tree", [ Integer ]), defns)
      ]}

      It is equivalent, though needlessly verbose, to replace the [Tycon] reference with
      the grammar of ["tree"], substituting [Integer] for [Tyvar "a"]:

      {[
        Recursive
          ( Variant
              { name_kind = Capitalized
              ; clauses =
                  [ { name = "Node"
                    ; args = Cons (Tycon ("node", [Tyvar "a"]), Empty)
                    }
                  ; { name = "Tree"
                    ; args = Cons (Tycon ("leaf", [Tyvar "a"]), Empty)
                    }
                  ]
              }
          , defns )
      ]}
  *)
  | Lazy of grammar lazy_t
  (** Lazily computed grammar. Use [Lazy] to avoid top-level side effects. To define
      recursive grammars, use [Recursive] instead. *)

(** Grammar of a list of sexps. *)
and list_grammar =
  | Empty (** accepts an empty list of sexps *)
  | Cons of grammar * list_grammar
  (** accepts a non-empty list with head and tail matching the given grammars *)
  | Many of grammar (** accepts zero or more sexps, each matching the given grammar *)
  | Fields of record (** accepts sexps representing fields of a record *)

(** Case sensitivity options for names of variant constructors. *)
and case_sensitivity =
  | Case_insensitive (** Comparison is case insensitive. Used for custom parsers. *)
  | Case_sensitive (** Comparison is case sensitive. Used for polymorphic variants. *)
  | Case_sensitive_except_first_character
  (** Comparison is case insensitive for the first character and case sensitive afterward.
      Used for regular variants. *)

(** Grammar of variants. Accepts any sexp matching one of the clauses. *)
and variant =
  { case_sensitivity : case_sensitivity
  ; clauses : clause with_tag_list list
  }

(** Grammar of a single variant clause. Accepts sexps based on the [clause_kind]. *)
and clause =
  { name : string
  ; clause_kind : clause_kind
  }

(** Grammar of a single variant clause's contents. [Atom_clause] accepts an atom matching
    the clause's name. [List_clause] accepts a list whose head is an atom matching the
    clause's name and whose tail matches [args]. The clause's name is matched modulo the
    variant's [name_kind]. *)
and clause_kind =
  | Atom_clause
  | List_clause of { args : list_grammar }

(** Grammar of a record. Accepts any list of sexps specifying each of the fields,
    regardless of order. If [allow_extra_fields] is specified, ignores sexps with names
    not found in [fields]. *)
and record =
  { allow_extra_fields : bool
  ; fields : field with_tag_list list
  }

(** Grammar of a record field. A field must show up exactly once in a record if
    [required], or at most once otherwise. Accepts a list headed by [name] as an atom,
    followed by sexps matching [args]. *)
and field =
  { name : string
  ; required : bool
  ; args : list_grammar
  }

(** Grammar tagged with client-specific key/value pair. *)
and 'a with_tag =
  { key : string
  ; value : Sexp.t
  ; grammar : 'a
  }

and 'a with_tag_list =
  | Tag of 'a with_tag_list with_tag
  | No_tag of 'a

(** Grammar of a recursive type definition. Names the [tycon] being defined, and the
    [tyvars] it takes as parameters. Specifies the [grammar] of the [tycon]. The grammar
    may refer to any of the [tyvars], and to any of the [tycon]s from the same set of
    [Recursive] definitions. *)
and defn =
  { tycon : string
  ; tyvars : string list
  ; grammar : grammar
  }

(** Top-level grammar type. Has a phantom type parameter to associate each grammar with
    the type its sexps represent. This makes it harder to apply grammars to the wrong
    type, while grammars can still be easily coerced to a new type if needed. *)
type _ t = { untyped : grammar } [@@unboxed]

let coerce (type a b) ({ untyped = _ } as t : a t) : b t = t

(** This reserved key is used for all tags generated from doc comments. *)
let doc_comment_tag = "sexp_grammar.doc_comment"
