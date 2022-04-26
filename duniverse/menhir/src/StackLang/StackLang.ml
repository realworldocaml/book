(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* A StackLang program can be thought of as a control flow graph, that is, a
   mapping of code labels to code blocks. The StackLang machine maintains a
   number of registers and a stack. The number and names of the registers is
   allowed to depend on the program point: that is, at different points in the
   program, different sets of registers may be defined. However, it is
   possible to statically determine which registers are defined at each
   program point. *)

(* -------------------------------------------------------------------------- *)

(* Registers, labels, patterns, values, bindings. *)

include StackLangBasics

type bindings =
  Bindings.t

(* -------------------------------------------------------------------------- *)

(* A primitive operation involves the execution of some OCaml code. The
   primitive operations include calls to the lexer, read accesses to an
   OCaml record, and semantic actions. *)

(* The primitive operation [PrimLexerCall vs] represents a call to the
   lexer. Its arguments are two values that represent the lexer and the
   lexbuf.  *)

(* The primitive operation [PrimOCamlAction (bs, prod, action)] carries a set
   [bs] of local bindings, a production index [prod], and a semantic action
   [action], which must be [Production.action prod]. *)

(* The set of registers read by a semantic action is [Action.vars action]. *)

(* The bindings [bs] may update registers that are read by the semantic
   action. The presence of these bindings makes the type [primitive] stable
   under substitution, which is convenient. The scope of these bindings is
   just this semantic action; in other words, these bindings are local
   definitions that shadow the global registers; they must not be understood
   as imperative updates of the global registers. We maintain the invariant
   that the domain of the bindings [bs] is a subset of [Action.var action]. *)

type primitive =
  | PrimLexerCall of value list
  | PrimOCamlFieldAccess of value * field
  | PrimOCamlAction of bindings * production * action

and field =
  string

and action =
  Action.t

(* -------------------------------------------------------------------------- *)

(* A trace instruction requests the logging of a message to [stderr]. *)

type trace =
  string

(* -------------------------------------------------------------------------- *)

(* In a case analysis on a token, each branch is guarded by a pattern that
   either selects a single terminal symbol (and stores its semantic value
   in a register) or selects multiple terminal symbols (and ignores their
   semantic value). There can also be a default branch, which is guarded
   by a wildcard pattern; that is implicit. *)

(* If the terminal symbol [tok] does not carry a semantic value, then the
   pattern [TokSingle (tok, r)] writes a unit semantic value into register
   [r]. *)

type tokpat =
  | TokSingle of terminal * register
  | TokMultiple of terminals

(* -------------------------------------------------------------------------- *)

(* In a case analysis on a tag, each branch is guarded by a pattern that
   selects a single tag. There is no default branch. We do not allow
   disjunction patterns because they turn out to be more troublesome than
   useful. They must be expanded away when compiling down to OCaml, anyway,
   because the OCaml type-checker does not understand disjunction patterns
   when pattern-matching on a GADT. *)

type tagpat =
  | TagSingle of tag

(* -------------------------------------------------------------------------- *)

(* The shape of each stack cell is described using the type [cell] that is
   provided by the module Invariant. *)

type cell =
  Invariant.cell

(**The shape of a suffix of the stack is represented as an array of cells. We
   follow the convention that is used elsewhere (e.g., in the right-hand side
   of a production, and in the module Invariant): the right end of this array
   represents the top of the stack. *)
type stack =
  Invariant.word

(**The result type of a block (once this block is translated to an OCaml
   function). If this block is reachable from only one start symbol [nt],
   then [final] is [Some nt]. If this block is reachable from several
   start symbols, then its result type is not statically known, so this
   block is translated to a polymorphic OCaml function; in that case,
   [final] is [None]. *)
type final =
  start_nonterminal option

(**The stack type upon entry into a block and the final type of this block
   are paired to form a block type. *)
type block_type = {

  stack : stack;
    (** The known suffix of the stack associated with this state. *)

  final : final;
    (** The final type associated with this state. *)

}

(**A block is a tree-shaped collection of instructions. (In classic compiler
   terminology, it could be known as an extended basic block.) The simplest
   instructions have exactly one successor. However, the case analysis
   instructions can have more than successor (this is where several tree
   branches become separate), and the control instructions have no successor
   (this is where a tree branch ends). *)
type block =

  (* Group 1: Instructions with exactly one successor. *)

  | IPush of values * cell * block
  (** [IPush (vs, cell, _)] pushes one cell, holding the values [vs], onto
      the stack. The shape of this stack cell is described by [cell].
      The list [vs] must be nonempty. *)

  | IPop of patterns * cell * block
  (** [IPop (ps, _)] pops one cell off the stack and writes the values that
      it contains to the patterns [ps]. The list [ps] must be nonempty. *)

  | IPeek of patterns * cell * block
  (** [IPeek (ps, _)] peeks at the top stack cell and writes the values that
      it contains to the patterns [ps]. The list [ps] must be nonempty. *)

  | IDef of bindings * block
  (** [IDef (bs, _)] performs a set of assignments to registers (in
      parallel), as described by the bindings [bs]. It can be used to move
      data between registers or to load a value into a register. A sequence
      of a push and a pop can be simplified to an [IDef] instruction. *)

  | IPrim of pattern * primitive * block
  (** [IPrim (p, prim, _)] invokes the primitive operation [prim] and
      assigns its result to the pattern [p]. *)

  | ITrace of trace * block
  (** [ITrace (msg, _)] logs the message [msg]. Depending on the Boolean
      flag [Settings.trace], when StackLang is translated down to OCaml,
      this instruction is translated to either an [eprintf] instruction
      or a comment. Emitting [ITrace] instructions unconditionally at the
      level of StackLang costs us a little bit in compilation time, but
      allows us to compare the traces of the reference interpreter and of
      the StackLang program (see StackLangTester). *)

  | IComment of string * block
  (** [IComment (comment, _)] is a comment. *)

  (* Group 2: Instructions with zero successor. *)

  | IDead of [`Static | `Dynamic]
  (** [IDead phase] is a claim that this instruction is unreachable. If
      [phase] is [`Static], then it must possible to statically prove that
      this instruction is unreachable; this corresponds to [.] in OCaml.
      If [phase] is [`Dynamic], then this corresponds to a runtime failure
      in OCaml. *)

  | IStop of int
  (** [IStop s] causes the parser to stop and reject the input. It is
      translated into OCaml by raising the exception [Error]. The integer [s]
      reflects the current state of the automaton, and can be exploited by
      the user in order to display a suitable syntax error message. The
      state [s] is not necessarily represented, which is why [s] cannot have
      type [tag] and must have type [int]. *)

  | IReturn of start_nonterminal * value
  (** [IReturn (nt, v)] causes the normal termination of the parser.
      The value [v] is returned to the caller. *)

  | IJump of label
  (** [IJump label] causes a jump to the block identified by the label
      [label]. The registers that are needed by the destination block must
      form a subset of the registers that are defined at the point of the
      jump. *)

  (* Group 3: Case analysis instructions. *)

  | ICaseToken of register * tokbranch list * block option
  (** [ICaseToken] performs a case analysis on a token (which is held in a
      register). It carries a list of branches, each of which is guarded by
      a pattern, and an optional default branch. *)

  | ICaseTag of register * tagbranch list
  (** [ICaseTag (r, branches)] performs a case analysis on the tag held in
      the register [r]. It carries a list of [branches], each of which is
      guarded by a pattern.

      A branch that is provably dead can be represented as a branch whose
      body is [IDead `Static]. In fact, for the code to be well-typed,
      a provably dead branch must be explicitly represented in this way.

      Any tag that is not covered by [branches] is considered dead, but not
      provably dead. In other words, there is an implicit default branch,
      whose body is [IDead `Dynamic].

      [casetag] is perhaps the most complex and most delicate construct in
      StackLang. A case analysis on a state allows recovering information
      about the shape of the stack. This is reflected by the typing rules
      in StackLangCheck, which are somewhat elaborate, and by translating
      this construct down to OCaml as a [match] construct on the [state]
      GADT.

      In order to make life easier both at the level of StackLang and at
      the level of OCaml, we impose two restrictions on [casetag] constructs:

      - A branch in a [casetag] construct must be small and must contain
        simple instructions only. Ideally, it should contain just a [jump]
        instruction. In rare cases, we use a few other simple instructions,
        such as [prim] and [peek].

      - A [casetag] construct can be used only if the state is currenly
        statically unknown. That is, [casetag] cannot be used if the
        [state] register receives a literal value, somewhere between
        the beginning of the current block and the [casetag] construct.

      To obey the first restriction, we never inline anything into a branch
      of a [casetag] construct. Symmetrically, we never inline a routine
      that contains a [casetag] construct, unless we know that this construct
      will disapppear.

      The second restriction helps satisfy the OCaml type-checker, which
      imposes a similar restriction: a [match] on a GADT must inspect a
      value whose type is rigid. *)

and tokbranch =
  tokpat * block

and tagbranch =
  tagpat * block

(** A typed block is annotated with type information, with the registers
    that the block needs, and with inlining and specialization hints. *)
and typed_block = {

  block : block;
  (** The block. *)

  block_type : block_type;
  (** The type of this block. *)

  needed : Reg.Set.t;
  (** The registers that must be defined upon entry into this block.
      In other words, these are the registers that this block needs
      (may read). *)

  spec : spec;
  (** A specialization hint. *)

  hint : hint;
  (** An inlining hint. *)

}

(** An inlining hint tells us under what circumstances we might wish to
    inline a block. *)
and hint =

  | Always
      (** Inline always. This hint is typically carried by a very small
          routine, such as a [goto] routine that contains just a [jump]
          instruction. *)

  | OnlyIfKnownState
      (** Inline only if the current state is statically known. This hint
          is carried by every [goto] routine. This hint prevents inlining
          under other circumstances. *)

  | IfCells of int
      (** Inline if a train of [k] PUSH instructions is available. This hint
          is typically carried by a [reduce] function that contains [k] POP
          instructions. *)

  | NoHint
      (** No specific inlining hint. *)

(**A specialization hint tells us whether it is permitted to specialize a
   routine. Specialization takes place when a routine is invoked in a context
   where the current state is known. *)
and spec =
  | SpecAllowed
  | SpecDisallowed

(* -------------------------------------------------------------------------- *)

(* A control flow graph is a mapping of code labels to typed blocks. *)

type cfg =
  typed_block Label.Map.t

(* -------------------------------------------------------------------------- *)

(* Type information about states. *)

(* A mapping of tags (states) to type information allows us to generate
   the [state] GADT. *)

(* The block type associated with a state [s] is the type of its [run]
   function. *)

type states =
  block_type Tag.Map.t

(* -------------------------------------------------------------------------- *)

(* A complete program. *)

type program = {

  cfg : cfg;
    (** The control flow graph. *)

  entry : label StringMap.t;
    (** The entry points of the control flow graph. This is a map whose
        keys are the names of the start symbols. To each start symbol,
        this map associates an entry point, a label in the control flow
        graph. *)

  states : states;
    (** A map of states to type information. Only the states that are
        represented at runtime should appear in this map. *)

}

(* -------------------------------------------------------------------------- *)

(* A handful of handy functions. *)

(* [lookup program label] returns the block associated with [label] in the
   control flow graph. *)

let lookup program label : typed_block =
  try
    Label.Map.find label program.cfg
  with Not_found ->
    Printf.eprintf "Argh! Cannot find label %s.\n%!" (Label.export label);
    assert false

(* [invariant program tag] returns the block type associated with [tag]. *)

let invariant program tag : block_type =
  try
    Tag.Map.find tag program.states
  with Not_found ->
    Printf.eprintf "Argh! Cannot find tag %s.\n%!" (Tag.print tag);
    assert false

let branch_iter f (_pat, block) =
  f block

let branch_map f (pat, block) =
  (pat, f block)

(* [cardinal program] counts the number of blocks in the program [program]. *)

let cardinal program =
  Label.Map.cardinal program.cfg

(* -------------------------------------------------------------------------- *)

(* Completing the set of branches in an [ICaseTag] instruction. *)

let covered (branches : tagbranch list) : tags =
  List.fold_left (fun accu (tagpat, _body) ->
    let TagSingle tag = tagpat in
    Tag.Set.add tag accu
  ) Tag.Set.empty branches

let complete program (branches : tagbranch list) : tagbranch list =
  (* Compute which tags are covered by [branches]. *)
  let covered = covered branches in
  (* Iterate over all tags. *)
  Tag.Map.fold (fun tag _bty accu ->
    (* If this tag is not covered, create a branch for it. *)
    if Tag.Set.mem tag covered then
      accu
    else
      (TagSingle tag, IDead `Dynamic) :: accu
  ) program.states branches

(* Trimming removes the branches whose body is [IDead `Dynamic]. Because every
   [casetag] construct has an implicit branch [_ -> IDead `Dynamic], they are
   redundant. *)

let trim branches =
  let keep (_, body) = match body with IDead `Dynamic -> false | _ -> true in
  List.filter keep branches

(* -------------------------------------------------------------------------- *)

(* Computing which branches cover which tokens in [ICaseToken] instructions.  *)

open Grammar.TerminalSet

let tokens tokpat : terminals =
  match tokpat with
  | TokSingle (tok, _) ->
      singleton tok
  | TokMultiple toks ->
      toks

let all_tokens (branches : tokbranch list) : terminals =
  let (+) = union in
  List.fold_left (fun accu (tokpat, _) -> accu + tokens tokpat) empty branches

let exhaustive (branches : tokbranch list) : bool =
  subset universe (all_tokens branches)
