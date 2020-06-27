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

(* The code generator. *)

module Run (T : sig end) = struct

open Grammar
open IL
open CodeBits
open CodePieces
open TokenType
open Interface

(* ------------------------------------------------------------------------ *)
(* Here is a description of our code generation mechanism.

   Every internal function that we produce is parameterized by the
   parser environment [env], which contains (pointers to) the lexer,
   the lexing buffer, the last token read, etc. No global variables
   are exploited, so our parsers are reentrant. The functions that we
   export do not expect an environment as a parameter; they create a
   fresh one when invoked.

   Every state [s] is translated to a [run] function. To a first
   approximation, the only parameter of the [run] function, besides [env],
   is the stack. However, in some cases (consult the predicate [runpushes]),
   the top stack cell is not yet allocated when [run s] is called. The
   cell's contents are passed as extra parameters, and it is [run]'s
   responsibility to allocate that cell.

   (When [run] is the target of a shift transition, the position
   parameters [startp] and [endp] are redundant with the [env]
   parameter, because they are always equal to [env.startp] and
   [env.endp]. However, this does not appear to make a great
   difference in terms of code size, and makes our life easier, so we
   do not attempt to eliminate this redundancy.)

   The first thing in [run] is to discard a token, if the state was entered
   through a shift transition, and to peek at the lookahead token. When the
   current token is to be discarded, the [discard] function is invoked. It
   discards the current token, invokes the lexer to obtain a new token, and
   returns an updated environment. When we only wish to peek at the current
   token, without discarding it, we simply read [env.token]. (We have to be
   careful in cases where the current lookahead token might be [error],
   since, in those cases, [env.token] is meaningless; see below.)

   Once the lookahead token is obtained, [run] performs a case analysis of
   the lookahead token. Each branch performs one of the following. In shift
   branches, control is dispatched to another [run] function, with
   appropriate parameters, typically the current stack plus the information
   that should go into the new top stack cell (a state, a semantic value,
   locations).  In reduce branches, a [reduce] function is invoked. In the
   default branch, error handling is initiated (see below).

   The [reduce] function associated with production [prod] pops as
   many stack cells as necessary, retrieving semantic values and the
   state [s] that initiated the reduction. It then evaluates the
   semantic action, which yields a new semantic value. (This is the
   only place where semantic actions are evaluated, so that semantic
   actions are never duplicated.) It then passes control on to the
   [goto] function associated with the nonterminal [nt], where [nt]
   is the left-hand side of the production [prod].

   The [goto] function associated with nonterminal [nt] expects just
   one parameter besides the environment -- namely, the
   stack. However, in some cases (consult the predicate [gotopushes]),
   the top stack cell is not allocated yet, so its contents are passed
   as extra parameters. In that case, [goto] first allocates that
   cell. Then, it examines the state found in that cell and performs a
   goto transition, that is, a shift transition on the nonterminal
   symbol [nt]. This simply consists in passing control to the [run]
   function associated with the transition's target state. If this
   case analysis only has one branch, because all transitions for [nt]
   lead to the same target state, then no case analysis is required.

   In principle, a stack cell contains a state, a semantic value, and
   start and end positions. However, the state can be omitted if it is
   never consulted by a [goto] function. The semantic value can be
   omitted if it is associated with a token that was declared not to
   carry a semantic value. (One could also omit semantic values for
   nonterminals whose type was declared to be [unit], but that does
   not seem very useful.) The start or end position can be omitted if
   they are associated with a symbol that does not require keeping
   track of positions. When all components of a stack cell are
   omitted, the entire cell disappears, so that no memory allocation
   is required.

   For each start symbol [nt], an entry point function, named after
   [nt], is generated. Its parameters are a lexer and a lexing buffer.
   The function allocates and initializes a parser environment and
   transfers control to the appropriate [run] function.

   Our functions are grouped into one huge [let rec] definition. The
   inliner, implemented as a separate module, will inline functions
   that are called at most once, remove dead code (although there
   should be none or next to none), and possibly perform other
   transformations.

   I note that, if a state can be entered only through (nondefault)
   reductions, then, in that state, the lookahead token must be a
   member of the set of tokens that allow these reductions, and by
   construction, there must exist an action on that token in that
   state. Thus, the default branch (which signals an error when
   the lookahead token is not a member of the expected set) is in
   fact dead. It would be nice (but difficult) to exploit types
   to prove that. However, one could at least replace the code of
   that branch with a simple [assert false]. TEMPORARY do it *)

(* ------------------------------------------------------------------------ *)
(* Here is a description of our error handling mechanism.

   With every state [s], we associate an [error] function.

   If [s] is willing to act when the lookahead token is [error], then
   this function tells how. This includes *both* shift *and* reduce
   actions. (For some reason, yacc/ocamlyacc/mule/bison can only shift
   on [error].)

   If [s] is unable to act when the lookahead token is [error], then
   this function pops a stack cell, extracts a state [s'] out of it,
   and transfers control, via a global [errorcase] dispatch function,
   to the [error] function associated with [s']. (Because some stack
   cells do not physically hold a state, this description is somewhat
   simpler than the truth, but that's the idea.)

   When an error is detected in state [s], then (see [initiate]) the [error]
   function associated with [s] is invoked. Immediately before invoking the
   [error] function, the flag [env.error] is set.  By convention, this means
   that the current token is discarded and replaced with an [error] token. The
   [error] token transparently inherits the positions associated with the
   underlying concrete token.

   Whenever we attempt to consult the current token, we check whether
   [env.error] is set and, if that is the case, resume error handling
   by calling the [error] function associated with the current state.
   This allows a series of reductions to correctly take place when the
   lookahead token is [error]. In many states, though, it is possible
   to statically prove that [env.error] cannot be set. In that case,
   we produce a lookup of [env.token] without checking [env.error].

   The flag [env.error] is cleared when a token is shifted.

   States with default reductions perform a reduction regardless of
   the current lookahead token, which can be either [error] or a
   regular token.

   A question that bothered me for a while was, when unwinding the
   stack, do we stop at a state that has a default reduction? Should
   it be considered able to handle the error token? I now believe that
   the answer is, this cannot happen. Indeed, if a state has a default
   reduction, then, whenever it is entered, reduction is performed and
   that state is exited, which means that it is never pushed onto the
   stack. So, it is fine to consider that a state with a default
   reduction is unable to handle errors.

   I note that a state that can handle [error] and has a default
   reduction must in fact have a reduction action on [error]. *)

(* The type of environments. *)

let tcenv =
  env

let tenv =
  TypApp (tcenv, [])

(* The [assertfalse] function. We have just one of these, in order to
   save code size. It should become unnecessary when we add GADTs. *)

let assertfalse =
  prefix "fail"

(* The [discard] function. *)

let discard =
  prefix "discard"

(* The [initenv] function. *)

let initenv =
  prefix "init"

(* The [run] function associated with a state [s]. *)

let run s =
  prefix (Printf.sprintf "run%d" (Lr1.number s))

(* The [goto] function associated with a nonterminal [nt]. *)

let goto nt =
  prefix (Printf.sprintf "goto_%s" (Nonterminal.print true nt))

(* The [reduce] function associated with a production [prod]. *)

let reduce prod =
  prefix (Printf.sprintf "reduce%d" (Production.p2i prod))

(* The [errorcase] function. *)

let errorcase =
  prefix "errorcase"

(* The [error] function associated with a state [s]. *)

let error s =
  prefix (Printf.sprintf "error%d" (Lr1.number s))

(* The constant associated with a state [s]. *)

let statecon s =
  dataprefix (Printf.sprintf "State%d" (Lr1.number s))

let estatecon s =
  EData (statecon s, [])

let pstatecon s =
  PData (statecon s, [])

let pstatescon ss =
  POr (List.map pstatecon ss)

(* The type of states. *)

let tcstate =
  prefix "state"

let tstate =
  TypApp (tcstate, [])

(* The [print_token] function. This automatically generated function
   is used in [--trace] mode. *)

let print_token =
  prefix "print_token"

(* Fields in the environment record. *)

let flexer =
  prefix "lexer"

let flexbuf =
  prefix "lexbuf"

let ftoken =
  prefix "token"

let ferror =
  prefix "error"

(* The type variable that represents the stack tail. *)

let tvtail =
  tvprefix "tail"

let ttail =
  TypVar tvtail

(* The result type for every function. TEMPORARY *)

let tvresult =
  tvprefix "return"

let tresult =
  TypVar tvresult

(* ------------------------------------------------------------------------ *)
(* Helpers for code production. *)

let var x : expr =
  EVar x

let pvar x : pattern =
  PVar x

let magic e : expr =
  EMagic e

let nomagic e =
  e

(* The following assertion checks that [env.error] is [false]. *)

let assertnoerror : pattern * expr =
  PUnit,
  EApp (EVar "assert", [ EApp (EVar "not", [ ERecordAccess (EVar env, ferror) ]) ])

let trace (format : string) (args : expr list) : (pattern * expr) list =
  if Settings.trace then
    [ PUnit, EApp (EVar "Printf.fprintf", (EVar "stderr") :: (EStringConst (format ^"\n%!")) :: args) ]
  else
    []

let tracecomment (comment : string) (body : expr) : expr =
  if Settings.trace then
    blet (trace comment [], body)
  else
    EComment (comment, body)

let auto2scheme t =
  scheme [ tvtail; tvresult ] t

(* ------------------------------------------------------------------------ *)
(* Accessing the positions of the current token. *)

(* There are two ways we can go about this. We can read the positions
   from the lexbuf immediately after we request a new token, or we can
   wait until we need the positions and read them at that point. As of
   2014/12/12, we switch to the latter approach. The speed difference
   in a micro-benchmark is not measurable, but this allows us to save
   two fields in the [env] record, which should be a good thing, as it
   implies less frequent minor collections. *)

let getstartp =
  ERecordAccess (ERecordAccess (EVar env, flexbuf), "Lexing.lex_start_p")

let getendp =
  ERecordAccess (ERecordAccess (EVar env, flexbuf), "Lexing.lex_curr_p")

(* ------------------------------------------------------------------------ *)
(* Determine whether the [goto] function for nonterminal [nt] will push
   a new cell onto the stack. If it doesn't, then that job is delegated
   to the [run] functions called by [goto].

   One could decide that [gotopushes] always returns true, and produce
   decent code. As a refinement, we decide to drive the [push]
   operation inside the [run] functions if all of them are able to
   eliminate this operation via shiftreduce optimization. This will be
   the case if all of these [run] functions implement a default
   reduction of a non-epsilon production.

   If that is not the case, then [gotopushes] returns true. In
   general, it is good to place the [push] operation inside [goto],
   because multiple [reduce] functions transfer control to [goto], and
   [goto] in turn transfers control to multiple [run] functions. Hence,
   this is where code sharing is maximal. All of the [run] functions
   that [goto] can transfer control to expect a stack cell of the same
   shape (indeed, the symbol [nt] is the same in every case, and the
   state is always represented), which makes this decision possible. *)

let gotopushes : Nonterminal.t -> bool =
  Nonterminal.tabulate (fun nt ->
    not (
      Lr1.targets (fun accu _ target ->
        accu &&
        match Default.has_default_reduction target with
        | Some (prod, _) ->
            Production.length prod > 0
        | None -> false
      ) true (Symbol.N nt)
    )
  )

(* ------------------------------------------------------------------------ *)
(* Determine whether the [run] function for state [s] will push a new cell
   onto the stack.

   Our convention is this. If this [run] function is entered via a shift
   transition, then it is in charge of pushing a new stack cell. If it
   is entered via a goto transition, then it is in charge of pushing a
   new cell if and only if the [goto] function that invoked it did not
   do so. Last, if this [run] function is invoked directly by an entry
   point, then it does not push a stack cell. *)

let runpushes s =
  match Lr1.incoming_symbol s with
  | Some (Symbol.T _) ->
      true
  | Some (Symbol.N nt) ->
      not (gotopushes nt)
  | None ->
      false

(* ------------------------------------------------------------------------ *)
(* In some situations, we are able to fuse a shift (or goto)
   transition with a reduce transition, which means that we save the
   cost (in speed and in code size) of pushing and popping the top
   stack cell.

   This involves creating a modified version of the [reduce] function
   associated with a production [prod], where the contents of the top
   stack cell are passed as extra parameters. Because we wish to avoid
   code duplication, we perform this change only if all call sites for
   [reduce] agree on this modified calling convention.

   At the call site, the optimization is possible only if a stack cell
   allocation exists and is immediately followed by a call to
   [reduce]. This is the case inside the [run] function for state [s]
   when [run] pushes a stack cell and performs a default reduction.

   This optimization amounts to coalescing the push operation inside
   [run] with the pop operation that follows inside [reduce].

   Unit production elimination, on the other hand, would coalesce the
   pop operation inside [reduce] with the push operation that follows
   inside [goto]. For this reason, the two are contradictory. As a
   result, we do not attempt to perform unit production elimination.
   In fact, we did implement it at one point and found that it was
   seldom applicable, because preference was given to the shiftreduce
   optimization.

   There are cases where shiftreduce optimization does not make any
   difference, for instance, if production [prod] is never reduced, or
   if the top stack cell is in fact nonexistent. *)

let shiftreduce : Production.index -> bool =
  Production.tabulate (fun prod ->

    (* Check that this production pops at least one stack cell. *)

    Production.length prod > 0 &&

    (* Check that all call sites push a stack cell and have a
       default reduction. *)

    Lr1.NodeSet.fold (fun s accu ->
      accu && (match Default.has_default_reduction s with None -> false | Some _ -> true)
           && (runpushes s)
    ) (Lr1.production_where prod) true

  )

let shiftreducecount =
  Production.sum (fun prod -> if shiftreduce prod then 1 else 0)

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
       "%d out of %d productions exploit shiftreduce optimization.\n"
       shiftreducecount Production.n)

(* Check that, as predicted above, [gotopushes nt] returns [false]
   only when all of the [run] functions that follow it perform
   shiftreduce optimization.

   This can be proved as follows. If [gotopushes nt] returns [false],
   then every successor state [s] has a default reduction for some
   non-epsilon production [prod]. Furthermore, all states that can
   reduce [prod] must be successors of that same [goto] function:
   indeed, because the right-hand side of the production ends with
   symbol [nt], every state that can reduce [prod] must be entered
   through [nt]. So, at all such states, [runpushes] is true, which
   guarantees that [shiftreduce prod] is true as well. *)

let () =
  assert (
    Nonterminal.fold (fun nt accu ->
      accu &&
      if gotopushes nt then
        true
      else
        Lr1.targets (fun accu _ target ->
          accu &&
          match Default.has_default_reduction target with
          | Some (prod, _) ->
              shiftreduce prod
          | None ->
              false
        ) true (Symbol.N nt)
    ) true
  )

(* ------------------------------------------------------------------------ *)
(* Type production. *)

(* This is the type of states. Only states that are represented are
   declared. *)

let statetypedef = {
  typename =       tcstate;
  typeparams =     [];
  typerhs =        TDefSum (
                     Lr1.fold (fun defs s ->
                       if Invariant.represented s then {
                         dataname =       statecon s;
                         datavalparams =  [];
                         datatypeparams = None
                       } :: defs
                       else defs
                     ) []
                   );
  typeconstraint = None
}

(* The type of lexers. *)

let tlexer =
  TypArrow (tlexbuf, ttoken)

(* This is the type of parser environments. *)

let field modifiable name t =
  {
    modifiable = modifiable;
    fieldname = name;
    fieldtype = type2scheme t
  }

let envtypedef = {
  typename = tcenv;
  typeparams = [];
  typerhs =
    TDefRecord [

      (* The lexer itself. *)

      field false flexer tlexer;

      (* The lexing buffer. *)

      field false flexbuf tlexbuf;

      (* The last token that was read from the lexer. This is the
         head of the token stream, unless [env.error] is set. *)

      field false ftoken ttoken;

      (* A flag which tells whether we currently have an [error] token
         at the head of the stream. When this flag is set, the head
         of the token stream is the [error] token, and the contents of
         the [token] field is irrelevant. The token following [error]
         is obtained by invoking the lexer again. *)

      field true ferror tbool;

    ];
  typeconstraint = None
}

(* [curry] curries the top stack cell in a type [t] of the form
   [(stack type) arrow (result type)]. [t] remains unchanged if the
   stack type does not make at least one cell explicit. *)

let curry = function
  | TypArrow (TypTuple (tstack :: tcell), tresult) ->
      TypArrow (tstack, marrow tcell tresult)
  | TypArrow _ as t ->
      t
  | _ ->
      assert false

(* [curryif true] is [curry], [curryif false] is the identity. *)

let curryif flag t =
  if flag then curry t else t

(* Types for stack cells.

   [celltype tailtype holds_state symbol] returns the type of a stack
   cell. The parameter [tailtype] is the type of the tail of the
   stack. The flag [holds_state] tells whether the cell holds a state.
   The parameter [symbol] is used to determine whether the cell holds
   a semantic value and what its type is.

   A subtlety here and in [curry] above is that singleton stack cells
   give rise to singleton tuple types, which the type printer
   eliminates, but which do exist internally. As a result, [curry]
   always correctly removes the top stack cell, even if it is a
   singleton tuple cell. *)

let celltype tailtype holds_state symbol _ =
  TypTuple (
    tailtype ::
    elementif (Invariant.endp symbol) tposition @
    elementif holds_state tstate @
    semvtype symbol @
    elementif (Invariant.startp symbol) tposition
  )

(* Types for stacks.

   [stacktype s] is the type of the stack at state [s]. [reducestacktype prod]
   is the type of the stack when about to reduce production [prod].
   [gotostacktype nt] is the type of the stack when the [goto]
   function associated with [nt] is called.

   In all cases, the tail (that is, the unknown part) of the stack is
   represented by [ttail], currently a type variable.

   These stack types are obtained by folding [celltype] over a
   description of the stack provided by module [Invariant]. *)

let stacktype s =
  Invariant.fold celltype ttail (Invariant.stack s)

let reducestacktype prod =
  Invariant.fold celltype ttail (Invariant.prodstack prod)

let gotostacktype nt =
  Invariant.fold celltype ttail (Invariant.gotostack nt)

(* The type of the [run] function. As announced earlier, if [s] is the
   target of shift transitions, the type of the stack is curried, that
   is, the top stack cell is not yet allocated, so its contents are
   passed as extra parameters. If [s] is the target of goto
   transitions, the top stack cell is allocated. If [s] is a start
   state, this issue makes no difference. *)

let runtypescheme s =
  auto2scheme (
    arrow tenv (
      curryif (runpushes s) (
        arrow (stacktype s) tresult
      )
    )
  )

(* The type of the [goto] function. The top stack cell is curried. *)

let gototypescheme nt =
  auto2scheme (arrow tenv (curry (arrow (gotostacktype nt) tresult)))

(* If [prod] is an epsilon production and if the [goto] function
   associated with it expects a state parameter, then the [reduce]
   function associated with [prod] also requires a state parameter. *)

let reduce_expects_state_param prod =
  let nt = Production.nt prod in
  Production.length prod = 0 &&
  Invariant.fold (fun _ holds_state _ _ -> holds_state) false (Invariant.gotostack nt)

(* The type of the [reduce] function. If shiftreduce optimization
   is performed for this production, then the top stack cell is
   not explicitly allocated. *)

let reducetypescheme prod =
  auto2scheme (
    arrow tenv (
      curryif (shiftreduce prod) (
        arrow (reducestacktype prod) (
          arrowif (reduce_expects_state_param prod) tstate tresult
        )
      )
    )
  )

(* The type of the [errorcase] function. The shape of the stack is
   unknown, and is determined by examining the state parameter. *)

let errorcasetypescheme =
  auto2scheme (marrow [ tenv; ttail; tstate ] tresult)

(* The type of the [error] function. The shape of the stack is the
   one associated with state [s]. *)

let errortypescheme s =
  auto2scheme ( marrow [ tenv; stacktype s ] tresult)

(* ------------------------------------------------------------------------ *)
(* Code production preliminaries. *)

(* This flag will be set to [true] if we ever raise the [Error]
   exception. This happens when we unwind the entire stack without
   finding a state that can handle errors. *)

let can_die =
  ref false

(* A code pattern for an exception handling construct where both alternatives
   are in tail position. Concrete syntax in OCaml 4.02 is [match e with x ->
   e1 | exception Error -> e2]. Earlier versions of OCaml do not support this
   construct. We continue to emulate it using a combination of [try/with],
   [match/with], and an [option] value. It is used only in a very rare case
   anyway. *)

let letunless e x e1 e2 =
  EMatch (
    ETry (
      EData ("Some", [ e ]),
      [ { branchpat = PData (excdef.excname, []); branchbody = EData ("None", []) } ]
    ),
    [ { branchpat = PData ("Some", [ PVar x ]); branchbody = e1 };
      { branchpat = PData ("None", []); branchbody = e2 } ]
  )

(* ------------------------------------------------------------------------ *)
(* Calling conventions. *)

(* The layout of a stack cell is determined here. The first field in a stack
   cell is always a pointer to the rest of the stack; it is followed by the
   fields listed below, each of which may or may not appear. [runpushcell] and
   [gotopushcell] are the two places where stack cells are allocated. *)

(* 2015/11/04. We make [endp] the first element in the list of optional fields,
   so we are able to access it at a fixed offset, provided we know that it
   exists. This is exploited when reducing an epsilon production. *)

(* The contents of a stack cell, exposed as individual parameters. The choice of
   identifiers is suitable for use in the definition of [run]. *)

let runcellparams var holds_state symbol =
  elementif (Invariant.endp symbol) (var endp) @
  elementif holds_state (var state) @
  symval symbol (var semv) @
  elementif (Invariant.startp symbol) (var startp)

(* The contents of a stack cell, exposed as individual parameters, again.
   The choice of identifiers is suitable for use in the definition of a
   [reduce] function.

   [prod] is the production's index. The integer [i] tells which
   symbol on the right-hand side we are focusing on, that is, which
   symbol this stack cell is associated with. *)

let reducecellparams prod i holds_state symbol =
  let ids = Production.identifiers prod in

  (* The semantic value is bound to the variable [ids.(i)]. Its type is [t]. As
     of 2016/03/11, we generate a type annotation. Indeed, because of our use of
     [magic], the semantic value would otherwise have an unknown type; and, if
     it is a function, the OCaml compiler could warn (incorrectly) that this
     function does not use its argument. *)

  let semvpat t =
    PAnnot (PVar ids.(i), t)
  in

  elementif (Invariant.endp symbol) (PVar (Printf.sprintf "_endpos_%s_" ids.(i))) @
  elementif holds_state (if i = 0 then PVar state else PWildcard) @
  symvalt symbol semvpat @
  elementif (Invariant.startp symbol) (PVar (Printf.sprintf "_startpos_%s_" ids.(i)))

(* The contents of a stack cell, exposed as individual parameters,
   again. The choice of identifiers is suitable for use in the
   definition of [error]. *)

let errorcellparams (i, pat) holds_state symbol _ =
  i + 1,
  ptuple (
    pat ::
    elementif (Invariant.endp symbol) PWildcard @
    elementif holds_state (if i = 0 then PVar state else PWildcard) @
    symval symbol PWildcard @
    elementif (Invariant.startp symbol) PWildcard
  )

(* Calls to [run]. *)

let runparams magic var s =
  var env ::
  magic (var stack) ::
  listif (runpushes s) (Invariant.fold_top (runcellparams var) [] (Invariant.stack s))

let call_run s actuals =
  EApp (EVar (run s), actuals)

(* The parameters to [reduce]. When shiftreduce optimization is in
   effect, the top stack cell is not allocated, so extra parameters
   are required. Note that [shiftreduce prod] and
   [reduce_expects_state_param prod] are mutually exclusive
   conditions, so the [state] parameter is never bound twice. *)

let reduceparams prod =
  PVar env ::
  PVar stack ::
  listif (shiftreduce prod) (
    Invariant.fold_top
      (reducecellparams prod (Production.length prod - 1))
    [] (Invariant.prodstack prod)
  ) @
  elementif (reduce_expects_state_param prod) (PVar state)

(* Calls to [reduce]. One must specify the production [prod] as well
   as the current state [s]. *)

let call_reduce prod s =
  let actuals =
    (EVar env) ::
    (EMagic (EVar stack)) ::
    listif (shiftreduce prod)
      (Invariant.fold_top (runcellparams var) [] (Invariant.stack s))
      (* compare with [runpushcell s] *) @
    elementif (reduce_expects_state_param prod) (estatecon s)
  in
  EApp (EVar (reduce prod), actuals)

(* Calls to [goto]. *)

let gotoparams var nt =
  var env ::
  var stack ::
  Invariant.fold_top (runcellparams var) [] (Invariant.gotostack nt)

let call_goto nt =
  EApp (EVar (goto nt), gotoparams var nt)

(* Calls to [errorcase]. *)

let errorcaseparams magic var =
  [ var env; magic (var stack); var state ]

let call_errorcase =
  EApp (EVar errorcase, errorcaseparams magic var)

(* Calls to [error]. *)

let errorparams magic var =
  [ var env; magic (var stack) ]

let call_error magic s =
  EApp (EVar (error s), errorparams magic var)

let call_error_via_errorcase magic s = (* TEMPORARY document *)
  if Invariant.represented s then
    EApp (EVar errorcase, [ var env; magic (var stack); estatecon s ])
  else
    call_error magic s

(* Calls to [assertfalse]. *)

let call_assertfalse =
  EApp (EVar assertfalse, [ EVar "()" ])

(* ------------------------------------------------------------------------ *)
(* Code production for the automaton functions. *)

(* Count how many states actually can peek at an error token. This
   figure is, in general, inferior or equal to the number of states at
   which [Invariant.errorpeeker] is true, because some of these states
   have a default reduction and will not consult the lookahead
   token. *)

let errorpeekers =
  ref 0

(* Code for calling the reduction function for token [prod] upon
   finding a token within [toks]. This produces a branch, to be
   inserted in a [run] function for state [s]. *)

let reducebranch toks prod s =
  {
    branchpat =
      tokspat toks;
    branchbody =
      call_reduce prod s
  }

(* Code for shifting from state [s] to state [s'] via the token [tok].
   This produces a branch, to be inserted in a [run] function for
   state [s].

   The callee, [run s'], is responsible for taking the current token
   off the input stream. (There is actually a case where the token is
   *not* taken off the stream: when [s'] has a default reduction on
   [#].)

   It is also responsible for pushing a new stack cell. The rationale
   behind this decision is that there may be multiple shift
   transitions into [s'], so we actually share that code by placing it
   inside [run s'] rather than inside every transition. *)

let shiftbranchbody s tok s' =

  (* Construct the actual parameters for [run s']. *)

  let actuals =
    (EVar env) ::
    (EMagic (EVar stack)) ::
    Invariant.fold_top (fun holds_state symbol ->
      assert (Symbol.equal (Symbol.T tok) symbol);
      elementif (Invariant.endp symbol) getendp @
      elementif holds_state (estatecon s) @
      tokval tok (EVar semv) @
      elementif (Invariant.startp symbol) getstartp
    ) [] (Invariant.stack s')
  in

  (* Call [run s']. *)

  tracecomment
   (Printf.sprintf "Shifting (%s) to state %d" (Terminal.print tok) (Lr1.number s'))
   (call_run s' actuals)

let shiftbranch s tok s' =
  assert (not (Terminal.pseudo tok));
  {
    branchpat =
      PData (tokendata (Terminal.print tok), tokval tok (PVar semv));
    branchbody =
      shiftbranchbody s tok s'
  }

(* This generates code for pushing a new stack cell upon entering the
   [run] function for state [s]. *)

let runpushcell s e =
  if runpushes s then
    let contents = var stack :: Invariant.fold_top (runcellparams var) [] (Invariant.stack s) in
    mlet [ pvar stack ] [ etuple contents ] e
  else
    e

let runpushcellunless shiftreduce s e =
  if shiftreduce then
    EComment ("Not allocating top stack cell", e)
  else
    runpushcell s e

(* This generates code for dealing with the lookahead token upon
   entering the [run] function for state [s]. If [s] is the target of
   a shift transition, then we must take the current token (which was
   consumed in the shift transition) off the input stream. Whether [s]
   was entered through a shift or a goto transition, we want to peek
   at the next token, unless we are performing a default reduction.
   The parameter [defred] tells which default reduction, if any, we
   are about to perform. *)

(* 2014/12/06 New convention regarding initial states (i.e., states
   which have no incoming symbol). The function [initenv] does not
   invoke the lexer, so the [run] function for an initial state must
   do it. (Except in the very special case where the initial state
   has a default reduction on [#] -- this means the grammar recognizes
   only the empty word. We have ruled out this case.) *)

let gettoken s defred e =
  match Lr1.incoming_symbol s, defred with

  | (Some (Symbol.T _) | None), Some (_, toks)
    when TerminalSet.mem Terminal.sharp toks ->
      assert (TerminalSet.cardinal toks = 1);

      (* There is a default reduction on token [#]. We cannot
         request the next token, since that might drive the
         lexer off the end of the input stream, so we cannot
         call [discard]. Do nothing. *)

      e

  | (Some (Symbol.T _) | None), Some _ ->

      (* There is some other default reduction. Discard the first
         input token. *)

      blet ([
        PVar env, EApp (EVar discard, [ EVar env ])
        (* Note that we do not read [env.token]. *)
      ], e)

  | (Some (Symbol.T _) | None), None ->

      (* There is no default reduction. Discard the first input token
         and peek at the next one. *)

      blet ([
        PVar env, EApp (EVar discard, [ EVar env ]);
        PVar token, ERecordAccess (EVar env, ftoken)
      ], e)

  | Some (Symbol.N _), Some _ ->

      (* There is some default reduction. Do not peek at the input
         token. *)

      e

  | Some (Symbol.N _), None ->

      (* There is no default reduction. Peek at the first input token,
         without taking it off the input stream. This is normally done
         by reading [env.token], unless the token might be [error]:
         then, we check [env.error] first. *)

      if Invariant.errorpeeker s then begin
        incr errorpeekers;
        EIfThenElse (
          ERecordAccess (EVar env, ferror),
          tracecomment "Resuming error handling" (call_error_via_errorcase magic s),
          blet ([ PVar token, ERecordAccess (EVar env, ftoken) ], e)
        )
      end
      else
        blet ([ assertnoerror;
                PVar token, ERecordAccess (EVar env, ftoken) ], e)

(* This produces the header of a [run] function. *)

let runheader s body =
  let body =
    tracecomment (Printf.sprintf "State %d:" (Lr1.number s)) body
  in {
    valpublic = false;
    valpat = PVar (run s);
    valval = EAnnot (EFun (runparams nomagic pvar s, body), runtypescheme s)
  }

(* This produces the comment attached with a default reduction. *)

let defaultreductioncomment toks e =
  EPatComment (
    "Reducing without looking ahead at ",
    tokspat toks,
    e
  )

(* This produces some bookkeeping code that is used when initiating error
   handling. We set the flag [env.error]. By convention, the field [env.token]
   becomes meaningless and one considers that the first token on the input
   stream is [error]. As a result, the next peek at the lookahead token will
   cause error handling to be resumed. The next call to [discard] will take
   the [error] token off the input stream and clear [env.error]. *)

(* It seems convenient for [env.error] to be a mutable field, as this allows
   us to generate compact code. Re-allocating the whole record would produce
   less compact code. And speed is not an issue in this error-handling code. *)

let errorbookkeeping e =
  tracecomment
    "Initiating error handling"
    (blet (
      [ PUnit, ERecordWrite (EVar env, ferror, etrue) ],
      e
    ))

(* This code is used to indicate that a new error has been detected in
   state [s].

   If I am correct, [env.error] is never set here. Indeed, that would mean
   that we first found an error, and then signaled another error before
   being able to shift the first error token. My understanding is that this
   cannot happen: when the first error is signaled, we end up at a state
   that is willing to handle the error token, by a series of reductions
   followed by a shift.

   We initiate error handling by first performing the standard bookkeeping
   described above, then transferring control to the [error] function
   associated with [s]. *)

let initiate s =

  blet (
    [ assertnoerror ],
    errorbookkeeping (call_error_via_errorcase magic s)
  )

(* This produces the body of the [run] function for state [s]. *)

let rundef s : valdef =

  match Default.has_default_reduction s with
  | Some (prod, toks) as defred ->

      (* Perform reduction without looking ahead.

         If shiftreduce optimization is being performed, then no
         stack cell is allocated. The contents of the top stack
         cell are passed do [reduce] as extra parameters. *)

      runheader s (
        runpushcellunless (shiftreduce prod) s (
          gettoken s defred (
            defaultreductioncomment toks (
              call_reduce prod s
            )
          )
        )
      )

  | None ->

      (* If this state is willing to act on the error token, ignore
         that -- this is taken care of elsewhere. *)

      let transitions =
        SymbolMap.remove (Symbol.T Terminal.error) (Lr1.transitions s)
      and reductions =
        TerminalMap.remove Terminal.error (Lr1.reductions s)
      in

      (* Construct the main case analysis that determines what action
         should be taken next.

         A default branch, where an error is detected, is added if the
         analysis is not exhaustive. In the default branch, we
         initiate error handling. *)

      let covered, branches =
        ProductionMap.fold (fun prod toks (covered, branches) ->
          (* There is a reduction for these tokens. *)
          TerminalSet.union toks covered,
          reducebranch toks prod s :: branches
        ) (Lr0.invert reductions) (TerminalSet.empty, [])
      in

      let covered, branches =
        SymbolMap.fold (fun symbol s' (covered, branches) ->
          match symbol with
          | Symbol.T tok ->
              (* There is a shift transition for this token. *)
              TerminalSet.add tok covered,
              shiftbranch s tok s' :: branches
          | Symbol.N _ ->
              covered, branches
        ) transitions (covered, branches)
      in

      let branches =
        if TerminalSet.subset TerminalSet.universe covered then
          branches
        else
          branches @ [ { branchpat = PWildcard; branchbody = initiate s } ]
      in

      (* Finally, construct the code for [run]. The former pushes things
         onto the stack, obtains the lookahead token, then performs the main
         case analysis on the lookahead token. *)

      runheader s (
        runpushcell s (
          gettoken s None (
            EMatch (
              EVar token,
              branches
            )
          )
        )
      )

(* This is the body of the [reduce] function associated with
   production [prod]. *)

let reducebody prod =

  (* Find out about the left-hand side of this production and about
     the identifiers that have been bound to the symbols in the
     right-hand side. These represent variables that we should bind to
     semantic values before invoking the semantic action. *)

  let nt, rhs = Production.def prod
  and ids = Production.identifiers prod
  and length = Production.length prod in

  (* Build a pattern that represents the shape of the stack. Out of
     the stack, we extract a state (except when the production is an
     epsilon production) and a number of semantic values.

     If shiftreduce optimization is being performed, then the top
     stack cell is not explicitly allocated, so we do not include
     it in the pattern that is built. *)

  let (_ : int), pat =
    Invariant.fold (fun (i, pat) holds_state symbol _ ->
      i + 1,
      if i = length - 1 && shiftreduce prod then
        pat
      else
        ptuple (pat :: reducecellparams prod i holds_state symbol)
    ) (0, PVar stack) (Invariant.prodstack prod)
  in

  (* If any identifiers refer to terminal symbols without a semantic
     value, then bind these identifiers to the unit value. This
     provides the illusion that every symbol, terminal or nonterminal,
     has a semantic value. This is more regular and allows applying
     operators such as ? to terminal symbols without a semantic
     value. *)

  let unitbindings =
    Misc.foldi length (fun i unitbindings ->
      match semvtype rhs.(i) with
      | [] ->
          (PVar ids.(i), EUnit) :: unitbindings
      | _ ->
          unitbindings
    ) []
  in

  (* If necessary, determine start and end positions for the left-hand
     side of the production. If the right-hand side is nonempty, this
     is done by extracting position information out of the first and
     last symbols of the right-hand side. If it is empty, then (as of
     2015/11/04) this is done by taking the end position stored in the
     top stack cell (whatever it is). The constraints imposed by the
     module [Invariant], the layout of cells, and our creation of a
     sentinel cell (see [entrydef] further on), ensure that this cell
     exists and has an [endp] field at offset 1. Yes, we live
     dangerously. You only live once. *)

  let extract x =
    (* Extract the end position (i.e., the field at offset 1) in the top
       stack cell and bind it to the variable [x]. *)
    PTuple [ PWildcard; PVar x ],
    EMagic (EVar stack)
  in

  let symbol = Symbol.N nt in

  let posbindings action =
    let bind_startp = Invariant.startp symbol in
    elementif (Action.has_beforeend action)
      ( extract beforeendp
      ) @
    elementif bind_startp
      ( if length > 0 then
          PVar startp,
          EVar (Printf.sprintf "_startpos_%s_" ids.(0))
        else
          extract startp
      ) @
    elementif (Invariant.endp symbol)
      ( if length > 0 then
          PVar endp,
          EVar (Printf.sprintf "_endpos_%s_" ids.(length - 1))
        else if bind_startp then
          PVar endp,
          EVar startp
        else
          extract endp
      )
  in

  (* If this production is one of the start productions, then reducing
     it means accepting the input. In that case, we return a final
     semantic value and stop. Otherwise, we transfer control to the
     [goto] function, unless the semantic action raises [Error], in
     which case we transfer control to [errorcase]. *)

  if Production.is_start prod then

      tracecomment
        "Accepting"
        (blet (
          [ pat, EVar stack ],
          EMagic (EVar ids.(0))
        ))

  else

    let action =
      Production.action prod
    in
    let act =
      annotate (Action.to_il_expr action) (semvtypent nt)
    in

    tracecomment
      (Printf.sprintf "Reducing production %s" (Production.print prod))
      (blet (
        (pat, EVar stack) ::
        unitbindings @
        posbindings action,

        (* If the semantic action is susceptible of raising [Error],
           use a [let/unless] construct, otherwise use [let]. *)

        if Action.has_syntaxerror action then
          letunless act semv (call_goto nt) (errorbookkeeping call_errorcase)
        else
          blet ([ PVar semv, act ], call_goto nt)
      ))

(* This is the definition of the [reduce] function associated with
   production [prod]. *)

let reducedef prod =
  {
    valpublic =
      false;
    valpat =
      PVar (reduce prod);
    valval =
      EAnnot (
        EFun (
          reduceparams prod,
          reducebody prod
        ),
        reducetypescheme prod
      )
  }

(* This generates code for pushing a new stack cell inside [goto]. *)

let gotopushcell nt e =
  if gotopushes nt then
    let contents = var stack :: Invariant.fold_top (runcellparams var) [] (Invariant.gotostack nt) in
    mlet [ pvar stack ] [ etuple contents ] e
  else
    e

(* This is the heart of the [goto] function associated with
   nonterminal [nt]. *)

let gotobody nt =

  (* Examine the current state to determine where to go next. *)

  let branches =
    Lr1.targets (fun branches sources target ->
      {
        branchpat =
          pstatescon sources;
        branchbody =
          call_run target (runparams magic var target)
      } :: branches
    ) [] (Symbol.N nt)
  in

  match branches with
  | [] ->

      (* If there are no branches, then this [goto] function is never
         invoked. The inliner will drop it, so whatever we generate
         here is unimportant. *)

      call_assertfalse

  | [ branch ] ->

      (* If there is only one branch, no case analysis is required. This
         optimization is not strictly necessary if GADTs are used by the
         compiler to prove that the case analysis is exhaustive. It does
         improve readability, though, and is also useful if the compiler
         does not have GADTs. *)

      EPatComment (
        "State should be ",
        branch.branchpat,
        branch.branchbody
      )

  | _ ->

      (* In the general case, we keep the branches computed above and,
         unless [nt] is universal, add a default branch, which is
         theoretically useless but helps avoid warnings if the
         compiler does not have GADTs. *)

      let default = {
        branchpat = PWildcard;
        branchbody = call_assertfalse
      } in
      EMatch (
        EVar state,
        branches @ (if Invariant.universal (Symbol.N nt) then [] else [ default ])
      )

(* This the [goto] function associated with nonterminal [nt]. *)

let gotodef nt = {
  valpublic =
    false;
  valpat =
    PVar (goto nt);
  valval =
    EAnnot (EFun (gotoparams pvar nt, gotopushcell nt (gotobody nt)), gototypescheme nt)
}

(* ------------------------------------------------------------------------ *)
(* Code production for the error handling functions. *)

(* This is the body of the [error] function associated with state [s]. *)

let handle s e =
  tracecomment (Printf.sprintf "Handling error in state %d" (Lr1.number s)) e

let errorbody s =
  try
    let s' = SymbolMap.find (Symbol.T Terminal.error) (Lr1.transitions s) in

    (* There is a shift transition on error. *)

    handle s (
      shiftbranchbody s Terminal.error s'
    )

  with Not_found ->
    try
      let prods = TerminalMap.lookup Terminal.error (Lr1.reductions s) in
      let prod = Misc.single prods in

      (* There is a reduce transition on error. If shiftreduce
         optimization is enabled for this production, then we must pop
         an extra cell for [reduce]'s calling convention to be met. *)

      let extrapop e =
        if shiftreduce prod then
          let pat =
            ptuple (PVar stack :: Invariant.fold_top (runcellparams pvar) [] (Invariant.stack s))
          in
          blet ([ pat, EVar stack ], e)
        else
          e
      in

      handle s (
        extrapop (
          call_reduce prod s
        )
      )

    with Not_found ->

      (* This state is unable to handle errors. Pop the stack to find
         a state that does handle errors, a state that can further pop
         the stack, or die. *)

      match Invariant.rewind s with
      | Invariant.Die ->
          can_die := true;
          ERaise errorval
      | Invariant.DownTo (w, st) ->
          let _, pat = Invariant.fold errorcellparams (0, PVar stack) w in
          blet (
            [ pat, EVar stack ],
            match st with
            | Invariant.Represented ->
                call_errorcase
            | Invariant.UnRepresented s ->
                call_error magic s
          )

(* This is the [error] function associated with state [s]. *)

let errordef s = {
  valpublic =
    false;
  valpat =
    PVar (error s);
  valval =
    EAnnot (
      EFun (
        errorparams nomagic pvar,
        errorbody s
      ),
      errortypescheme s
    )
}

(* This is the [errorcase] function. It examines its state parameter
   and dispatches control to an appropriate [error] function. *)

let errorcasedef =
  let branches =
    Lr1.fold (fun branches s ->
      if Invariant.represented s then
        {
          branchpat  = pstatecon s;
          branchbody = EApp (EVar (error s), [ EVar env; EMagic (EVar stack) ])
        } :: branches
      else
        branches
    ) []
  in
  {
    valpublic =
      false;
    valpat =
      PVar errorcase;
    valval =
      EAnnot (
        EFun (
          errorcaseparams nomagic pvar,
          EMatch (
            EVar state,
            branches
          )
        ),
        errorcasetypescheme
      )
  }

(* ------------------------------------------------------------------------ *)
(* Code production for the entry points. *)

(* This is the entry point associated with a start state [s]. By convention,
   it is named after the nonterminal [nt] that corresponds to this state.
   This is a public definition.

   The code initializes a parser environment, an empty stack, and invokes
   [run].

   2015/11/11. If the state [s] can reduce an epsilon production whose left-hand
   symbol keeps track of its start or end position, or if [s] can reduce any
   production that mentions [$endpos($0)], then the initial stack should contain
   a sentinel cell with a valid [endp] field at offset 1. For simplicity, we
   always create a sentinel cell. *)

let entrydef s =
  let nt = Item.startnt (Lr1.start2item s) in
  let lexer = "lexer"
  and lexbuf = "lexbuf" in

  let initial_stack =
    let initial_position = getendp in
    etuple [ EUnit; initial_position ]
  in

  {
    valpublic = true;
    valpat = PVar (Nonterminal.print true nt);
    valval = EAnnot (
               EFun ( [ PVar lexer; PVar lexbuf ],
                 blet (
                   [ PVar env, EApp (EVar initenv, [ EVar lexer; EVar lexbuf ]) ],
                   EMagic (EApp (EVar (run s), [ EVar env; initial_stack ]))
                 )
               ),
               entrytypescheme Front.grammar (Nonterminal.print true nt)
             )
  }

(* ------------------------------------------------------------------------ *)
(* Code production for auxiliary functions. *)

(* This is [assertfalse], used when internal failure is detected.
   This should never happen if our tool is correct. *)

let assertfalsedef = {
  valpublic = false;
  valpat = PVar assertfalse;
  valval =
    EAnnot (
      EFun ([ PUnit ],
        blet ([
            PUnit, EApp (EVar "Printf.fprintf",
                       [ EVar "stderr";
                         EStringConst "Internal failure -- please contact the parser generator's developers.\n%!" ]);
          ],
          EApp (EVar "assert", [ efalse ])
        )
      ),
      scheme [ "a" ] (arrow tunit (tvar "a"))
    )
}

(* This is [print_token], used to print tokens in [--trace] mode. *)

let printtokendef =
  destructuretokendef
    print_token
    tstring
    false
    (fun tok -> EStringConst (Terminal.print tok))

(* This is [discard], used to take a token off the input stream and
   query the lexer for a new one. The code queries the lexer for a new
   token and stores it into [env.token], overwriting the previous
   token. It also stores the start and positions of the new token.
   Last, [env.error] is cleared.

   We use the lexer's [lex_start_p] and [lex_curr_p] fields to extract
   the start and end positions of the token that we just read. In
   practice, it seems that [lex_start_p] can be inaccurate (that is
   the case when the lexer calls itself recursively, instead of simply
   recognizing an atomic pattern and returning immediately). However,
   we are 100% compatible with ocamlyacc here, and there is no better
   solution anyway.

   As of 2014/12/12, we re-allocate the environment record instead of
   updating it. Perhaps surprisingly, this makes the code TWICE FASTER
   overall. The write barrier is really costly! *)

let discardbody =
  let lexer = "lexer"
  and lexbuf = "lexbuf" in
  EFun (
    [ PVar env ],
    blet ([
      PVar lexer, ERecordAccess (EVar env, flexer);
      PVar lexbuf, ERecordAccess (EVar env, flexbuf);
      PVar token, EApp (EVar lexer, [ EVar lexbuf ]);
    ] @
      trace "Lookahead token is now %s (%d-%d)"
            [ EApp (EVar print_token, [ EVar token ]);
              ERecordAccess (ERecordAccess (EVar lexbuf, "Lexing.lex_start_p"), "Lexing.pos_cnum");
              ERecordAccess (ERecordAccess (EVar lexbuf, "Lexing.lex_curr_p"), "Lexing.pos_cnum") ],
      ERecord [
        flexer, EVar lexer;
        flexbuf, EVar lexbuf;
        ftoken, EVar token;
        ferror, efalse
      ]
    )
  )

let discarddef = {
  valpublic = false;
  valpat = PVar discard;
  valval =
    annotate
      discardbody
      (arrow tenv tenv)
}

(* This is [initenv], used to allocate a fresh parser environment.
   It fills in all fields in a straightforward way. The [token]
   field receives a dummy value. It will be overwritten by the
   first call to [run], which will invoke [discard]. This allows
   us to invoke the lexer in just one place. *)

let initenvdef =
  let lexer = "lexer"
  and lexbuf = "lexbuf" in
  {
    valpublic = false;
    valpat = PVar initenv;
    valval =
      annotate (
        EFun ( [ PVar lexer; PVar lexbuf ],
          blet (
            (* We do not have a dummy token at hand, so we forge one. *)
            (* It will be overwritten by the first call to the lexer. *)
            [ PVar token, EMagic EUnit ],
            ERecord ([
              (flexer, EVar lexer);
              (flexbuf, EVar lexbuf);
              (ftoken, EVar token);
              (ferror, efalse)
            ]
            )
          )
        )
       )
       (marrow [ tlexer; tlexbuf ] tenv)
  }

(* ------------------------------------------------------------------------ *)
(* Here is complete code for the parser. *)

open BasicSyntax

let grammar =
  Front.grammar

let program =

  [ SIFunctor (grammar.parameters,

    mbasics grammar @

    SITypeDefs [ envtypedef; statetypedef ] ::

    SIStretch grammar.preludes ::

    SIValDefs (true,
      ProductionMap.fold (fun _ s defs ->
        entrydef s :: defs
      ) Lr1.entry (
      Lr1.fold (fun defs s ->
        rundef s :: errordef s :: defs
      ) (
      Nonterminal.foldx (fun nt defs ->
        gotodef nt :: defs
      ) (Production.fold (fun prod defs ->
        if Lr1.NodeSet.is_empty (Lr1.production_where prod) then
          defs
        else
          reducedef prod :: defs
      ) [ discarddef; initenvdef; printtokendef; assertfalsedef; errorcasedef ])))
    ) ::

    SIStretch grammar.postludes ::

  [])]

(* ------------------------------------------------------------------------ *)
(* We are done! *)

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
       "%d out of %d states can peek at an error.\n"
       !errorpeekers Lr1.n)

let () =
  if not !can_die then
    Error.logC 1 (fun f -> Printf.fprintf f
      "The generated parser cannot raise Error.\n")

let () =
  Time.tick "Producing abstract syntax"

end
