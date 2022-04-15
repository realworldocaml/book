(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
let map, concat = List.(map, concat)
let if1, ifn = MList.(if1, ifn)
(* Our source language: *)
open Grammar
open StackLang
let mem, elements = Reg.Set.(mem, elements)
let state = EmitStackLang.state
(* Our target language: *)
open IL
open CodeBits
let semvtype, call_stop, tokpat, tokspat, tok_bind_unit, basics, mbasics =
  CodePieces.(semvtype, call_stop, tokpat, tokspat, tok_bind_unit,
              basics, mbasics)
let print_token, call_assertfalse, printtokendef, assertfalsedef =
  CodeBackend.(print_token, call_assertfalse, printtokendef, assertfalsedef)
let exvar (r : register) = EVar (Reg.export r)
let pxvar (r : register) = PVar (Reg.export r)
let exvars rs = map exvar rs
let pxvars rs = map pxvar rs
let exlab (l : label) = EVar (Label.export l)

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* The local variable that holds the stack. *)

let stack =
  "stack" |> prefix |> Reg.import

(* The name of the state GADT. *)

let tcstate =
  prefix "state"

(* The base name of the types for stack cells. *)

let tccell =
  prefix "cell"

let dccell =
  dataprefix "Cell"

(* The base name of the nominal boxes for final types. *)

let tcbox =
  prefix "box"

let dcbox =
  dataprefix "Box"

(* The data constructor associated with a state (or tag). *)

let statecon s =
  dataprefix (sprintf "State%s" (Tag.print s))

let estatecon s =
  EData (statecon s, [])

let pstatecon s =
  PData (statecon s, [])

let tag_branch tag body =
  branch (pstatecon tag) body

(* The name of the semantic action function for production [prod]. *)

let actionname prod =
  let prod = Misc.padded_index Production.n (Production.p2i prod) in
  prefix (sprintf "action_%s" prod)

(* A type scheme for the type [token], qualified with the module name
   [MenhirBasics], so as to avoid the risk of a capture. *)

let stoken =
  let tctoken = sprintf "%s.%s" basics TokenType.tctoken in
  type2scheme (IL.TypApp (tctoken, []))

(* -------------------------------------------------------------------------- *)

(* The function [discard] takes a token off the input stream and
   queries the lexer for a new one.

   fun lexer lexbuf ->
     let token = lexer lexbuf in
     trace "Lookahead token is now...";
     token *)

let pos_cnum e =
  ERecordAccess (e, "Lexing.pos_cnum")

let lex_start_p e =
  ERecordAccess (e, "Lexing.lex_start_p")

let lex_curr_p e =
  ERecordAccess (e, "Lexing.lex_curr_p")

(* The auxiliary function [discard] invokes the lexer. If [Settings.trace] is
   set, it also prints a trace message. When [Settings.trace] is unset, we
   bypass [discard] and invoke the lexer directly. *)

let discard =
  prefix "discard"

let discarddef =
  def discard (
    let lexer, lexbuf, token = "lexer", "lexbuf", "token" in
    EFun (
      [ PVar lexer; PVar lexbuf ],
      let lexer, lexbuf = EVar lexer, EVar lexbuf in
      blet (
        [ (PVar token, EApp (lexer, [ lexbuf ])) ],
        let token = EVar token in
        blet (
          trace
            "Lookahead token is now %s (%d-%d)"
            [ EApp (EVar print_token, [ token ]);
              lexbuf |> lex_start_p |> pos_cnum;
              lexbuf |> lex_curr_p  |> pos_cnum ],
          token
  ))))

(* -------------------------------------------------------------------------- *)

(* Make everything a functor, so that the source [program] is available
   everywhere, and so that we can maintain mutable state if needed. We
   have a little bit of mutable state: see [used] below. *)

module Run (P : sig val program : StackLang.program end) = struct open P

(* -------------------------------------------------------------------------- *)

(* Generating the [state] GADT requires that the type of every nonterminal
   symbol be known. Check that this is indeed the case. *)

let () =
  let nts = Nonterminal.symbols_without_ocamltype() in
  if nts <> [] then begin
    let b = Buffer.create 1024 in
    bprintf b "\
      the code back-end requires the type of every nonterminal symbol to be\n\
      known. Please specify the type of every symbol via %%type declarations, or\n\
      enable type inference (look up --infer in the manual).\n\
      Type inference is automatically enabled when Menhir is used via Dune,\n\
      provided the dune-project file says (using menhir 2.0) or later.\n"
    ;
    bprintf b "The types of the following nonterminal symbols are unknown:";
    nts |> List.iter begin fun nt ->
      bprintf b "\n%s" (Nonterminal.print false nt)
    end;
    Error.error [] "%s" (Buffer.contents b)
  end

(* -------------------------------------------------------------------------- *)

(* Types for stack cells. *)

(* This table keeps track of which cells are used. *)

module CellSet =
  Set.Make(struct
    open Invariant
    type t = cell
    let compare cell1 cell2 =
      let symbol1, symbol2 = symbol cell1, symbol cell2 in
      let c = Symbol.compare symbol1 symbol2 in
      if c <> 0 then c else
      let holds_state1, holds_state2 = holds_state cell1, holds_state cell2 in
      compare holds_state1 holds_state2
  end)

let used =
  ref CellSet.empty

let use cell =
  used := CellSet.add cell !used

(* The symbol [cell.symbol] determines the presence and type of the fields
   [semv], [startp], [endp]. It does not determine the presence of the [state]
   field. Thus, for every non-terminal symbol, we need two types. *)

(* These types are defined as algebraic data types with one constructor (or,
   roughly speaking, tuple types). It is desirable that the cell types
   associated with distinct symbols be distinct (even if these symbols happen
   to have the same semantic value type): this allows the OCaml type-checker
   to prove that certain branches are dead, exactly in the same way as the
   StackLang type-checker. *)

(* These cell types have up to two type parameters:
   - [tvtail],  the type of the tail of the stack;
   - [tvfinal], the final type that appears in the type
                of the state field (if present). *)

let bool2bit b =
  if b then "1" else "0"

let celltypename cell =
  use cell;
  let symbol = Invariant.symbol cell
  and holds_state = Invariant.holds_state cell in
  sprintf "%s%s_%s"
    tccell (bool2bit holds_state)
    (Symbol.print true symbol)

let celldataname cell =
  use cell;
  let symbol = Invariant.symbol cell
  and holds_state = Invariant.holds_state cell in
  sprintf "%s%s_%s"
    dccell (bool2bit holds_state)
    (Symbol.print true symbol)

let celltypedef cell =
  let symbol = Invariant.symbol cell
  and holds_state = Invariant.holds_state cell
  and holds_semv = Invariant.holds_semv cell
  and holds_startp = Invariant.holds_startp cell
  and holds_endp = Invariant.holds_endp cell in
  (* Names for type variables. *)
  let tvtail, tvfinal = "s", "r" in
  (* The data constructor definition. *)
  let datadef = {
    dataname =
      celldataname cell;
    datavalparams =
      tvar tvtail ::
      if1 holds_state (TypApp (tcstate, [ tvar tvtail; tvar tvfinal ])) @
      ifn holds_semv (semvtype symbol) @
      if1 holds_startp tposition @
      if1 holds_endp tposition ;
    datatypeparams = None;
    comment = None;
    unboxed = false;
  } in
  (* The type definition. *)
  { typename       = celltypename cell;
    typeparams     = [ tvtail ] @ if1 holds_state tvfinal;
    typerhs        = TDefSum [datadef];
    typeconstraint = None }

(* [tcell tfinal ttail cell] constructs an instance of the type associated with
   the cell [cell]. It also records the fact that this type has been used. *)

let tcell tfinal ttail cell =
  assert (Invariant.present cell);
  TypApp (
    celltypename cell,
    ttail :: if1 (Invariant.holds_state cell) tfinal
  )

(* [tstack tfinal ttail cells] constructs a stack type based on the sequence
   of cells [cells]. This type contains references to [ttail], the type of
   the remainder of the stack, and [tfinal], the final type. *)

let tstack tfinal ttail cells =
  Invariant.fold_left (tcell tfinal) ttail cells

(* -------------------------------------------------------------------------- *)

(* We also need nominal types (algebraic data types) for final types. This
   allows the OCaml type-checker to recognize the final types for two distinct
   sybmols as distinct, even if these symbols happen to have the same semantic
   value type. *)

(* There is one such type for each start symbol [nt]. *)

let finaltypename nt =
  sprintf "%s_%s" tcbox (Nonterminal.print true nt)

let finaldataname nt =
  sprintf "%s_%s" dcbox (Nonterminal.print true nt)

let finaldatadef nt =
  (* The data constructor definition. *)
  let datadef = {
    dataname       = finaldataname nt;
    datavalparams  = semvtype (Symbol.N nt);
    datatypeparams = None;
    comment        = None;
    unboxed        = true;
  } in
  (* The type definition. *)
  { typename       = finaltypename nt;
    typeparams     = [];
    typerhs        = TDefSum [datadef];
    typeconstraint = None }

let finaldatadef (nt : string) _label defs =
  let nt = Nonterminal.lookup nt in
  finaldatadef nt :: defs

let finaldatadefs =
  StringMap.fold finaldatadef program.entry []

(* -------------------------------------------------------------------------- *)

(* [compile_final tfinal final] compiles the final type [final]. If [final] is
   [Some nt], then a monomorphic final type is obtained, namely the boxed
   final type associated with the start symbol [nt]. If [final] is [None],
   which means that we are inside a polymorphic function, then the placeholder
   [final] is used. It is typically a universally quantified type variable. *)

let compile_final tfinal final =
  match final with
  | None ->
      tfinal
  | Some nt ->
      TypApp (finaltypename nt, [])

(* -------------------------------------------------------------------------- *)

(* The definition of the type [state]. *)

(* This is a GADT with two type parameters, [tvstack] and [tvfinal]. *)

let statetypedef =

  (* Names for type variables. *)
  let tvstack, tvfinal = "s", "r" in

  (* Construct the data constructors. *)
  let datadefs =
    Tag.Map.bindings program.states |>
    List.map begin fun (tag, { stack; final }) ->
      let tfinal = compile_final (tvar tvfinal) final in
      {
        dataname =
          statecon tag;
        datavalparams =
          [];
        datatypeparams =
          Some [
            tstack tfinal (tvar tvstack) stack;
            tfinal
          ];
        comment =
          Some (sprintf
            "State %s.\n        Stack shape : %s.\n        Start symbol: %s."
            (Tag.print tag)
            (Invariant.print stack)
            (StackLangPrinter.ToString.final final)
          );
        unboxed =
          false;
      }
    end
  in

  (* Construct the type definition. *)
  {
    typename =
      tcstate;
    typeparams =
      [ tvstack; tvfinal ];
    typerhs =
      TDefSum datadefs;
    typeconstraint =
      None
  }

(* -------------------------------------------------------------------------- *)

(* Two helpers for generating a call to a labeled block. *)

(* The stack is the first argument. The other arguments, listed in the set
   [needed], come in the order determined by [elements]. *)

let call callee needed =
  EApp (exlab callee, exvar stack :: exvars (elements needed))

let jump label =
  call label (lookup program label).needed

(* -------------------------------------------------------------------------- *)

(* The entry point associated with a nonterminal symbol [nt]. This is a public
   definition. The code initializes an empty stack and invokes a suitable
   [run] function, whose label is [label]. The set of registers needed by
   [label] should contain just [lexer] and [lexbuf]. The stack is implicit in
   StackLang and is not part of this set. *)

(* The function call [jump label] produces a semantic value at a boxed final
   type; we must unbox it by pattern matching on it. *)

let entrydef (nt : string) label defs =
  let lexer, lexbuf = EmitStackLang.(lexer, lexbuf) in
  let data = finaldataname (Nonterminal.lookup nt) in
  let semv = "v" in
  defpublic nt (EFun (
    [ pxvar lexer; pxvar lexbuf ],
    blet ([ pxvar stack, EUnit ],
    blet ([ PData (data, [pvar semv]), jump label ],
    evar semv
  )))) :: defs

(* -------------------------------------------------------------------------- *)

(* We generate one OCaml function for each semantic action, so as to avoid
   duplicating semantic actions. The OCaml compiler is free to inline these
   functions if it so desires. *)

(* It is preferable to declare the type of the semantic action (even though
   one might think that OCaml can infer it) because this allows type-directed
   disambiguation to take place. In 20211230, no type annotation was produced.
   As of 20220103, we annotate just the body of the semantic action with its
   result type. In principle, we could also annotate the parameters with their
   type, but that would be somewhat messy (see the function [actiondef] in the
   module Infer) and my gut feeling is that most of the time, this should not
   be necessary. *)

(* The list of all productions cannot be easily extracted from the StackLang
   program, so we obtain it by calling [Production.mapx] directly. This could
   be remedied if desired. *)

let actionparams action =
  action |> Action.vars |> StringSet.elements

(* Catch: if the semantic action takes zero parameters, then we must introduce
   one, a unit parameter. The semantic action could be an impure expression,
   so we cannot execute it unprotected. *)

let pactionparams action =
  let xs = actionparams action in
  match xs with [] -> [PUnit] | _ -> pvars xs

let eactionparams action =
  let xs = actionparams action in
  match xs with [] -> [EUnit] | _ -> evars xs

let annotate e nt =
  match Nonterminal.ocamltype nt with
  | Some ty ->
      CodeBits.annotate e (TypTextual ty)
  | None ->
      e
      (* In principle, this won't happen. We check at the beginning of [Run]
         that every nonterminal symbol has a known OCaml type. *)

let actionbody prod =
  let action = Production.action prod
  and nt = Production.nt prod in
  annotate (EComment (
    Production.print prod,
    Action.to_il_expr action
  )) nt

(* [must_not_return e msg] has the same semantics as [e] if [e] raises
   an exception or aborts the program. If [e] returns a value, then
   [must_not_return e msg] prints the message [msg] and causes a
   dynamic assertion failure. *)

let must_not_return e msg =
  ELet (
    [
      PWildcard, e;
      PUnit, EApp (EVar "prerr_string", [EStringConst msg]);
    ],
    eassert efalse
  )

(* In the simplified strategy, a production that contains the [error] token
   is not allowed to terminate normally: it must abort the parser by raising
   an exception. We check this at runtime, and if this check fails, we blame
   the user. *)

(* For the moment, this is done only here, in the new code back-end, but it
   could in principle be done uniformly for every back-end. *)

let blame prod =
  sprintf
    "Menhir: misuse: the semantic action associated with the production\n\
     %s\n\
     is expected to abort the parser, but does not do so.\n"
    (Production.print prod)

let actiondef prod =
  let action = Production.action prod in
  def (actionname prod) (
    EFun (
      pactionparams action,
      if Production.error_free prod then
        (* The usual case. *)
        actionbody prod
      else
        (* An error production is not allowed to terminate normally. *)
        must_not_return (actionbody prod) (blame prod)
  ))

let call_action prod action =
  EApp (
    evar (actionname prod),
    eactionparams action
  )

(* -------------------------------------------------------------------------- *)

(* From here on, the types that we produce are type annotations carried by
   function definitions. These functions are polymorphic in (one or) two
   types, namely [tctail] and [tcfinal]. We use OCaml's "locally abstract"
   syntax, like this:

     let id : type a . a -> a = ...

   so [tctail] and [tcfinal] are types, not type variables. *)

(* Every function is polymorphic in [tctail]. The functions that are
   reachable from only one entry point have a monomorphic result type, while
   those that are reachable from multiple entry points are polymorphic in
   [tcfinal]. *)

let tctail, tcfinal =
  tvprefix "stack", tvprefix "result"

let compile_final final =
  compile_final (tname tcfinal) final

(* [function_type tblock] produces a type scheme for the block [tblock].
   We explicitly declare the type of the stack and the type of the [state]
   register, where the universally quantified types [tctail] and [tcfinal]
   appear. We let the OCaml type-checker infer the types of the other
   arguments (which must be semantic values and positions). *)

(* The stack is the first argument. The other arguments, listed in the set
   [needed], come in the order determined by [Reg.Set.elements]. *)

(* In a naive approach, the type of the stack can be duplicated, because
   it appears both as the type of the stack itself and as a type argument
   inside the type of the current state. If [bty.stack] has length 0, then
   the type of the stack is a variable anyway, so this is not a problem.
   Otherwise, we avoid this duplication by using an OCaml [as] construct. *)

let function_type tblock : typescheme =
  let needed, bty = tblock.needed, tblock.block_type in
  let polymorphic = (bty.final = None)
  and tfinal = compile_final bty.final in
  (* The type of the stack, in its simplest form. *)
  let tstack = tstack tfinal (tname tctail) bty.stack in
  (* The first and second occurrences of the type of the stack. *)
  let tstack1, tstack2 =
    if Invariant.length bty.stack > 0 && mem state needed then
      let tvstack = "stack" in
      TypAs (tstack, tvstack),
      tvar tvstack
    else
      tstack,
      tstack
  in
  (* We declare the type of the [state] register, and let OCaml infer the
     types of all other registers. *)
  let reg r =
    if r = state then TypApp (tcstate, [ tstack2; tfinal ]) else tvar "_"
  in
  (* Construct the type scheme. *)
  local_scheme
    ([ tctail ] @ if1 polymorphic tcfinal)
    (marrow (tstack1 :: map reg (elements needed)) tfinal)

(* -------------------------------------------------------------------------- *)

(* Code production. *)

let rec compile_value v =
  match v with
  | VTag tag ->
      estatecon tag
  | VReg r ->
      exvar r
  | VUnit ->
      EUnit

and compile_values vs =
  map compile_value vs

let rec compile_pattern p =
  match p with
  | StackLang.PWildcard ->
      PWildcard
  | PReg r ->
      pxvar r

and compile_patterns ps =
  map compile_pattern ps

let compile_binding (r, v) =
  pxvar r, compile_value v

let compile_bindings bs (e : IL.expr) =
  eletand (
    map compile_binding (Bindings.to_list bs),
    e
  )

let compile_prim prim =
  match prim with
  | PrimLexerCall vs ->
      if Settings.trace then
        (* [discard lexer lexbuf] *)
        EApp (EVar discard, compile_values vs)
      else begin
        (* When [Settings.trace] is unset, [discard lexer lexbuf] boils
           down to [lexer lexbuf]. *)
        match vs with
        | [ lexer; lexbuf ] ->
            EApp (compile_value lexer, [compile_value lexbuf])
        | _ ->
            assert false
      end
  | PrimOCamlFieldAccess (v, field) ->
      ERecordAccess (compile_value v, field)
  | PrimOCamlAction (bs, prod, action) ->
      compile_bindings bs (call_action prod action)

let rec compile_block block =
  match block with

  | IPush (vs, cell, block) ->
      assert (vs <> []);
      let data =
        EData (
          celldataname cell,
          exvar stack :: compile_values vs
        )
      in
      blet ([ pxvar stack, data ], compile_block block)

  | IPop (ps, cell, block) ->
      assert (ps <> []);
      let data =
        PData (
          celldataname cell,
          pxvar stack :: compile_patterns ps
        )
      in
      blet ([ data, exvar stack ], compile_block block)

  | IPeek (ps, cell, block) ->
      assert (ps <> []);
      let data =
        PData (
          celldataname cell,
          PWildcard :: compile_patterns ps
        )
      in
      blet ([ data, exvar stack ], compile_block block)

  | IDef (bs, IJump label) ->
      (* We identify the pattern [IDef (_, IJump _)], that is, a set of
         bindings followed with a jump. We compile it nicely to OCaml by
         applying the bindings to the arguments of the function call.
         This improves readability only, not efficiency. It is optional;
         everything would still work if we did not do this. *)
      def_jump bs label

  | IDef (bs, block) ->
      compile_bindings bs (compile_block block)

  | IPrim (p, prim, block) ->
      blet (
        [ compile_pattern p, compile_prim prim ],
        compile_block block
      )

  | ITrace (message, block) ->
      (* An [ITrace] instruction is compiled to either an [eprintf]
         instruction or a comment. *)
      if Settings.trace then
        blet (
          trace message [],
          compile_block block
        )
      else
        EComment (message, compile_block block)

  | IComment (comment, block) ->
      EComment (comment, compile_block block)

  | IDead `Static ->
      (* This instruction should never be encountered. *)
      assert false

  | IDead `Dynamic ->
      call_assertfalse

  | IStop s ->
      call_stop s

  | IReturn (nt, v) ->
      (* Box the return value in a data type. This is done for type-checking
         purposes only; this data type is marked @@unboxed, so no boxing
         actually takes place at runtime. *)
      EData (finaldataname nt, [compile_value v])

  | IJump label ->
      jump label

  | ICaseToken (r, branches, odefault) ->
      compile_casetoken r branches odefault

  | ICaseTag (r, branches) ->
      compile_casetag r branches

and def_reg bs r =
  compile_value (Bindings.apply bs (VReg r))

and def_regs bs rs =
  map (def_reg bs) rs

and def_jump bs label =
  let needed = (lookup program label).needed in
  EApp (exlab label, exvar stack :: def_regs bs (elements needed))

(* In a [match] construct on a token, we run a risk that the data constructors
   of the type [token] be hidden by the user's prologue. (This has happened in
   practice.) Indeed, the type [token] is defined before the prologue appears,
   and I don't think we should change that. So, to work around this problem,
   we have two solutions: either (1) qualify every data constructor with the
   name [MenhirBasics]; or (2) annotate the scrutinee with its type, namely
   [MenhirBasics.token]; this lets OCaml perform type-based disambiguation.
   This feature has been present since OCaml 4.01, so we can rely on it. We
   choose option (2) because it is less verbose. *)

and compile_casetoken r branches odefault =
  EMatch (
    EAnnot (exvar r, stoken),
    map compile_case_token_branch branches @
    compile_default_branch odefault
  )

and compile_case_token_branch (pat, block) =
  match pat with
  | TokSingle (tok, r) ->
      branch
        (tokpat tok (pxvar r))
        (tok_bind_unit tok (pxvar r) (compile_block block))
  | TokMultiple toks ->
      branch
        (tokspat toks)
        (compile_block block)

and compile_default_branch odefault =
  match odefault with
  | None ->
      []
  | Some block ->
      [ branch PWildcard (compile_block block) ]

(* The translation of [ICaseTag] instructions into OCaml is somewhat subtle,
   because the OCaml type-checker is very fussy about dead branches. Provided
   all of the live branches are explicitly listed, OCaml allows providing an
   explicit default branch of the form [_ -> .], and it also allows omitting
   this default branch. However, it does *not* allow replacing the wildcard
   pattern in this default branch with an explicit disjunction of tags!

   I believe that we *could* in principle proceed as follows:

   1. the branches whose body is a normal instruction are listed first;
   2. the branches whose body is [IDead `Dynamic] come next;
      they are grouped as a single branch, guarded by a disjunction pattern;
   3. the branches whose body is [IDead `Static] come last,
      and can in fact remain implicit.

   However, this seems complex, verbose, fragile. It seems simpler and more
   robust to intentionally confuse [IDead `Static] and [IDead `Dynamic] and
   compile all branches in categories 2 and 3 as a single branch that is
   guarded by a wildcard pattern and causes a runtime failure. (Except in
   the case where category is empty, because in that case, such a default
   branch would be provably dead and would trigger warning 56. In that case,
   the default branch can be omitted.) *)

and compile_casetag r branches =
  (* Make every branch explicit. *)
  let branches = complete program branches in
  (* Test if there is any branch whose body is [IDead `Dynamic]. *)
  let dead_dynamic (_, body) =
    match body with IDead `Dynamic -> true | _ -> false
  in
  (* If there is, then create a default branch, whose body is a runtime
     failure instruction. The branches whose body is [IDead `Static] are
     also covered by this default branch; this is a loss of precision. *)
  (* If there is not, then create no default branch. This is equivalent
     to stating that every unlisted tag is provably dead. *)
  let default =
    if List.exists dead_dynamic branches then
      [branch PWildcard call_assertfalse]
    else
      []
  in
  (* Keep the branches whose body is not [IDead _]. *)
  let live (_, body) = match body with IDead _ -> false | _ -> true in
  let branches = List.filter live branches in
  (* Compile these branches. *)
  let branches = concat (map compile_casetag_branch branches) in
  (* Build the [match] construct. *)
  EMatch (exvar r, branches @ default)

and compile_casetag_branch (TagSingle tag, block) =
  [ tag_branch tag (compile_block block) ]

and compile_function tblock =
  EAnnot (
    EFun (
      pxvar stack :: pxvars (elements tblock.needed),
      compile_block tblock.block
    ),
    function_type tblock
  )

let def_block label =
  let tblock = lookup program label in
  def (Label.export label) (compile_function tblock)

(* -------------------------------------------------------------------------- *)

(* Arrange the blocks in strongly connected components. We do not have to do
   this (we could produce just one huge [let rec] definition), but this speeds
   up type-checking and compilation (by OCaml) by about 20%. It can also help
   produce more understandable code. *)

module G = struct
  type node = label
  let count = ref 0
  let index =
    Label.Map.map (fun _block -> Misc.postincrement count) program.cfg
  let n = !count
  let index label =
    Label.Map.find label index
  let successors yield label =
    Block.successors yield (lookup program label).block
  let iter yield =
    Label.Map.iter (fun label _block -> yield label) program.cfg
end

module S =
  Tarjan.Run(G)

let () =
  Time.tick "StackLang: computing the mutually recursive groups"

let blocks : valdef list list =
  S.map (fun _representative labels -> List.map def_block labels)

let blocks : structure_item list =
  List.rev_map (fun valdefs -> SIValDefs (true, valdefs)) blocks

let () =
  Error.logC 1 (fun f ->
    fprintf f "The StackLang code comprises %d mutually recursive groups.\n"
      (List.length blocks)
  )

(* -------------------------------------------------------------------------- *)

(* Find out which type declarations we need. This code must be executed
   after the [state] type and the type of every function have been built,
   since this construction process affects the table [used]. Thus, this
   definition must occur after [statetypedef] and after [blocks] above. *)

let celltypedefs =
  CellSet.fold (fun cell accu -> celltypedef cell :: accu) !used []

(* -------------------------------------------------------------------------- *)

(* The main program. This is the complete generated parser. *)

(* We disable the following OCaml warnings in the heart of the parser, that is,
   in the [blocks] that correspond to the StackLang program:

     "fragile match" (4)
     "unused data constructor" (37)
     "unused rec flag" (39)

   This does not affect the OCaml code fragments written by the user;
   the prelude, postlude, and semantic actions lie outside of this area.
   We could disable other warnings as well if desired. *)

open BasicSyntax

let grammar =
  Front.grammar

let program =
  [ SIFunctor (grammar.parameters,

      mbasics grammar @

      SIStretch grammar.preludes ::

      SITypeDefs (statetypedef :: celltypedefs @ finaldatadefs) ::

      valdefs (Production.mapx actiondef) @

      valdefs [ printtokendef; assertfalsedef ] @

      if1 Settings.trace (valdef discarddef) @

      SIInclude (MStruct (
        SIAttribute ("ocaml.warning", "-4-37-39") ::
        blocks
      )) ::

      valdefs (StringMap.fold entrydef program.entry []) @

      SIStretch grammar.postludes ::

      []

  )]

end (* Run *)

(* -------------------------------------------------------------------------- *)

(* Wrap up. *)

let compile program =
  let module R = Run (struct let program = program end) in
  Time.tick "StackLang: compiling down to IL";
  R.program
