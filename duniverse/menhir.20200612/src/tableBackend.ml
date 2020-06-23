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

open CodeBits
open Grammar
open IL
open Interface
open Printf
open TokenType
open NonterminalType
open CodePieces

module Run (T : sig end) = struct

(* ------------------------------------------------------------------------ *)

(* Conventional names for modules, exceptions, record fields, functions. *)

let menhirlib =
  "MenhirLib"

let make_engine_table =
  menhirlib ^ ".TableInterpreter.MakeEngineTable"

let make_engine =
  menhirlib ^ ".Engine.Make"

let make_symbol =
  menhirlib ^ ".InspectionTableInterpreter.Symbols"

let make_inspection =
  menhirlib ^ ".InspectionTableInterpreter.Make"

let engineTypes =
  menhirlib ^ ".EngineTypes"

let field x =
  engineTypes ^ "." ^ x

let fstate =
  field "state"

let fsemv =
  field "semv"

let fstartp =
  field "startp"

let fendp =
  field "endp"

let fnext =
  field "next"

let fstack =
  field "stack"

let fcurrent =
  field "current"

let entry =
  interpreter ^ ".entry"

let start =
  interpreter ^ ".start"

let staticVersion =
  menhirlib ^ ".StaticVersion"

(* The following are names of internal sub-modules. *)

let tables =
  "Tables"

let symbols =
  "Symbols"

let et =
  "ET"

let ti =
  "TI"

(* ------------------------------------------------------------------------ *)

(* Statistics. *)

(* Integer division, rounded up. *)

let div a b =
  if a mod b = 0 then a / b else a / b + 1

(* [size] provides a rough measure of the size of its argument, in words.
   The [unboxed] parameter is true if we have already counted 1 for the
   pointer to the object. *)

let rec size unboxed = function
  | EIntConst _
  | ETuple []
  | EData (_, []) ->
      if unboxed then 0 else 1
  | EStringConst s ->
      1 + div (String.length s * 8) Sys.word_size
  | ETuple es
  | EData (_, es)
  | EArray es ->
      1 + List.length es + List.fold_left (fun s e -> s + size true e) 0 es
  | _ ->
      assert false (* not implemented *)

let size =
  size false

(* Optionally, print a measure of each of the tables that we are defining. *)

let define (name, expr) = {
  valpublic = true;
  valpat = PVar name;
  valval = expr
}

let define_and_measure (x, e) =
  Error.logC 1 (fun f ->
    fprintf f
      "The %s table occupies roughly %d bytes.\n"
      x
      (size e * (Sys.word_size / 8))
  );
  define (x, e)


(* ------------------------------------------------------------------------ *)

(* Code generation for semantic actions. *)

(* The functions [reducecellparams] and [reducebody] are adapted from
   [CodeBackend]. *)

(* Things are slightly more regular here than in the code-based
   back-end, since there is no optimization: every stack cell has the
   same structure and holds a state, a semantic value, and a pair of
   positions. Because every semantic value is represented, we do not
   have a separate [unitbindings]. *)

(* [reducecellparams] constructs a pattern that describes the contents
   of a stack cell. If this is the bottom cell, the variable [state]
   is bound to the state found in the cell. If [ids.(i)] is used in
   the semantic action, then it is bound to the semantic value. The
   position variables are always bound. *)

let reducecellparams prod i _symbol (next : pattern) : pattern =

  let ids = Production.identifiers prod in

  PRecord [
    fstate, (if i = 0 then PVar state else PWildcard);
    fsemv, PVar ids.(i);
    fstartp, PVar (Printf.sprintf "_startpos_%s_" ids.(i));
    fendp, PVar (Printf.sprintf "_endpos_%s_" ids.(i));
    fnext, next;
  ]

(* The semantic values bound in [reducecellparams] have type [Obj.t].
   They should now be cast to their real type. If we had [PMagic] in
   the syntax of patterns, we could do that in one swoop; since we don't,
   we have to issue a series of casts a posteriori. *)

let reducecellcasts prod i symbol casts =

  let ids = Production.identifiers prod in
  let id = ids.(i) in
  let t : typ =
    match semvtype symbol with
    | [] ->
        tunit
    | [ t ] ->
        t
    | _ ->
        assert false
  in
  (* Cast: [let id = ((Obj.magic id) : t) in ...]. *)
  (
    PVar id,
    annotate (EMagic (EVar id)) t
  ) :: casts

(* 2015/11/04. The start and end positions of an epsilon production are obtained
   by taking the end position stored in the top stack cell (whatever it is). *)

let endpos_of_top_stack_cell =
  ERecordAccess(EVar stack, fendp)

(* This is the body of the [reduce] function associated with
   production [prod]. It assumes that the variables [env] and [stack]
   have been bound. *)

let reducebody prod =

  let nt, rhs = Production.def prod
  and ids = Production.identifiers prod
  and length = Production.length prod in

  (* Build a pattern that represents the shape of the stack. Out of
     the stack, we extract a state (except when the production is an
     epsilon production) and a number of semantic values. *)

  (* At the same time, build a series of casts. *)

  (* We want a [fold] that begins with the deepest cells in the stack.
     Folding from left to right on [rhs] is appropriate. *)

  let (_ : int), pat, casts =
    Array.fold_left (fun (i, pat, casts) symbol ->
      i + 1,
      reducecellparams prod i symbol pat,
      reducecellcasts prod i symbol casts
    ) (0, PVar stack, []) rhs
  in

  (* Determine beforeend/start/end positions for the left-hand side of the
     production, and bind them to the conventional variables [beforeendp],
     [startp], and [endp]. These variables may be unused by the semantic
     action, in which case these bindings are dead code and can be ignored
     by the OCaml compiler. *)

  let posbindings =
    ( PVar beforeendp,
      endpos_of_top_stack_cell
    ) ::
    ( PVar startp,
      if length > 0 then
        EVar (Printf.sprintf "_startpos_%s_" ids.(0))
      else
        endpos_of_top_stack_cell
    ) ::
    ( PVar endp,
      if length > 0 then
        EVar (Printf.sprintf "_endpos_%s_" ids.(length - 1))
      else
        EVar startp
    ) :: []
  in

  (* This cannot be one of the start productions. *)
  assert (not (Production.is_start prod));

  (* This is a regular production. Perform a reduction. *)

  let action =
    Production.action prod
  in
  let act =
    annotate (Action.to_il_expr action) (semvtypent nt)
  in

  EComment (
    Production.print prod,
    blet (
      (pat, EVar stack) ::                  (* destructure the stack *)
      casts @                               (* perform type casts *)
      posbindings @                         (* bind [startp] and [endp] *)
      [ PVar semv, act ],                   (* run the user's code and bind [semv] *)

      (* Return a new stack, onto which we have pushed a new stack cell. *)

      ERecord [                             (* the new stack cell *)
        fstate, EVar state;                 (* the current state after popping; it will be updated by [goto] *)
        fsemv, ERepr (EVar semv);           (* the newly computed semantic value *)
        fstartp, EVar startp;               (* the newly computed start and end positions *)
        fendp, EVar endp;
        fnext, EVar stack;                  (* this is the stack after popping *)
      ]

    )
  )

let semantic_action prod =
  EFun (
    [ PVar env ],

    (* Access the stack and current state via the environment. *)

    (* In fact, the current state needs be bound here only if this is
       an epsilon production. Otherwise, the variable [state] will be
       bound by the pattern produced by [reducecellparams] above. *)

    ELet (

      [ PVar stack, ERecordAccess (EVar env, fstack) ] @
        (if Production.length prod = 0 then [ PVar state, ERecordAccess (EVar env, fcurrent) ] else []),

      reducebody prod

    )
  )

(* Export the number of start productions. *)

let start_def =
  define (
    "start",
    EIntConst Production.start
  )

(* ------------------------------------------------------------------------ *)

(* Table encodings. *)

(* Encodings of entries in the default reduction table. *)

let encode_DefRed prod =            (* 1 + prod *)
  1 + Production.p2i prod

let encode_NoDefRed =               (* 0 *)
  0

(* Encodings of entries in the action table. *)

let encode_Reduce prod =            (* prod | 01 *)
  (Production.p2i prod lsl 2) lor 1

let encode_ShiftDiscard s =         (*    s | 10 *)
  ((Lr1.number s) lsl 2) lor 0b10

let encode_ShiftNoDiscard s =       (*    s | 11 *)
  ((Lr1.number s) lsl 2) lor 0b11

let encode_Fail =                   (*        00 *)
  0

(* Encodings of entries in the goto table. *)

let encode_Goto node =              (* 1 + node *)
  1 + Lr1.number node

let encode_NoGoto =                 (* 0 *)
  0

(* Encodings of the hole in the action and goto tables. *)

let hole =
  assert (encode_Fail = 0);
  assert (encode_NoGoto = 0);
  0

(* Encodings of entries in the error bitmap. *)

let encode_Error =                  (* 0 *)
  0

let encode_NoError =                (* 1 *)
  1

(* Encodings of terminal and nonterminal symbols in the production table. *)

let encode_no_symbol =
  0                                          (* 0 | 0 *)

let encode_terminal tok =
  (Terminal.t2i tok + 1) lsl 1          (*  t + 1 | 0 *)

let encode_nonterminal nt =
  ((Nonterminal.n2i nt) lsl 1) lor 1        (* nt | 1 *)

let encode_symbol = function
  | Symbol.T tok ->
      encode_terminal tok
  | Symbol.N nt ->
      encode_nonterminal nt

let encode_symbol_option = function
  | None ->
      encode_no_symbol
  | Some symbol ->
      encode_symbol symbol

(* Encoding a Boolean as an integer value. *)

let encode_bool b =
  if b then 1 else 0

(* ------------------------------------------------------------------------ *)

(* Table compression. *)

(* Our sparse, two-dimensional tables are turned into one-dimensional tables
   via [RowDisplacement]. *)

(* The error bitmap, which is two-dimensional but not sparse, is made
   one-dimensional by simple flattening. *)

(* Every one-dimensional table is then packed via [PackedIntArray]. *)

(* Optionally, we print some information about the compression ratio. *)

(* [population] counts the number of significant entries in a
   two-dimensional matrix. *)

let population (matrix : int array array) =
  Array.fold_left (fun population row ->
    Array.fold_left (fun population entry ->
      if entry = hole then population else population + 1
    ) population row
  ) 0 matrix

(* [marshal1] marshals a one-dimensional array. *)

let marshal1 (table : int array) =
  let (bits : int), (text : string) = MenhirLib.PackedIntArray.pack table in
  ETuple [ EIntConst bits; EStringConst text ]

(* [marshal11] marshals a one-dimensional array whose bit width is
   statically known to be [1]. *)

let marshal11 (table : int array) =
  let (bits : int), (text : string) = MenhirLib.PackedIntArray.pack table in
  assert (bits = 1);
  EStringConst text

(* List-based versions of the above functions. *)

let marshal1_list (table : int list) =
  marshal1 (Array.of_list table)

let marshal11_list (table : int list) =
  marshal11 (Array.of_list table)

(* [linearize_and_marshal1] marshals an array of integer arrays (of possibly
   different lengths). *)

let linearize_and_marshal1 (table : int array array) =
  let data, entry = MenhirLib.LinearizedArray.make table in
  ETuple [ marshal1 data; marshal1 entry ]

(* [flatten_and_marshal11_list] marshals a two-dimensional bitmap,
   whose width (for now) is assumed to be [Terminal.n - 1]. *)

let flatten_and_marshal11_list (table : int list list) =
  ETuple [
    (* Store the table width. *)
    EIntConst (Terminal.n - 1);
    (* View the table as a one-dimensional array, and marshal it. *)
    marshal11_list (List.flatten table)
  ]

(* [marshal2] marshals a two-dimensional table, with row displacement. *)

let marshal2 name m n (matrix : int list list) =
  let matrix : int array array =
    Array.of_list (List.map Array.of_list matrix)
  in
  let (displacement : int array), (data : int array) =
    MenhirLib.RowDisplacement.compress
      (=)
      (fun x -> x = hole)
      hole
      m
      n
      matrix
  in
  Error.logC 1 (fun f ->
    fprintf f
      "The %s table is %d entries; %d non-zero; %d compressed.\n"
      name
      (m * n)
      (population matrix)
      (Array.length displacement + Array.length data)
  );
  ETuple [
    marshal1 displacement;
    marshal1 data;
  ]

(* ------------------------------------------------------------------------ *)

(* Table generation. *)

(* The action table. *)

let action node t =
  match Default.has_default_reduction node with
  | Some _ ->

      (* [node] has a default reduction; in that case, the action
         table is never looked up. *)

      hole

  | None ->

      try
        let target = SymbolMap.find (Symbol.T t) (Lr1.transitions node) in

        (* [node] has a transition to [target]. If [target] has a default
           reduction on [#], use [ShiftNoDiscard], otherwise [ShiftDiscard]. *)

        match Default.has_default_reduction target with
        | Some (_, toks) when TerminalSet.mem Terminal.sharp toks ->
            assert (TerminalSet.cardinal toks = 1);
            encode_ShiftNoDiscard target
        | _ ->
            encode_ShiftDiscard target

      with Not_found ->
        try

          (* [node] has a reduction. *)

          let prod = Misc.single (TerminalMap.find t (Lr1.reductions node)) in
          encode_Reduce prod

        with Not_found ->

          (* [node] has no action. *)

          encode_Fail

(* In the error bitmap and in the action table, the row that corresponds to the
   [#] pseudo-terminal is never accessed. Thus, we do not create this row. This
   does not create a gap in the table, because this is the right-most row. For
   sanity, we check this fact here. *)

let () =
  assert (Terminal.t2i Terminal.sharp = Terminal.n - 1)

(* The goto table. *)

let goto node nt =
  try
    let target = SymbolMap.find (Symbol.N nt) (Lr1.transitions node) in
    encode_Goto target
  with Not_found ->
    encode_NoGoto

(* The error bitmap reflects which entries in the action table are
   [Fail]. Like the action table, it is not accessed when [node] has a
   default reduction. *)

let error node t =
  if action node t = encode_Fail then
    encode_Error
  else
    encode_NoError

(* The default reductions table. *)

let default_reduction node =
  match Default.has_default_reduction node with
  | Some (prod, _) ->
      encode_DefRed prod
  | None ->
      encode_NoDefRed

(* Generate the table definitions. *)

let action =
  define_and_measure (
    "action",
    marshal2 "action" Lr1.n (Terminal.n - 1) (
      Lr1.map (fun node ->
        Terminal.mapx (fun t ->
          action node t
        )
      )
    )
  )

let goto =
  define_and_measure (
    "goto",
    marshal2 "goto" Lr1.n Nonterminal.n (
      Lr1.map (fun node ->
        Nonterminal.map (fun nt ->
          goto node nt
        )
      )
    )
  )

let error =
  define_and_measure (
    "error",
    flatten_and_marshal11_list (
      Lr1.map (fun node ->
        Terminal.mapx (fun t ->
          error node t
        )
      )
    )
  )

let default_reduction =
  define_and_measure (
    "default_reduction",
    marshal1_list (
      Lr1.map (fun node ->
        default_reduction node
      )
    )
  )

let lhs =
  define_and_measure (
    "lhs",
    marshal1 (
      Production.amap (fun prod ->
        Nonterminal.n2i (Production.nt prod)
      )
    )
  )

let semantic_action =
  define (
    "semantic_action",
    (* Non-start productions only. *)
    EArray (Production.mapx semantic_action)
  )

(* ------------------------------------------------------------------------ *)

(* When [--trace] is enabled, we need tables that map terminals and
   productions to strings. *)

let stringwrap f x =
  EStringConst (f x)

let reduce_or_accept prod =
  match Production.classify prod with
  | Some _ ->
      "Accepting"
  | None ->
      "Reducing production " ^ (Production.print prod)

let trace =
  define_and_measure (
    "trace",
    if Settings.trace then
      EData ("Some", [
        ETuple [
          EArray (Terminal.map (stringwrap Terminal.print));
          EArray (Production.map (stringwrap reduce_or_accept));
        ]
      ])
    else
      EData ("None", [])
  )

(* ------------------------------------------------------------------------ *)

(* Generate the two functions that map a token to its integer code and to
   its semantic value, respectively. *)

let token2terminal =
  destructuretokendef
    "token2terminal"
    tint
    false
    (fun tok -> EIntConst (Terminal.t2i tok))

let token2value =
  destructuretokendef
    "token2value"
    tobj
    true
    (fun tok ->
      ERepr (
        match Terminal.ocamltype tok with
        | None ->
            EUnit
        | Some _ ->
            EVar semv
      )
    )

(* ------------------------------------------------------------------------ *)

(* The client APIs invoke the interpreter with an appropriate start state.
   The monolithic API calls [entry] (see [Engine]), while the incremental
   API calls [start]. *)

(* An entry point to the monolithic API. *)

let monolithic_entry_point state nt t =
  define (
    Nonterminal.print true nt,
    let lexer = "lexer"
    and lexbuf = "lexbuf" in
    EFun (
      [ PVar lexer; PVar lexbuf ],
      annotate (
        EMagic (
          EApp (
            EVar entry, [
              EIntConst (Lr1.number state);
              EVar lexer;
              EVar lexbuf
            ]
          )
        )
      )
      (TypTextual t)
    )
  )

(* The whole monolithic API. *)

let monolithic_api : IL.valdef list =
  Lr1.fold_entry (fun _prod state nt t api ->
    monolithic_entry_point state nt t ::
    api
  ) []

(* An entry point to the incremental API. *)

let incremental_entry_point state nt t =
  let initial = "initial_position" in
  define (
    Nonterminal.print true nt,
    (* In principle the eta-expansion [fun initial_position -> start s
       initial_position] should not be necessary, since [start] is a pure
       function. However, when [--trace] is enabled, [start] will log messages
       to the standard error channel. *)
    EFun (
      [ PVar initial ],
      annotate (
        EMagic (
          EApp (
            EVar start, [
              EIntConst (Lr1.number state);
              EVar initial;
            ]
          )
        )
      )
      (checkpoint (TypTextual t))
    )
  )

(* The whole incremental API. *)

let incremental_api : IL.valdef list =
  Lr1.fold_entry (fun _prod state nt t api ->
    incremental_entry_point state nt t ::
    api
  ) []

(* ------------------------------------------------------------------------ *)

(* Constructing representations of symbols. *)

(* [eterminal t] is a value of type ['a terminal] (for some ['a]) that
   encodes the terminal symbol [t]. It is just a data constructor of
   the terminal GADT. *)

let eterminal (t : Terminal.t) : expr =
  EData (tokengadtdata (Terminal.print t), [])

(* [enonterminal nt] is a value of type ['a nonterminal] (for some
   ['a]) that encodes the nonterminal symbol [nt]. It is just a data
   constructor of the nonterminal GADT. *)

let enonterminal (nt : Nonterminal.t) : expr =
  EData (tnonterminalgadtdata (Nonterminal.print false nt), [])

(* [esymbol symbol] is a value of type ['a symbol] (for some ['a])
   that encodes the symbol [symbol]. It is built by applying the
   injection [T] or [N] to the terminal or nonterminal encoding. *)

let dataT =
  "T"

let dataN =
  "N"

let esymbol (symbol : Symbol.t) : expr =
  match symbol with
  | Symbol.T t ->
      EData (dataT, [ eterminal t ])
  | Symbol.N nt ->
      EData (dataN, [ enonterminal nt ])

(* [xsymbol symbol] is a value of type [xsymbol] that encodes the
   symbol [symbol]. It is built by applying the injection [X] (an
   existential quantifier) to [esymbol symbol]. *)

let dataX =
  "X"

let xsymbol (symbol : Symbol.t) : expr =
  EData (dataX, [ esymbol symbol ])

(* ------------------------------------------------------------------------ *)

(* Produce a function that maps a terminal symbol (represented as an integer
   code) to its representation as an [xsymbol]. Include [error] but not [#],
   i.e., include all of the symbols which can appear in a production. *)

(* Note that, instead of generating a function, we could (a) use an array
   or (b) use an unsafe conversion of an integer to a data constructor,
   then wrap it using [X] and [T/N]. Approach (b) is unsafe and causes
   memory allocation (due to the wrapping) at each call. *)

let terminal () =
  assert Settings.inspection;
  let t = "t" in
  define (
    "terminal",
    EFun ([ PVar t ],
      EMatch (EVar t,
        Terminal.mapx (fun tok ->
          { branchpat = pint (Terminal.t2i tok);
            branchbody = xsymbol (Symbol.T tok) }
        ) @ [
          { branchpat = PWildcard;
            branchbody =
              EComment ("This terminal symbol does not exist.",
                EApp (EVar "assert", [ efalse ])
              ) }
        ]
      )
    )
  )

(* ------------------------------------------------------------------------ *)

(* Produce a function that maps a (non-start) nonterminal symbol (represented
   as an integer code) to its representation as an [xsymbol]. *)

let nonterminal () =
  assert Settings.inspection;
  let nt = "nt" in
  define (
    "nonterminal",
    EFun ([ PVar nt ],
      EMatch (EVar nt,
        Nonterminal.foldx (fun nt branches ->
          { branchpat = pint (Nonterminal.n2i nt);
            branchbody = xsymbol (Symbol.N nt) } :: branches
        ) [
          { branchpat = PWildcard;
            branchbody =
              EComment ("This nonterminal symbol does not exist.",
                EApp (EVar "assert", [ efalse ])
              ) }
        ]
      )
    )
  )

(* ------------------------------------------------------------------------ *)

(* Produce a mapping of every LR(0) state to its incoming symbol (encoded as
   an integer value). (Note that the initial states do not have one.) *)

let lr0_incoming () =
  assert Settings.inspection;
  define_and_measure (
    "lr0_incoming",
    marshal1 (Array.init Lr0.n (fun node ->
      encode_symbol_option (Lr0.incoming_symbol node)
    ))
  )

(* ------------------------------------------------------------------------ *)

(* A table that maps a production (i.e., an integer index) to the production's
   right-hand side. In principle, we use this table for ordinary productions
   only, as opposed to the start productions, whose existence is not exposed
   to the user. However, it is simpler (and not really costly) to include all
   productions in this table. *)

let rhs () =
  assert Settings.inspection;
  let productions : int array array =
    Production.amap (fun prod ->
      Array.map encode_symbol (Production.rhs prod)
    )
  in
  define_and_measure (
    "rhs",
    linearize_and_marshal1 productions
  )

(* ------------------------------------------------------------------------ *)

(* A table that maps an LR(1) state to its LR(0) core. *)

let lr0_core () =
  assert Settings.inspection;
  define_and_measure (
    "lr0_core",
    marshal1_list (Lr1.map (fun (node : Lr1.node) ->
      Lr0.core (Lr1.state node)
    ))
  )

(* A table that maps an LR(0) state to a set of LR(0) items. *)

let lr0_items () =
  assert Settings.inspection;
  let items : int array array =
    Array.init Lr0.n (fun node ->
      Array.map Item.marshal (Array.of_list (Item.Set.elements (Lr0.items node)))
    )
  in
  define_and_measure (
    "lr0_items",
    linearize_and_marshal1 items
  )

(* ------------------------------------------------------------------------ *)

(* A table that tells which nonterminal symbols are nullable.
   (For simplicity, this table includes the start symbols.) *)

let nullable () =
  assert Settings.inspection;
  define_and_measure (
    "nullable",
    marshal11_list (
      Nonterminal.map (fun nt ->
        encode_bool (Analysis.nullable nt)
      )
    )
  )

(* ------------------------------------------------------------------------ *)

(* A two-dimensional bitmap, indexed first by nonterminal symbols, then by
   terminal symbols, encodes the FIRST sets. *)

let first () =
  assert Settings.inspection;
  define_and_measure (
    "first",
    flatten_and_marshal11_list (
      Nonterminal.map (fun nt ->
        Terminal.mapx (fun t ->
          encode_bool (TerminalSet.mem t (Analysis.first nt))
        )
      )
    )
  )

(* ------------------------------------------------------------------------ *)

(* A reference to [MenhirLib.StaticVersion.require_XXXXXXXX], where [XXXXXXXX]
   is our 8-digit version number. This ensures that the generated code can be
   linked only with an appropriate version of MenhirLib. This is important
   because we use unsafe casts, and a version mismatch could cause a crash. *)

let versiondef = {
  valpublic = true;
  valpat = PUnit;
  valval = EVar (staticVersion ^ ".require_" ^ Version.version);
}

(* ------------------------------------------------------------------------ *)

(* Let's put everything together. *)

open BasicSyntax

let grammar =
  Front.grammar

let program =

  [ SIFunctor (grammar.parameters,

    (* Make a reference to [MenhirLib.StaticVersion.require_XXXXXXXX], where
       [XXXXXXXX] is our 8-digit version number. This ensures that the
       generated code can be linked only with an appropriate version of
       MenhirLib. This is important because we use unsafe casts, and a
       version mismatch could cause a crash. *)

    SIComment "This generated code requires the following version of MenhirLib:" ::
    SIValDefs (false, [ versiondef ]) ::

    (* Define the internal sub-module [basics], which contains the definitions
       of the exception [Error] and of the type [token]. Then, include this
       sub-module. This sub-module is used again below, as part of the
       application of the functor [TableInterpreter.Make]. *)

    mbasics grammar @

    (* In order to avoid hiding user-defined identifiers, only the
       exception [Error] and the type [token] should be defined (at
       top level, with non-mangled names) above this line. We also
       define the value [_eRR] above this line so that we do not
       have a problem if a user prelude hides the name [Error]. *)

    SIStretch grammar.preludes ::

    (* Define the tables. *)

    SIModuleDef (tables,
      MStruct [
        (* The internal sub-module [basics] contains the definitions of the
           exception [Error] and of the type [token]. *)
        SIInclude (MVar basics);

        (* This is a non-recursive definition, so none of the names
           defined here are visible in the semantic actions. *)
        SIValDefs (false, [
          token2terminal;
          define ("error_terminal", EIntConst (Terminal.t2i Terminal.error));
          token2value;
          default_reduction;
          error;
          start_def;
          action;
          lhs;
          goto;
          semantic_action;
          trace;
        ])
      ]
    ) ::

    SIModuleDef (interpreter, MStruct (

      (* Apply the functor [TableInterpreter.MakeEngineTable] to the tables. *)
      SIModuleDef (et, MApp (MVar make_engine_table, MVar tables)) ::
      (* Apply the functor [Engine.Make] to obtain an engine. *)
      SIModuleDef (ti, MApp (MVar make_engine, MVar et)) ::
      SIInclude (MVar ti) ::

      listiflazy Settings.inspection (fun () ->

        (* Define the internal sub-module [symbols], which contains type
           definitions. Then, include this sub-module. This sub-module is used
           again below, as part of the application of the functor
           [TableInterpreter.MakeInspection]. *)

        SIModuleDef (symbols, MStruct (
          interface_to_structure (
            tokengadtdef grammar @
            nonterminalgadtdef grammar
          )
        )) ::

        SIInclude (MVar symbols) ::

        (* Apply the functor [InspectionTableInterpreter.Make], which expects
           four arguments. *)
        SIInclude (mapp (MVar make_inspection) [
          (* Argument 1, of type [TableFormat.TABLES]. *)
          MVar tables;
          (* Argument 2, of type [InspectionTableFormat.TABLES]. *)
          MStruct (
            (* [lr1state] *)
            SIInclude (MVar ti) ::
            (* [terminal], [nonterminal]. *)
            SIInclude (MVar symbols) ::
            (* This functor application builds the types [symbol] and [xsymbol]
               in terms of the types [terminal] and [nonterminal]. This saves
               us the trouble of generating these definitions. *)
            SIInclude (MApp (MVar make_symbol, MVar symbols)) ::
            SIValDefs (false,
              terminal() ::
              nonterminal() ::
              lr0_incoming() ::
              rhs() ::
              lr0_core() ::
              lr0_items() ::
              nullable() ::
              first() ::
              []
            ) ::
            []
          );
          (* Argument 3, of type [EngineTypes.TABLE]. *)
          MVar et;
          (* Argument 4, of type [EngineTypes.ENGINE with ...]. *)
          MVar ti;
        ]) ::

        []

      )

    )) ::

    SIValDefs (false, monolithic_api) ::

    SIModuleDef (incremental, MStruct [
      SIValDefs (false, incremental_api)
    ]) ::

    SIStretch grammar.postludes ::

  [])]

let () =
  Time.tick "Producing abstract syntax"

end
