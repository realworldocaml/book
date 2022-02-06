(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open BasicSyntax
open Syntax
open Positions

module Make (G : sig

  (* An abstract syntax tree for the grammar. *)
  val grammar: BasicSyntax.grammar

  (* This flag indicates whether it is OK to produce warnings, verbose
     information, etc., when this functor is invoked. If it is set to
     [false], then only serious errors can be signaled. *)
  val verbose: bool

end) () = struct

  open G

(* ------------------------------------------------------------------------ *)

(* [index] indexes a list of (distinct) strings, that is, assigns an
   integer index to each string and builds mappings both ways between
   strings and indices. *)

let index (strings : string list) : int * string array * int StringMap.t =
  let name = Array.of_list strings
  and n, map = List.fold_left (fun (n, map) s ->
    n+1, StringMap.add s n map
  ) (0, StringMap.empty) strings in
  n, name, map

(* ------------------------------------------------------------------------ *)
(* Precedence levels for tokens or pseudo-tokens alike. *)

module TokPrecedence = struct

  (* This set records, on a token by token basis, whether the token's
     precedence level is ever useful. This allows emitting warnings
     about useless precedence declarations. *)

  let ever_useful : StringSet.t ref =
    ref StringSet.empty

  let use id =
    ever_useful := StringSet.add id !ever_useful

  (* This function is invoked when someone wants to consult a token's
     precedence level. This does not yet mean that this level is
     useful, though. Indeed, if it is subsequently compared against
     [UndefinedPrecedence], it will not allow solving a conflict. So,
     in addition to the desired precedence level, we return a delayed
     computation which, when evaluated, records that this precedence
     level was useful. *)

  let levelip id properties =
    lazy (use id), properties.tk_precedence

  let leveli id =
    let properties =
      try
        StringMap.find id grammar.tokens
      with Not_found ->
        assert false (* well-formedness check has been performed earlier *)
    in
    levelip id properties

  (* This function prints warnings about useless precedence declarations
     for terminal symbols (%left, %right, %nonassoc). It should be invoked
     after only the automaton has been constructed. *)

  let diagnostics () =
    StringMap.iter (fun id properties ->
      if not (StringSet.mem id !ever_useful) then
        match properties.tk_precedence with
        | UndefinedPrecedence ->
            ()
        | PrecedenceLevel (_, _, pos1, pos2) ->
            Error.grammar_warning [Positions.import (pos1, pos2)]
              "the precedence level assigned to %s is never useful." id
    ) grammar.tokens

end

(* ------------------------------------------------------------------------ *)
(* Nonterminals. *)

module Nonterminal = struct

  type t = int

  let n2i i = i
  let i2n i = i

  let equal (nt1 : t) (nt2 : t) =
    nt1 = nt2

  let compare = (-)

  (* Determine how many nonterminals we have and build mappings
     both ways between names and indices. A new nonterminal is
     created for every start symbol. *)

  let new_start_nonterminals =
    StringSet.fold (fun symbol ss -> (symbol ^ "'") :: ss) grammar.start_symbols []

  let original_nonterminals =
    nonterminals grammar

  let start =
    List.length new_start_nonterminals

  let (n : int), (name : string array), (map : int StringMap.t) =
    index (new_start_nonterminals @ original_nonterminals)

  let () =
    if verbose then
      Error.logG 1 (fun f ->
        Printf.fprintf f
          "Grammar has %d nonterminal symbols, among which %d start symbols.\n"
          (n - start) start
      )

  let is_internal_start nt =
    nt < start

  let is_user_start nt =
    StringSet.mem name.(nt) grammar.start_symbols

  let print normalize nt =
    if normalize then
      Misc.normalize name.(nt)
    else
      name.(nt)

  let lookup name =
    StringMap.find name map

  let positions nt =
    (StringMap.find (print false nt) grammar.rules).positions

  let init f =
    Array.init n f

  let iter f =
    Misc.iteri n f

  let fold f accu =
    Misc.foldi n f accu

  let map f =
    Misc.mapi n f

  let iterx f =
    for nt = start to n - 1 do
      f nt
    done

  let foldx f accu =
    Misc.foldij start n f accu

  let ocamltype nt =
    assert (not (is_internal_start nt));
    try
      Some (StringMap.find (print false nt) grammar.types)
    with Not_found ->
      None

  let ocamltype_of_start_symbol nt =
    match ocamltype nt with
    | Some typ ->
        typ
    | None ->
        (* Every start symbol has a type. *)
        assert false

  let symbols_without_ocamltype () =
    foldx (fun nt accu ->
      match ocamltype nt with
      | Some _ ->
          accu
      | None ->
          nt :: accu
    ) []

  let tabulate f =
    Array.get (Array.init n f)

  let attributes : Syntax.attributes array =
    Array.make n []

  let () =
    StringMap.iter (fun nonterminal { attributes = attrs } ->
      let nt = lookup nonterminal in
      attributes.(nt) <- attrs
    ) grammar.rules

  let attributes nt =
    attributes.(nt)

end

(* Sets and maps over nonterminals. *)

module NonterminalMap = Patricia.Big

module NonterminalSet = Patricia.Big.Domain

(* ------------------------------------------------------------------------ *)
(* Terminals. *)

module Terminal = struct

  type t = int

  let t2i i = i
  let i2t i = i

  let compare = (-)

  let equal (tok1 : t) (tok2 : t) =
    tok1 = tok2

  (* 2021/11/23 If a token is named [Error], warn. This is undesirable,
     as it creates a collision in the public interface of the generated
     parser between the token [Error] and the exception [Error]. OCaml
     itself may warn about this collision. *)
  let () =
    if verbose then try
      let properties = StringMap.find "Error" grammar.tokens in
      if properties.tk_is_declared then
        let pos = properties.tk_position in
        Error.grammar_warning [pos]
          "please do not name a terminal symbol Error."
    with Not_found ->
      ()

  (* Determine how many terminals we have and build mappings
     both ways between names and indices. A new terminal "#"
     is created. A new terminal "error" is created. The fact
     that the integer code assigned to the "#" pseudo-terminal
     is the last one is exploited in the table-based back-end.
     (The right-most row of the action table is not created.)

     Pseudo-tokens (used in %prec declarations, but never
     declared using %token) are filtered out. *)

  (* In principle, the number of the [error] token is irrelevant.
     It is currently 0, but we do not rely on that. *)

  let (n : int), (name : string array), (map : int StringMap.t) =
    let tokens = tokens grammar in
    match tokens with
    | [] when verbose ->
        Error.error [] "no tokens have been declared."
    | _ ->
        index ("error" :: tokens @ [ "#" ])

  let print tok =
    name.(tok)

  let lookup name =
    StringMap.find name map

  let sharp =
    lookup "#"

  let error =
    lookup "error"

  let pseudo tok =
    (tok = sharp) || (tok = error)

  let real t =
    error <> t && t <> sharp

  let non_error tok =
    tok <> error

  let token_properties =
    let not_so_dummy_properties = (* applicable to [error] and [#] *)
      {
        tk_filename      = "__primitives__";
        tk_precedence    = UndefinedPrecedence;
        tk_associativity = UndefinedAssoc;
        tk_ocamltype     = None;
        tk_is_declared   = true;
        tk_position      = Positions.dummy;
        tk_attributes    = [];
        tk_alias         = None;
      }
    in
    Array.init n (fun tok ->
      try
         StringMap.find name.(tok) grammar.tokens
       with Not_found ->
         assert (tok = sharp || tok = error);
         not_so_dummy_properties
    )

  let () =
    if verbose then
      Error.logG 1 (fun f ->
        Printf.fprintf f "Grammar has %d terminal symbols.\n" (n - 2)
      )

  let precedence_level tok =
    TokPrecedence.levelip (print tok) token_properties.(tok)

  let associativity tok =
    token_properties.(tok).tk_associativity

  let ocamltype tok =
    token_properties.(tok).tk_ocamltype

  let init f =
    Array.init n f

  let iter f =
    Misc.iteri n f

  let fold f accu =
    Misc.foldi n f accu

  let map f =
    Misc.mapi n f

  let () =
    assert (sharp = n - 1)
  let foldx f accu =
    Misc.foldi sharp f accu
  let mapx f =
    Misc.mapi sharp f

  let () =
    assert (error = 0)
  let iter_real f =
    for i = 1 to n-2 do
      f i
    done

  let tokens_without_an_alias =
    let accu = ref [] in
    iter_real begin fun tok ->
      let properties = token_properties.(tok) in
      if properties.tk_alias = None then
        accu := tok :: !accu
    end;
    List.rev !accu

  let () =
    if verbose && Settings.require_aliases then
      tokens_without_an_alias |> List.iter begin fun tok ->
        let properties = token_properties.(tok) in
        let pos = properties.tk_position in
        Error.grammar_warning [pos]
          "no alias has been defined for the token %s."
          (print tok)
      end

  let every_token_has_an_alias =
    tokens_without_an_alias = []

  let alias tok =
    token_properties.(tok).tk_alias

  let unquoted_alias tok =
    alias tok |> Option.map (fun qid ->
      assert (qid.[0] = '"');
      (* Skip the opening quote and decode the remainder using the lexer
         [decode_string]. *)
      let qid = String.sub qid 1 (String.length qid - 1) in
      let lexbuf = Lexing.from_string qid in
      Misc.with_buffer 8 (fun b -> Lexer.decode_string b lexbuf)
  )

  let print_concrete t =
  match unquoted_alias t with
  | Some alias ->
      alias
  | None ->
      (* An alias is missing. Use the abstract name of the terminal
         instead. This is a best effort. *)
      print t

  (* If a token named [EOF] exists, then it is assumed to represent
     ocamllex's [eof] pattern. *)

  let eof =
    try
      Some (lookup "EOF")
    with Not_found ->
      None

  let attributes tok =
    token_properties.(tok).tk_attributes

  (* The sub-module [Word] offers an implementation of words (that is,
     sequences) of terminal symbols. It is used by [LRijkstra]. We
     make it a functor, because it has internal state (a hash table)
     and a side effect (failure if there are more than 256 terminal
     symbols). *)

  module Word (X : sig end) = struct

    (* We could use lists, or perhaps the sequences offered by the module
       [Seq], which support constant time concatenation. However, we need a
       much more compact representation: [LRijkstra] stores tens of millions
       of such words. We use strings, because they are very compact (8 bits
       per symbol), and on top of that, we use a hash-consing facility. In
       practice, hash-consing allows us to save 1000x in space. *)

    (* A drawback of this approach is that it works only if the number of
       terminal symbols is at most 256. For the moment, this is good enough.
       [LRijkstra] already has difficulty at 100 terminal symbols or so. *)

    let () =
      assert (n <= 256)

    let (encode : string -> int), (decode : int -> string), verbose =
      Misc.new_encode_decode 1024

    type word =
      int

    let epsilon =
      encode ""

    let singleton t =
      encode (String.make 1 (Char.chr t))

    let append i1 i2 =
      let w1 = decode i1
      and w2 = decode i2 in
      if String.length w1 = 0 then
        i2
      else if String.length w2 = 0 then
        i1
      else
        encode (w1 ^ w2)

    let length i =
      String.length (decode i)

    let first i z =
      let w = decode i in
      if String.length w > 0 then
        Char.code w.[0]
      else
        z

    let rec elements i n w =
      if i = n then
        []
      else
        Char.code w.[i] :: elements (i + 1) n w

    let elements i =
      let w = decode i in
      elements 0 (String.length w) w

    let print i =
      let w = decode i in
      Misc.separated_iter_to_string
        (fun c -> print (Char.code c))
        " "
        (fun f -> String.iter f w)

    (* [Generic.compare] implements a lexicographic ordering on strings. *)
    let compare i1 i2 =
      Generic.compare (decode i1) (decode i2)

  end

end

(* Sets of terminals are used intensively in the LR(1) construction,
   so it is important that they be as efficient as possible. *)

module TerminalSet = struct

  (* 2020/01/29: use [BoundedBitSet] instead of [SparseBitSet]. *)

  include BoundedBitSet.Make(Terminal)()

  let print toks =
    Misc.separated_iter_to_string Terminal.print " " (fun f -> iter f toks)

  let universe =
    remove Terminal.sharp (
      remove Terminal.error (
        Terminal.fold add empty
      )
    )

  (* The following definitions are used in the computation of FIRST sets
     below. They are not exported outside of this file. *)

  type property =
    t

  let bottom =
    empty

  let is_maximal _ =
    false

  let leq_join =
    union

end

(* Maps over terminals. *)

module TerminalMap = Patricia.Big

(* ------------------------------------------------------------------------ *)
(* Symbols. *)

module Symbol = struct

  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  let is_terminal sym =
    match sym with
    | N _ ->
        false
    | T _ ->
        true

  let compare sym1 sym2 =
    match sym1, sym2 with
    | N nt1, N nt2 ->
        Nonterminal.compare nt1 nt2
    | T tok1, T tok2 ->
        Terminal.compare tok1 tok2
    | N _, T _ ->
        1
    | T _, N _ ->
        -1

  let equal sym1 sym2 =
    compare sym1 sym2 = 0

  let rec lequal syms1 syms2 =
    match syms1, syms2 with
    | [], [] ->
        true
    | sym1 :: syms1, sym2 :: syms2 ->
        equal sym1 sym2 && lequal syms1 syms2
    | _ :: _, []
    | [], _ :: _ ->
        false

  let iter f =
    Terminal.iter (fun t -> f (T t));
    Nonterminal.iter (fun nt -> f (N nt))

  let non_error sym =
    match sym with
    | T tok ->
        Terminal.non_error tok
    | N _ ->
        true

  let print normalize = function
    | N nt ->
        Nonterminal.print normalize nt
    | T tok ->
        Terminal.print tok

  let nonterminal = function
    | T _ ->
        false
    | N _ ->
        true

  (* Printing an array of symbols. [offset] is the start offset -- we
     print everything to its right. [dot] is the dot offset -- we
     print a dot at this offset, if we find it. Note that [dot] can be
     equal to [length]. *)

  let buffer =
    Buffer.create 1024

  let printaod offset dot normalize symbols =
    let length = Array.length symbols in
    let first = ref true in
    let separate () =
      if not !first then Printf.bprintf buffer " ";
      first := false
    in
    for i = offset to length do
      if i = dot then begin
        separate();
        Printf.bprintf buffer "."
      end;
      if i < length then begin
        separate();
        Printf.bprintf buffer "%s" (print normalize symbols.(i))
      end
    done;
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s

  let printao offset normalize symbols =
    printaod offset (-1) normalize symbols

  let printa normalize symbols =
    printao 0 normalize symbols

  let printl symbols =
    let normalize = false in
    printa normalize (Array.of_list symbols)

  let lookup name =
    try
      T (Terminal.lookup name)
    with Not_found ->
      try
        N (Nonterminal.lookup name)
      with Not_found ->
        assert false (* well-formedness check has been performed earlier *)

end

(* Sets of symbols. *)

module SymbolSet = struct

  include Set.Make(Symbol)

  let print symbols =
    Symbol.printl (elements symbols)

  (* The following definitions are used in the computation of symbolic FOLLOW
     sets below. They are not exported outside of this file. *)

  type property =
    t

  let bottom =
    empty

  let leq =
    subset

  let join =
    union

end

(* Maps over symbols. *)

module SymbolMap = struct

  include Map.Make(Symbol)

  let domain m =
    fold (fun symbol _ accu ->
      symbol :: accu
    ) m []

  let init f xs =
    List.fold_left (fun accu x ->
      add x (f x) accu
    ) empty xs

  let purelynonterminal m =
    fold (fun symbol _ accu ->
      accu && Symbol.nonterminal symbol
    ) m true

end

(* ------------------------------------------------------------------------ *)
(* Productions. *)

module Production = struct

  type index =
      int

  let compare =
    (-)

  (* A new production S' -> S is created for every start symbol S.
     It is known as a start production. *)

  (* Count how many productions we have, including the start productions.
     This is [n]. *)

  let n : int =
    let n = StringMap.fold (fun _ { branches = branches } n ->
      n + List.length branches
    ) grammar.rules 0 in
    if verbose then
      Error.logG 1 (fun f -> Printf.fprintf f "Grammar has %d productions.\n" n);
    n + StringSet.cardinal grammar.start_symbols

  let p2i prod =
    prod

  let i2p prod =
    assert (prod >= 0 && prod < n);
    prod

  (* Create a number of uninitialized tables that map a production index to
     information about this production. *)

  (* [table] maps a production to the left-hand side and right-hand side of
     this production. [identifiers] maps a production to an array of the
     identifiers that are used to name the elements of the right-hand side.
     [actions] maps a production to an optional semantic action. (Only the
     start productions have none.) [positions] maps a production to an array
     of the positions (in the .mly file) of the elements of the right-hand
     side. [rhs_attributes] maps a production to an array of the attributes
     attached to the elements of the right-hand side. [prec_decl] maps a
     production to an optional [%prec] annotation. [production_level] maps
     a production to a production level (see [ParserAux]). *)

  let table : (Nonterminal.t * Symbol.t array) array =
    Array.make n (-1, [||])

  let identifiers : identifier array array =
    Array.make n [||]

  let actions : action option array =
    Array.make n None

  let positions : Positions.t list array =
    Array.make n []

  let rhs_attributes : Syntax.attributes array array =
    Array.make n [||]

  let prec_decl : symbol located option array =
    Array.make n None

  let production_level : branch_production_level array =
    (* The start productions receive a level that pretends that they
       originate in a fictitious "builtin" file. So, a reduce/reduce
       conflict that involves a start production will not be solved. *)
    let dummy = ProductionLevel (InputFile.builtin_input_file, 0) in
    Array.make n dummy

  (* [ntprods] maps a nonterminal symbol to the interval of its productions. *)

  let ntprods : (int * int) array =
    Array.make Nonterminal.n (-1, -1)

  (* This Boolean flag records whether the grammar uses the [error] token. *)

  let grammar_uses_error_token =
    ref false

  (* Create the start productions, populating the above arrays as appropriate.
     [start] is the number of start productions, therefore also the index of the
     first non-start production. [startprods] is a mapping of the start symbols
     to the corresponding start productions. *)

  let (start : int),
      (startprods : index NonterminalMap.t) =
    StringSet.fold (fun nonterminal (k, startprods) ->
      let nt = Nonterminal.lookup nonterminal
      and nt' = Nonterminal.lookup (nonterminal ^ "'") in
      table.(k) <- (nt', [| Symbol.N nt |]);
      identifiers.(k) <- [| "_1" |];
      ntprods.(nt') <- (k, k+1);
      positions.(k) <- Nonterminal.positions nt;
      k+1,
      NonterminalMap.add nt k startprods
    ) grammar.start_symbols (0, NonterminalMap.empty)

  (* Create the non-start productions, populating the above arrays. *)

  let producer_symbol producer =
    Symbol.lookup (producer_symbol producer)

  let (_ : int) = StringMap.fold (fun nonterminal { branches } k ->
    let nt = Nonterminal.lookup nonterminal in
    let k' = List.fold_left (fun k branch ->
      let producers = Array.of_list branch.producers in
      let rhs = Array.map producer_symbol producers in
      table.(k) <- (nt, rhs);
      identifiers.(k) <- Array.map producer_identifier producers;
      actions.(k) <- Some branch.action;
      rhs_attributes.(k) <- Array.map producer_attributes producers;
      production_level.(k) <- branch.branch_production_level;
      prec_decl.(k) <- branch.branch_prec_annotation;
      positions.(k) <- [ branch.branch_position ];
      if not (MArray.for_all Symbol.non_error rhs) then
        grammar_uses_error_token := true;
      k+1
    ) k branches in
    ntprods.(nt) <- (k, k');
    k'
  ) grammar.rules start

  (* Iteration over the productions associated with a specific nonterminal. *)

  let iternt nt f =
    let k, k' = ntprods.(nt) in
    Misc.iterij k k' f

  let foldnt nt f accu =
    let k, k' = ntprods.(nt) in
    Misc.foldij k k' f accu

  let mapnt nt f =
    let k, k' = ntprods.(nt) in
    Misc.mapij k k' f

  let foldnt_lazy nt f accu =
    let k, k' = ntprods.(nt) in
    Misc.foldij_lazy k k' f accu

  (* Accessors. *)

  let def prod =
    table.(prod)

  let nt prod =
    let nt, _ = table.(prod) in
    nt

  let rhs prod =
    let _, rhs = table.(prod) in
    rhs

  let length prod =
    Array.length (rhs prod)

  let identifiers prod =
    identifiers.(prod)

  let is_start prod =
    prod < start

  let classify prod =
    if is_start prod then
      match (rhs prod).(0) with
      | Symbol.N nt ->
          Some nt
      | Symbol.T _ ->
          assert false
    else
      None

  let action prod =
    match actions.(prod) with
    | Some action ->
        action
    | None ->
        (* Start productions have no action. *)
        assert (is_start prod);
        assert false

  let positions prod =
    positions.(prod)

  let lhs_attributes prod =
    Nonterminal.attributes (nt prod)

  let rhs_attributes prod =
    rhs_attributes.(prod)

  let startsymbol2startprod nt =
    try
      NonterminalMap.find nt startprods
    with Not_found ->
      assert false (* [nt] is not a start symbol *)

  let error_free prod =
    MArray.for_all Symbol.non_error (rhs prod)

  (* Iteration. *)

  let init f =
    Array.init n f

  let iter f =
    Misc.iteri n f

  let fold f accu =
    Misc.foldi n f accu

  let map f =
    Misc.mapi n f

  let amap f =
    Array.init n f

  let iterx f =
    for prod = start to n - 1 do
      f prod
    done

  let foldx f accu =
    Misc.foldij start n f accu

  let mapx f =
    Misc.mapij start n f

  (* Printing a production. *)

  let print prod =
    assert (not (is_start prod));
    let nt, rhs = table.(prod) in
    if Array.length rhs = 0 then
      (* Avoid producing a trailing space. *)
      Printf.sprintf "%s ->"
        (Nonterminal.print false nt)
    else
      Printf.sprintf "%s -> %s"
        (Nonterminal.print false nt)
        (Symbol.printao 0 false rhs)

  let describe gerund prod =
    match classify prod with
    | Some nt ->
        let ending = if gerund then "ing" else "" in
        Printf.sprintf "accept%s %s" ending (Nonterminal.print false nt)
    | None ->
        let ending = if gerund then "ing" else "e" in
        Printf.sprintf "reduc%s production %s" ending (print prod)

  (* Tabulation and sum. *)

  let tabulate f =
    Misc.tabulate n f

  let sum f =
    Misc.sum n f

  (* This array allows recording, for each %prec declaration, whether it is
     ever useful. This allows us to emit a warning about useless %prec
     declarations. *)

  (* 2015/10/06: We take into account the fact that a %prec declaration can be
     duplicated by inlining or by the expansion of parameterized non-terminal
     symbols. Our table is not indexed by productions, but by positions (of
     %prec declarations in the source). Thus, if a %prec declaration is
     duplicated, at least one of its copies should be found useful for the
     warning to be suppressed. *)

  let ever_useful : (Positions.t, unit) Hashtbl.t =
    (* assuming that generic hashing and equality on positions are OK *)
    Hashtbl.create 16

  let consult_prec_decl prod =
    let osym = prec_decl.(prod) in
    lazy (
      Option.iter (fun sym ->
        (* Mark this %prec declaration as useful. *)
        let pos = Positions.position sym in
        Hashtbl.add ever_useful pos ()
      ) osym
    ),
    osym

  (* This function prints warnings about useless precedence declarations for
     productions (%prec). It should be invoked after only the automaton has
     been constructed. *)

  let diagnostics () =
    iterx (fun prod ->
      let osym = prec_decl.(prod) in
      Option.iter (fun sym ->
        (* Check whether this %prec declaration was useless. *)
        let pos = Positions.position sym in
        if not (Hashtbl.mem ever_useful pos) then begin
          Error.grammar_warning [pos] "this %%prec declaration is never useful.";
          Hashtbl.add ever_useful pos () (* hack: avoid two warnings at the same position *)
        end
      ) osym
    )

  (* Determining the precedence level of a production. If no %prec
     declaration was explicitly supplied, it is the precedence level
     of the rightmost terminal symbol in the production's right-hand
     side. *)

  type production_level =
    | PNone
    | PRightmostToken of Terminal.t
    | PPrecDecl of symbol

  let rightmost_terminal prod =
    Array.fold_left (fun accu symbol ->
      match symbol with
      | Symbol.T tok ->
          PRightmostToken tok
      | Symbol.N _ ->
          accu
    ) PNone (rhs prod)

  let combine e1 e2 =
    lazy (Lazy.force e1; Lazy.force e2)

  let precedence prod =
    let fact1, prec_decl = consult_prec_decl prod in
    let oterminal =
      match prec_decl with
      | None ->
          rightmost_terminal prod
      | Some { value = terminal } ->
          PPrecDecl terminal
    in
    match oterminal with
    | PNone ->
        fact1, UndefinedPrecedence
    | PRightmostToken tok ->
        let fact2, level = Terminal.precedence_level tok in
        combine fact1 fact2, level
    | PPrecDecl id ->
        let fact2, level = TokPrecedence.leveli id  in
        combine fact1 fact2, level

end

let grammar_uses_error_token =
  !Production.grammar_uses_error_token

(* ------------------------------------------------------------------------ *)
(* Maps over productions. *)

module ProductionMap = struct

  include Patricia.Big

  (* Iteration over the start productions only. *)

  let start f =
    Misc.foldi Production.start (fun prod m ->
      add prod (f prod) m
    ) empty

end

(* ------------------------------------------------------------------------ *)
(* Support for analyses of the grammar, expressed as fixed point computations.
   We exploit the generic fixed point algorithm in [Fix]. *)

(* We perform memoization only at nonterminal symbols. We assume that the
   analysis of a symbol is the analysis of its definition (as opposed to,
   say, a computation that depends on the occurrences of this symbol in
   the grammar). *)

module GenericAnalysis
  (P : Fix.PROPERTY)
  (S : sig
    open P

    (* An analysis is specified by the following functions. *)

    (* [shortcut] can be used to map a nonterminal symbol to a property.
       In that case, the definition of this nonterminal symbol is ignored. *)
    val shortcut: Nonterminal.t -> property option

    (* [terminal] maps a terminal symbol to a property. *)
    val terminal: Terminal.t -> property

    (* [disjunction] abstracts a binary alternative. That is, when we analyze
       an alternative between several productions, we compute a property for
       each of them independently, then we combine these properties using
       [disjunction]. *)
    val disjunction: property -> (unit -> property) -> property

    (* [P.bottom] should be a neutral element for [disjunction]. We use it in
       the analysis of an alternative with zero branches. *)

    (* [conjunction] abstracts a binary sequence. That is, when we analyze a
       sequence, we compute a property for each member independently, then we
       combine these properties using [conjunction]. In general, conjunction
       needs access to the first member of the sequence (a symbol), not just
       to its analysis (a property). *)
    val conjunction: Symbol.t -> property -> (unit -> property) -> property

    (* [epsilon] abstracts the empty sequence. It should be a neutral element
       for [conjunction]. *)
    val epsilon: property

    (* The Boolean flag [ignore_error_productions] indicates whether the
       productions that mention the [error] token should be ignored. *)
    val ignore_error_productions: bool

  end)
: sig
  open P

  (* The results of the analysis take the following form. *)

  (* To every nonterminal symbol, we associate a property. *)
  val nonterminal: Nonterminal.t -> property

  (* To every symbol, we associate a property. *)
  val symbol: Symbol.t -> property

  (* To every suffix of every production, we associate a property.
     The offset [i], which determines the beginning of the suffix,
     must be contained between [0] and [n], inclusive, where [n]
     is the length of the production. *)
  val production: Production.index -> int -> property

end = struct
  open P

  (* The following analysis functions are parameterized over [get], which allows
     making a recursive call to the analysis at a nonterminal symbol. [get] maps
     a nonterminal symbol to a property. *)

  (* Analysis of a symbol. *)

  let symbol sym get : property =
    match sym with
    | Symbol.T tok ->
        S.terminal tok
    | Symbol.N nt ->
        (* Recursive call to the analysis, via [get]. *)
        get nt

  (* Analysis of (a suffix of) a production [prod], starting at index [i]. *)

  let production prod i get : property =
    assert (not S.ignore_error_productions || Production.error_free prod);
    let rhs = Production.rhs prod in
    let n = Array.length rhs in
    (* Conjunction over all symbols in the right-hand side. This can be viewed
       as a version of [Array.fold_right], which does not necessarily begin at
       index [0]. Note that, because [conjunction] is lazy, it is possible
       to stop early. *)
    let rec loop i =
      if i = n then
        S.epsilon
      else
        let sym = rhs.(i) in
        S.conjunction sym
          (symbol sym get)
          (fun () -> loop (i+1))
    in
    loop i

  (* The analysis is the least fixed point of the following function, which
     analyzes a nonterminal symbol by looking up and analyzing its definition
     as a disjunction of conjunctions of symbols. *)

  let nonterminal nt get : property =
    match S.shortcut nt with
    | Some p ->
        p
    | None ->
        (* Disjunction over all productions for this nonterminal symbol. *)
        Production.foldnt_lazy nt (fun prod rest ->
          if S.ignore_error_productions && not (Production.error_free prod) then
            rest()
          else
            S.disjunction
              (production prod 0 get)
              rest
        ) P.bottom

  (* The least fixed point is taken as follows. Note that it is computed
     on demand, as [lfp] is called by the user. *)

  module F =
    Fix.Make
      (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
      (P)

  let nonterminal =
    F.lfp nonterminal

  (* The auxiliary functions can be published too. *)

  let symbol sym =
    symbol sym nonterminal

  let production prod i =
    production prod i nonterminal

end

(* ------------------------------------------------------------------------ *)

(* Compute which nonterminal symbols are nonempty, that is, which symbols
   generate a nonempty language. Also, compute which nonterminal symbols are
   nullable. The two computations are almost identical. The only difference is
   in the base case: a single terminal symbol is not nullable, but is
   nonempty. *)

module NONEMPTY =
  GenericAnalysis
    (Fix.Prop.Boolean)
    (struct
      let shortcut _nt = None
      (* A terminal symbol is nonempty. *)
      let terminal _ = true
      (* An alternative is nonempty if at least one branch is nonempty. *)
      let disjunction p q = p || q()
      (* A sequence is nonempty if both members are nonempty. *)
      let conjunction _ p q = p && q()
      (* The sequence epsilon is nonempty. It generates the singleton
         language {epsilon}. *)
      let epsilon = true
      let ignore_error_productions = false
     end)

module NULLABLE =
  GenericAnalysis
    (Fix.Prop.Boolean)
    (struct
      let shortcut _nt = None
      (* A terminal symbol is not nullable. *)
      let terminal _ = false
      (* An alternative is nullable if at least one branch is nullable. *)
      let disjunction p q = p || q()
      (* A sequence is nullable if both members are nullable. *)
      let conjunction _ p q = p && q()
      (* The sequence epsilon is nullable. *)
      let epsilon = true
      let ignore_error_productions = false
     end)

(* ------------------------------------------------------------------------ *)
(* Compute FIRST sets. *)

module FIRST =
  GenericAnalysis
    (TerminalSet)
    (struct
      let shortcut _nt = None
      (* A terminal symbol has a singleton FIRST set. *)
      let terminal = TerminalSet.singleton
      (* The FIRST set of an alternative is the union of the FIRST sets. *)
      let disjunction p q = TerminalSet.union p (q())
      (* The FIRST set of a sequence is the union of:
           the FIRST set of the first member, and
           the FIRST set of the second member, if the first member is nullable. *)
      let conjunction symbol p q =
        if NULLABLE.symbol symbol then
          TerminalSet.union p (q())
        else
          p
      (* The FIRST set of the empty sequence is empty. *)
      let epsilon = TerminalSet.empty
      let ignore_error_productions = false
     end)

(* ------------------------------------------------------------------------ *)
(* For every nonterminal symbol [nt], compute a word of minimal length
   generated by [nt]. *)

(* In principle, this analysis subsumes [NONEMPTY] and [NULLABLE]. Indeed,
   [nt] produces a nonempty language if only if the minimal length is finite;
   [nt] is nullable if only if the minimal length is zero. Be careful, though:
   [MINIMAL] analyzes a grammar where error productions are ignored. *)

(* This analysis is in principle more costly than [NONEMPTY] and [NULLABLE],
   so it is performed only on demand. In practice, it seems to be very cheap:
   its cost is not measurable for any of the grammars in our benchmark
   suite. *)

module MINIMAL =
  GenericAnalysis
    (struct
      include CompletedNatWitness
      type property = Terminal.t t
     end)
    (struct
      let shortcut _nt = None
      open CompletedNatWitness
      (* A terminal symbol has length 1. *)
      let terminal = singleton
      (* The length of an alternative is the minimum length of any branch. *)
      let disjunction = min_lazy
      (* The length of a sequence is the sum of the lengths of the members. *)
      let conjunction _ = add_lazy
      (* The epsilon sequence has length 0. *)
      let epsilon = epsilon
      (* The productions that contain [error] are ignored. *)
      let ignore_error_productions = true
     end)

(* ------------------------------------------------------------------------ *)

(* For every nonterminal symbol [nt], we wish to compute the maximum length of
   a word generated by [nt]. This length can be either finite or [infty]. *)

(* We assume that every symbol generates a nonempty language. This removes the
   need to watch out for productions whose right hand side is empty (which we
   would have to ignore). A similar assumption is made in the computation of
   FIRST sets above. *)

(* We need to determine which symbols generate a nonempty word, that is, a
   word of length at least 1. We might tempted to deduce this information from
   the FIRST sets (indeed, a symbol generates a nonempty word if and only if
   its FIRST set is nonempty), but this does not work because (here) we want
   to ignore error productions. *)

module GENERATES_NONEMPTY_WORD =
  GenericAnalysis
    (Fix.Prop.Boolean)
    (struct
      let shortcut _nt = None
      (* A terminal symbol generates a nonempty word. *)
      let terminal _tok = true
      (* An alternative generates a nonempty word if at least one branch
         generates a nonempty word. *)
      let disjunction p q = p || q()
      (* A sequence generates a nonempty word if at least one side generates
         a nonempty word. (We assume both languages are nonempty.) *)
      let conjunction _ p q = p || q()
      (* [epsilon] fails to generate a nonempty word. *)
      let epsilon = false
      (* The productions that contain [error] are ignored. *)
      let ignore_error_productions = true
     end)

let generates_nonempty_word symbol : bool =
  GENERATES_NONEMPTY_WORD.symbol symbol

(* Then, we construct the reference graph of the grammar. The vertices of
   this graph are the nonterminal symbols, and, if there is a production
   [A -> alpha B gamma], then there is an edge from A to B. With each such
   edge, we associate a Boolean label, which is [true] if [alpha . gamma]
   generates a nonempty word. *)

module G = struct
  type node = Nonterminal.t
  let n = Nonterminal.n
  let index nt = nt
  let iter = Nonterminal.iter
  let labeled_successors yield nt =
    Production.iternt nt (fun prod ->
      let rhs = Production.rhs prod in
      rhs |> Array.iteri (fun i symbol ->
        match symbol with
        | Symbol.T _   -> ()
        | Symbol.N nt' ->
            (* There is an edge from [nt] to [nt'], whose Boolean label [gnw]
               is obtained by computing whether the right-hand side, deprived
               of [nt'], can generate a nonempty word. *)
            let gnw =
              MArray.existsi (fun j symbol ->
                i <> j && generates_nonempty_word symbol
              ) rhs
            in
            yield gnw nt'
      )
    )
  let successors yield nt =
    labeled_successors (fun _gnw nt' -> yield nt') nt
end

(* We now compute the strongly connected components of the reference graph
   described above. If a component contains an edge labeled [true], then every
   nonterminal symbol in this component can generate words of unbounded
   length. Mark these symbols in an array [unbounded]. (This computation is
   performed only when needed.) *)

let unbounded : bool array Lazy.t =
  lazy begin
    let unbounded = Array.make Nonterminal.n false in
    let module T = Tarjan.Run(G) in
    (* For each edge of [nt] to [nt'] labeled [gnw], *)
    G.iter begin fun nt ->
      nt |> G.labeled_successors begin fun gnw nt' ->
        (* If [gnw] is set and if [nt] and [nt'] lie in the same component, *)
        if gnw && T.representative nt = T.representative nt' then begin
          (* Then mark every symbol in this component as unbounded. *)
          T.scc (T.representative nt) |> List.iter begin fun nt ->
            unbounded.(nt) <- true
          end
        end
      end
    end;
    unbounded
  end

let unbounded nt : bool =
  (Lazy.force unbounded).(nt)

(* We can finally perform a least fixed point computation, in the lattice
   [NatMaxInfinity], to find out what is the maximum length of a word
   generated by each nonterminal symbol. Such a computation normally would not
   terminate, as the lattice has unbounded height: a cycle of strictly
   positive weight in the grammar will cause an endless climb. However, we
   have just identified the symbols that participate in these cycles: they are
   the symbols [nt] such that [unbounded nt] is [true]. We use the [shortcut]
   function to set these symbols directly to [infinity]. The cycles that may
   remain in the grammar must have zero weight and cannot cause divergence.
   The fixed point computation must therefore terminate and yield the desired
   information. *)

module MAXIMAL =
  GenericAnalysis
    (NatInfinityMax)
    (struct
      open NatInfinityMax
      let shortcut nt = if unbounded nt then Some infinity else None
      (* A terminal symbol has length 1. *)
      let terminal _tok = finite 1
      (* The length of an alternative is the maximum length of any branch. *)
      let disjunction = max_lazy
      (* The length of a sequence is the sum of the lengths of the members. *)
      let conjunction _ = add_lazy
      (* The epsilon sequence has length 0. *)
      let epsilon = bottom
      (* The productions that contain [error] are ignored. *)
      let ignore_error_productions = true
     end)

(* ------------------------------------------------------------------------ *)

let () =
  if verbose then begin
    (* If a start symbol generates the empty language or generates
       the language {epsilon}, report an error. In principle, this
       could be just a warning. However, in [Engine], in the function
       [start], it is convenient to assume that neither of these
       situations can arise. This means that at least one token must
       be read. *)
    StringSet.iter (fun symbol ->
      let nt = Nonterminal.lookup symbol in
      if not (NONEMPTY.nonterminal nt) then
        Error.error
          (Nonterminal.positions nt)
          "%s generates the empty language." (Nonterminal.print false nt);
      if TerminalSet.is_empty (FIRST.nonterminal nt) then
        Error.error
          (Nonterminal.positions nt)
          "%s generates the language {epsilon}." (Nonterminal.print false nt)
    ) grammar.start_symbols;
    (* If a nonterminal symbol generates the empty language, issue a warning. *)
    for nt = Nonterminal.start to Nonterminal.n - 1 do
      if not (NONEMPTY.nonterminal nt) then
        Error.grammar_warning
          (Nonterminal.positions nt)
          "%s generates the empty language." (Nonterminal.print false nt);
    done
  end

(* ------------------------------------------------------------------------ *)
(* Dump the analysis results. *)

let () =
  if verbose then
    Error.logG 2 (fun f ->
      for nt = Nonterminal.start to Nonterminal.n - 1 do
        Printf.fprintf f "nullable(%s) = %b\n"
          (Nonterminal.print false nt)
          (NULLABLE.nonterminal nt)
      done;
      for nt = Nonterminal.start to Nonterminal.n - 1 do
        Printf.fprintf f "first(%s) = %s\n"
          (Nonterminal.print false nt)
          (TerminalSet.print (FIRST.nonterminal nt))
      done;
      for nt = Nonterminal.start to Nonterminal.n - 1 do
        Printf.fprintf f "minimal(%s) = %s\n"
          (Nonterminal.print false nt)
          (CompletedNatWitness.print Terminal.print (MINIMAL.nonterminal nt))
      done;
      for nt = Nonterminal.start to Nonterminal.n - 1 do
        Printf.fprintf f "maximal(%s) = %s\n"
          (Nonterminal.print false nt)
          (NatInfinityMax.print (MAXIMAL.nonterminal nt))
      done;
  )

let () =
  if verbose then
    Time.tick "Analysis of the grammar"

(* ------------------------------------------------------------------------ *)
(* Compute FOLLOW sets. Unnecessary for us, but requested by a user. Also,
   this is useful for the SLR(1) test. Thus, we perform this analysis only
   on demand. *)

(* The computation of the symbolic FOLLOW sets follows exactly the same
   pattern as that of the traditional FOLLOW sets. We share code and
   parameterize this computation over a module [P]. The type [P.property]
   intuitively represents a set of symbols. *)

module FOLLOW (P : sig
  include Fix.MINIMAL_SEMI_LATTICE
  val bottom: property
  val terminal: Terminal.t -> property
  val first: Production.index -> int -> property
end) = struct

  module M =
    Fix.Glue.ArraysAsImperativeMaps(Nonterminal)

  module S =
    FixSolver.Make(M)(P)

  (* Build a system of constraints. *)

  (* Iterate over all start symbols. *)
  let () =
    let sharp = P.terminal Terminal.sharp in
    for nt = 0 to Nonterminal.start - 1 do
      assert (Nonterminal.is_internal_start nt);
      (* Add # to FOLLOW(nt). *)
      S.record_ConVar sharp nt
    done
    (* We need to do this explicitly because our start productions are
       of the form S' -> S, not S' -> S #, so # will not automatically
       appear into FOLLOW(S) when the start productions are examined. *)

  (* Iterate over all productions. *)
  let () =
    Array.iteri (fun prod (nt1, rhs) ->
      (* Iterate over all nonterminal symbols [nt2] in the right-hand side. *)
      Array.iteri (fun i symbol ->
        match symbol with
        | Symbol.T _ ->
            ()
        | Symbol.N nt2 ->
            let nullable = NULLABLE.production prod (i+1)
            and first = P.first prod (i+1) in
            (* The FIRST set of the remainder of the right-hand side
               contributes to the FOLLOW set of [nt2]. *)
            S.record_ConVar first nt2;
            (* If the remainder of the right-hand side is nullable,
               FOLLOW(nt1) contributes to FOLLOW(nt2). *)
            if nullable then
              S.record_VarVar nt1 nt2
      ) rhs
    ) Production.table

  (* Second pass. Solve the equations (on demand). *)

  let follow : (Nonterminal.t -> P.property) Lazy.t =
    lazy (
      let module S = S.Solve() in
      fun nt -> Option.value (S.solution nt) ~default:P.bottom
    )

  let follow : Nonterminal.t -> P.property =
    fun nt -> (Lazy.force follow) nt

end

(* Use the above functor to obtain the standard (concrete) FOLLOW sets. *)

let follow : Nonterminal.t -> TerminalSet.t =
  let module F = FOLLOW(struct
    include TerminalSet
    let terminal = singleton
    let first = FIRST.production
  end) in
  F.follow

(* At log level 2, display the FOLLOW sets. *)

let () =
  if verbose then
    Error.logG 2 (fun f ->
      for nt = Nonterminal.start to Nonterminal.n - 1 do
        Printf.fprintf f "follow(%s) = %s\n"
          (Nonterminal.print false nt)
          (TerminalSet.print (follow nt))
      done
    )

(* Compute FOLLOW sets for the terminal symbols as well. Again, unnecessary
   for us, but requested by a user. This is done in a single pass over the
   grammar -- no new fixpoint computation is required. *)

let tfollow : TerminalSet.t array Lazy.t =
  lazy (

    let tfollow =
      Array.make Terminal.n TerminalSet.empty
    in

    (* Iterate over all productions. *)
    Array.iteri (fun prod (nt1, rhs) ->
      (* Iterate over all terminal symbols [t2] in the right-hand side. *)
      Array.iteri (fun i symbol ->
        match symbol with
        | Symbol.N _ ->
            ()
        | Symbol.T t2 ->
            let nullable = NULLABLE.production prod (i+1)
            and first = FIRST.production prod (i+1) in
            (* The FIRST set of the remainder of the right-hand side
               contributes to the FOLLOW set of [t2]. *)
            tfollow.(t2) <- TerminalSet.union first tfollow.(t2);
            (* If the remainder of the right-hand side is nullable,
               FOLLOW(nt1) contributes to FOLLOW(t2). *)
            if nullable then
              tfollow.(t2) <- TerminalSet.union (follow nt1) tfollow.(t2)
      ) rhs
    ) Production.table;

    tfollow

  )

(* Define another accessor. *)

let tfollow t =
  (Lazy.force tfollow).(t)

(* At log level 3, display the FOLLOW sets for terminal symbols. *)

let () =
  if verbose then
    Error.logG 3 (fun f ->
      for t = 0 to Terminal.n - 1 do
        Printf.fprintf f "follow(%s) = %s\n"
          (Terminal.print t)
          (TerminalSet.print (tfollow t))
      done
    )

(* ------------------------------------------------------------------------ *)
(* Compute symbolic FIRST and FOLLOW sets. *)

(* The symbolic FIRST set of the word determined by [prod/i] is defined
   (and computed) as follows. *)

let sfirst prod i =
  let rhs = Production.rhs prod in
  let n = Array.length rhs in
  let rec loop i =
    if i = n then
      (* If the word [prod/i] is empty, the set is empty. *)
      SymbolSet.empty
    else
      let sym = rhs.(i) in
      (* If the word [prod/i] begins with a symbol [sym], then [sym]
         itself is part of the symbolic FIRST set, unconditionally. *)
      SymbolSet.union
        (SymbolSet.singleton sym)
        (* Furthermore, if [sym] is nullable, then the symbolic
           FIRST set of the sub-word [prod/i+1] contributes, too. *)
        (if NULLABLE.symbol sym then loop (i + 1) else SymbolSet.empty)
  in
  loop i

(* The symbolic FOLLOW sets are computed just like the FOLLOW sets,
   except we use a symbolic FIRST set instead of a standard FIRST
   set. *)

let sfollow : Nonterminal.t -> SymbolSet.t =
  let module F = FOLLOW(struct
    let bottom = SymbolSet.bottom
    include Fix.Glue.MinimalSemiLattice(SymbolSet)
    let terminal t = SymbolSet.singleton (Symbol.T t)
    let first = sfirst
  end) in
  F.follow

(* At log level 3, display the symbolic FOLLOW sets. *)

let () =
  if verbose then
    Error.logG 3 (fun f ->
      for nt = Nonterminal.start to Nonterminal.n - 1 do
        Printf.fprintf f "sfollow(%s) = %s\n"
          (Nonterminal.print false nt)
          (SymbolSet.print (sfollow nt))
      done
    )

(* ------------------------------------------------------------------------ *)
(* Provide explanations about FIRST sets. *)

(* The idea is to explain why a certain token appears in the FIRST set
   for a certain sequence of symbols. Such an explanation involves
   basic assertions of the form (i) symbol N is nullable and (ii) the
   token appears in the FIRST set for symbol N. We choose to take
   these basic facts for granted, instead of recursively explaining
   them, so as to keep explanations short. *)

(* We first produce an explanation in abstract syntax, then
   convert it to a human-readable string. *)

type explanation =
  | EObvious                                 (* sequence begins with desired token *)
  | EFirst of Terminal.t * Nonterminal.t     (* sequence begins with a nonterminal that produces desired token *)
  | ENullable of Symbol.t list * explanation (* sequence begins with a list of nullable symbols and ... *)

let explain (tok : Terminal.t) (rhs : Symbol.t array) (i : int) =
  let length = Array.length rhs in
  let rec loop i =
    assert (i < length);
    let symbol = rhs.(i) in
    match symbol with
    | Symbol.T tok' ->
        assert (Terminal.equal tok tok');
        EObvious
    | Symbol.N nt ->
        if TerminalSet.mem tok (FIRST.nonterminal nt) then
          EFirst (tok, nt)
        else begin
          assert (NULLABLE.nonterminal nt);
          match loop (i + 1) with
          | ENullable (symbols, e) ->
              ENullable (symbol :: symbols, e)
          | e ->
              ENullable ([ symbol ], e)
        end
  in
  loop i

let rec convert = function
  | EObvious ->
      ""
  | EFirst (tok, nt) ->
      Printf.sprintf "%s can begin with %s"
        (Nonterminal.print false nt)
        (Terminal.print tok)
  | ENullable (symbols, e) ->
      let e = convert e in
      Printf.sprintf "%s can vanish%s%s"
        (Symbol.printl symbols)
        (if e = "" then "" else " and ")
        e

(* ------------------------------------------------------------------------ *)
(* Package the analysis results. *)

module Analysis = struct

  let nullable = NULLABLE.nonterminal

  let nullable_symbol = NULLABLE.symbol

  let first = FIRST.nonterminal

  let first_symbol = FIRST.symbol

  (* An initial definition of [nullable_first_prod]. *)

  let nullable_first_prod prod i =
    NULLABLE.production prod i,
    FIRST.production prod i

  (* A memoised version, so as to avoid recomputing along a production's
     right-hand side. *)

  let nullable_first_prod =
    Misc.tabulate Production.n (fun prod ->
      Misc.tabulate (Production.length prod + 1) (fun i ->
        nullable_first_prod prod i
      )
    )

  let first_prod_lookahead prod i z =
    let nullable, first = nullable_first_prod prod i in
    if nullable then
      TerminalSet.add z first
    else
      first

  let explain_first_rhs (tok : Terminal.t) (rhs : Symbol.t array) (i : int) =
    convert (explain tok rhs i)

  let follow = follow

  let attributes =
    grammar.gr_attributes

  let minimal nt =
    CompletedNatWitness.to_int (MINIMAL.nonterminal nt)

  let minimal_prod prod i =
    assert (0 <= i && i <= Production.length prod);
    CompletedNatWitness.to_int (MINIMAL.production prod i)

  let maximal nt =
    NatInfinityMax.to_int (MAXIMAL.nonterminal nt)

  let maximal_prod prod i =
    assert (0 <= i && i <= Production.length prod);
    NatInfinityMax.to_int (MAXIMAL.production prod i)

end

(* ------------------------------------------------------------------------ *)
(* Conflict resolution via precedences. *)

module Precedence = struct

  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  type order = Lt | Gt | Eq | Ic

  let precedence_order p1 p2 =
    match p1, p2 with
      | UndefinedPrecedence, _
      | _, UndefinedPrecedence ->
          Ic
      | PrecedenceLevel (m1, l1, _, _), PrecedenceLevel (m2, l2, _, _) ->
          if not (InputFile.same_input_file m1 m2) then
            Ic
          else
            if l1 > l2 then
              Gt
            else if l1 < l2 then
              Lt
            else
              Eq

  let production_order p1 p2 =
    match p1, p2 with
      | ProductionLevel (m1, l1), ProductionLevel (m2, l2) ->
          if not (InputFile.same_input_file m1 m2) then
            Ic
          else
            if l1 > l2 then
              Gt
            else if l1 < l2 then
              Lt
            else
              Eq

  let shift_reduce tok prod =
    let fact1, tokp  = Terminal.precedence_level tok
    and fact2, prodp = Production.precedence prod in
    match precedence_order tokp prodp with

      (* Our information is inconclusive. Drop [fact1] and [fact2],
         that is, do not record that this information was useful. *)

    | Ic ->
        DontKnow

      (* Our information is useful. Record that fact by evaluating
         [fact1] and [fact2]. *)

    | (Eq | Lt | Gt) as c ->
        Lazy.force fact1;
        Lazy.force fact2;
        match c with

        | Ic ->
            assert false (* already dispatched *)

        | Eq ->
            begin
              match Terminal.associativity tok with
              | LeftAssoc  -> ChooseReduce
              | RightAssoc -> ChooseShift
              | NonAssoc   -> ChooseNeither
              | _          -> assert false
                              (* If [tok]'s precedence level is defined, then
                                 its associativity must be defined as well. *)
            end

        | Lt ->
            ChooseReduce

        | Gt ->
            ChooseShift


  let reduce_reduce prod1 prod2 =
    let pl1 = Production.production_level.(prod1)
    and pl2 = Production.production_level.(prod2) in
    match production_order pl1 pl2 with
    | Lt ->
        Some prod1
    | Gt ->
        Some prod2
    | Eq ->
        (* The order is strict except in the presence of parameterized
           non-terminals and/or inlining. Two productions can have the same
           precedence level if they originate, via macro-expansion or via
           inlining, from a single production in the source grammar. *)
        None
    | Ic ->
        None

end

(* This function prints warnings about useless precedence declarations for
   terminal symbols (%left, %right, %nonassoc) and productions (%prec). It
   should be invoked after only the automaton has been constructed. *)

let diagnostics () =
  if not Settings.ignore_all_unused_precedence_levels then begin
    TokPrecedence.diagnostics();
    Production.diagnostics()
  end

(* ------------------------------------------------------------------------ *)
(* %on_error_reduce declarations. *)

module OnErrorReduce = struct

  (* We keep a [StringMap] internally, and convert back and forth between
     the types [Nonterminal.t] and [string] when querying this map. This
     is not very elegant, and could be changed if desired. *)

  let declarations : Syntax.on_error_reduce_level StringMap.t =
    grammar.on_error_reduce

  let print (nt : Nonterminal.t) : string =
    Nonterminal.print false nt

  let lookup (nt : string) : Nonterminal.t =
    try
      Nonterminal.lookup nt
    with Not_found ->
      (* If this fails, then we have an [%on_error_reduce] declaration
         for an invalid symbol. *)
      assert false

  let reduce prod =
    let nt = Production.nt prod in
    StringMap.mem (print nt) declarations

  let iter f =
    StringMap.iter (fun nt _prec ->
      f (lookup nt)
    ) declarations

  open Precedence

  let preferable prod1 prod2 =
    (* The two productions that we are comparing must be distinct. *)
    assert (prod1 <> prod2);
    let nt1 = Production.nt prod1
    and nt2 = Production.nt prod2 in
    (* If they have the same left-hand side (which seems rather unlikely?),
       declare them incomparable. *)
    nt1 <> nt2 &&
    (* Otherwise, look up the priority levels associated with their left-hand
       symbols. *)
    let prec1, prec2 =
      try
        StringMap.find (print nt1) declarations,
        StringMap.find (print nt2) declarations
      with Not_found ->
        (* [preferable] should be used to compare two symbols for which
           there exist [%on_error_reduce] declarations. *)
        assert false
    in
    match production_order prec1 prec2 with
    | Gt ->
        (* [prec1] is a higher integer than [prec2], therefore comes later
           in the file. By analogy with [%left] and friends, we give higher
           priority to later declarations. *)
        true
    | Lt ->
        false
    | Eq
    | Ic ->
        (* We could issue a warning or an information message in these cases. *)
        false

end

(* ------------------------------------------------------------------------ *)
(* Facilities for printing sentences. *)

module Sentence = struct

  type sentence =
    Nonterminal.t option * Terminal.t list

  open Printf

  let print_abstract (nto, terminals) : string =
    Misc.with_buffer 128 (fun b ->
      Option.iter (fun nt ->
        bprintf b "%s: " (Nonterminal.print false nt)
      ) nto;
      let separator = Misc.once "" " " in
      List.iter (fun t ->
        bprintf b "%s%s" (separator()) (Terminal.print t)
      ) terminals;
      bprintf b "\n";
    )

  let print_concrete (_nto, terminals) : string =
    Misc.with_buffer 128 (fun b ->
      let separator = Misc.once "" " " in
      List.iter (fun t ->
        bprintf b "%s%s" (separator()) (Terminal.print_concrete t)
      ) terminals
    )

  let print style sentence =
    match style with
    | `Abstract ->
        print_abstract sentence
    | `Concrete ->
        print_concrete sentence

end

(* ------------------------------------------------------------------------ *)

end (* module Make *)
