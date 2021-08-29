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

 (* This module discovers information about the shape and content of the stack
    in each of the automaton's states. *)

open Grammar
module C = Conflict (* artificial dependency; ensures that [Conflict] runs first *)

let stack_symbols =
  let module SS = StackSymbols.Run() in
  SS.stack_symbols

let stack_height (node : Lr1.node) : int =
  Array.length (stack_symbols node)

(* ------------------------------------------------------------------------ *)
(* Above, we have computed a prefix of the stack at every state. We have
   computed the length of this prefix and the symbols that are held in
   this prefix of the stack. Now, compute which states may be held in this
   prefix. *)

(* In order to compute this information, we perform an analysis of the
   automaton, via a least fixed fixed point computation. *)

(* It is worth noting that it would be possible to use an analysis based on a
   least fixed point computation to discover at the same time the length of
   the stack prefix, the symbols that it contains, and the states that it may
   contain. This alternate approach, which was used until 2012/08/25, would
   lead us to discovering a richer invariant, that is, potentially longer
   prefixes. This extra information, however, was useless; computing it was a
   waste of time. Hence, as of 2012/08/25, the height of the stack prefix and
   the symbols that it contains are predicted (see above), and the least fixed
   point computation is used only to populate these prefixes of predictable
   length with state information. As of 2021/03/03, the submodule [Long] at
   the end of this file computes this richer invariant. (However, it computes
   symbols only, not sets of states.) *)

(* By the way, this least fixed point analysis remains the most costly
   computation throughout this module. *)

(* Our keys are the nodes of the LR(1) automaton. *)

module Key = struct
  type t = Lr1.node
  let n = Lr1.n
  let encode = Lr1.number
end

module KeyMap =
  Fix.Glue.InjectMinimalImperativeMaps
    (Fix.Glue.ArraysAsImperativeMaps(Key))
    (Key)

(* Vectors of sets of states. *)

module StateSetVector = struct

  type property =
    Lr1.NodeSet.t array

  let bottom height =
    Array.make height Lr1.NodeSet.empty

  let empty =
    [||]

  let leq_join v1 v2 =
    let n = Array.length v1 in
    (* Because all heights are known ahead of time, we are able (and careful)
       to compare only vectors of equal length. *)
    assert (n = Array.length v2);
    let v = Array.init n (fun i -> Lr1.NodeSet.leq_join v1.(i) v2.(i)) in
    if Misc.array_for_all2 (==) v2 v then v2 else v

  let push v x =
    (* Push [x] onto the right end of [v]. *)
    let n = Array.length v in
    Array.init (n+1) (fun i -> if i < n then v.(i) else x)

  let truncate k v =
    (* Keep a suffix of length [k] of [v]. *)
    let n = Array.length v in
    Array.sub v (n-k) k

  let print v =
    if Array.length v = 0 then
      "epsilon"
    else
      Misc.separated_list_to_string Lr1.NodeSet.print "; " (Array.to_list v)

  let iter =
    Array.iter

  let get =
    (* The index 0 corresponds to the cell that lies deepest in the stack. *)
    Array.get

end

open StateSetVector

(* Define the data flow graph. *)

module G = struct

  type variable = Lr1.node

  type property = StateSetVector.property

  (* At each start state of the automaton, the stack is empty. *)

  let foreach_root contribute =
    Lr1.entry |> ProductionMap.iter (fun _prod root ->
      assert (stack_height root = 0);
      contribute root empty
    )

  (* The edges of the data flow graph are the transitions of the automaton. *)

  let foreach_successor source stack contribute =
    Lr1.transitions source |> SymbolMap.iter (fun _symbol target ->
      (* The contribution of [source], through this edge, to [target], is the
         stack at [source], extended with a new cell for this transition, and
         truncated to the stack height at [target], so as to avoid obtaining a
         vector that is longer than expected/necessary. *)
      let cell = Lr1.NodeSet.singleton source
      and height = stack_height target in
      contribute target (truncate height (push stack cell))
    )

end

(* Compute the least fixed point. *)

let stack_states : Lr1.node -> property option =
  let module F = Fix.DataFlow.Run(KeyMap)(StateSetVector)(G) in
  F.solution

(* If every state is reachable, then the least fixed point must be non-[None]
   everywhere, so we may view it as a function that produces a vector of sets
   of states. *)

let stack_states (node : Lr1.node) : property =
  match stack_states node with
  | None ->
      (* apparently this node is unreachable *)
      assert false
  | Some v ->
      v

(* ------------------------------------------------------------------------ *)
(* From the above information, deduce, for each production, the states that
   may appear in the stack when this production is reduced. *)

(* We are careful to produce a vector of states whose length is exactly that
   of the production [prod]. *)

let production_states : Production.index -> property =
  Production.tabulate (fun prod ->
    let sites = Lr1.production_where prod in
    let height = Production.length prod in
    Lr1.NodeSet.fold (fun node accu ->
      leq_join (truncate height (stack_states node)) accu
    ) sites (bottom height)
  )

(* ------------------------------------------------------------------------ *)
(* If requested, print the information that has been computed above. *)

let () =
  Error.logC 3 (fun f ->
    Lr1.iter (fun node ->
      Printf.fprintf f "stack(%s) = %s\n"
        (Lr1.print node)
        (print (stack_states node))
    )
  )

let () =
  Error.logC 3 (fun f ->
    Production.iterx (fun prod ->
      Printf.fprintf f "stack when reducing %s = %s\n"
        (Production.print prod)
        (print (production_states prod))
    )
  )

(* ------------------------------------------------------------------------ *)
(* We now determine which states must be represented, that is,
   explicitly pushed onto the stack. For simplicity, a state is either
   always represented or never represented. More fine-grained
   strategies, where a single state is sometimes pushed onto the stack
   and sometimes not pushed, depending on which outgoing transition is
   being taken, are conceivable, but quite tricky, and probably not
   worth the trouble.

   (1) If two states are liable to appear within a single stack cell,
   then one is represented if and only if the other is
   represented. This ensures that the structure of stacks is known
   everywhere and that we can propose types for stacks.

   (2) If a state [s] has an outgoing transition along nonterminal
   symbol [nt], and if the [goto] table for symbol [nt] has more than
   one target, then state [s] is represented.

   (3) If a stack cell contains more than one state and if at least
   one of these states is able to handle the [error] token, then these
   states are represented.

   (4) If the semantic action associated with a production mentions
   the [$syntaxerror] keyword, then the state that is being reduced to
   (that is, the state that initiated the recognition of this
   production) is represented. (Indeed, it will be passed as an
   argument to [errorcase].) *)

(* Data. *)

let rep : bool UnionFind.point array =
  Array.init Lr1.n (fun _ -> UnionFind.fresh false)

(* Getter. *)

let represented state =
  rep.(Lr1.number state)

(* Setters. *)

let represent state =
  UnionFind.set (represented state) true

let represents states =
  represent (Lr1.NodeSet.choose states)

(* Enforce condition (1) above. *)

let share (v : property) =
  StateSetVector.iter (fun states ->
    let dummy = UnionFind.fresh false in
    Lr1.NodeSet.iter (fun state ->
      UnionFind.union dummy (represented state)
    ) states
  ) v

let () =
  Lr1.iter (fun node ->
    share (stack_states node)
  );
  Production.iter (fun prod ->
    share (production_states prod)
  )

(* Enforce condition (2) above. *)

let () =
  Nonterminal.iter (fun nt ->
    let count =
      Lr1.targets (fun count _ _ ->
        count + 1
      ) 0 (Symbol.N nt)
    in
    if count > 1 then
      Lr1.targets (fun () sources _ ->
        List.iter represent sources
      ) () (Symbol.N nt)
  )

(* Enforce condition (3) above. *)

let handler state =
  try
    let _ = SymbolMap.find (Symbol.T Terminal.error) (Lr1.transitions state) in
    true
  with Not_found ->
    try
      let _ = TerminalMap.lookup Terminal.error (Lr1.reductions state) in
      true
    with Not_found ->
      false

let handlers states =
  Lr1.NodeSet.exists handler states

let () =
  Lr1.iter (fun node ->
    let v = stack_states node in
    StateSetVector.iter (fun states ->
      if Lr1.NodeSet.cardinal states >= 2 && handlers states then
        represents states
    ) v
  )

(* Enforce condition (4) above. *)

let () =
  Production.iterx (fun prod ->
    if Action.has_syntaxerror (Production.action prod) then
      let sites = Lr1.production_where prod in
      let length = Production.length prod in
      if length = 0 then
        Lr1.NodeSet.iter represent sites
      else
        let states = StateSetVector.get (production_states prod) 0 in
        represents states
  )

(* Define accessors. *)

let represented state =
  UnionFind.get (represented state)

let representeds states =
  if Lr1.NodeSet.is_empty states then
    assert false
  else
    represented (Lr1.NodeSet.choose states)

(* Statistics. *)

let () =
  Error.logC 1 (fun f ->
    let count =
      Lr1.fold (fun count node ->
        if represented node then count + 1 else count
      ) 0
    in
    Printf.fprintf f "%d out of %d states are represented.\n" count Lr1.n
  )

(* If requested, show a detailed table of which states are represented. *)

let () =
  Error.logC 3 (fun f ->
    Lr1.iter (fun node ->
      Printf.fprintf f "represented(%s) = %b\n"
        (Lr1.print node) (represented node)
    )
  )

(* ------------------------------------------------------------------------ *)
(* Accessors for information about the stack. *)

(* We describe a stack prefix as a list of cells, where each cell is a pair
   of a symbol and a set of states. The top of the stack is the head of the
   list. *)

type cell =
    Symbol.t * Lr1.NodeSet.t

type word =
    cell list

(* This auxiliary function converts a stack-as-an-array (top of stack
   at the right end) to a stack-as-a-list (top of stack at list head). *)

let _convert a =
  let n = Array.length a in
  let rec loop i accu =
    if i = n then accu else loop (i + 1) (a.(i) :: accu)
  in
  loop 0 []

(* This auxiliary function converts a pair of stacks-as-arrays to a
   stack-as-a-list-of-pairs. *)

let convert2 a b =
  let n = Array.length a in
  assert (n = Array.length b);
  let rec loop i accu =
    if i = n then accu else loop (i + 1) ((a.(i), b.(i)) :: accu)
  in
  loop 0 []

(* [stack s] describes the stack when the automaton is in state [s]. *)

let stack node : word =
  convert2 (stack_symbols node) (stack_states node)

(* [prodstack prod] describes the stack when production [prod] is about to be
   reduced. *)

let prodstack prod : word =
  convert2 (Production.rhs prod) (production_states prod)

(* [gotostack nt] is the structure of the stack when a shift
   transition over nonterminal [nt] is about to be taken. It
   consists of just one cell. *)

let gotostack : Nonterminal.t -> word =
  Nonterminal.tabulate (fun nt ->
    let sources =
      Lr1.targets (fun accu sources _ ->
        List.fold_right Lr1.NodeSet.add sources accu
      ) Lr1.NodeSet.empty (Symbol.N nt)
    in
    [ Symbol.N nt, sources ]
  )

let fold f accu w =
  List.fold_right (fun (symbol, states) accu ->
    f accu (representeds states) symbol states
  ) w accu

let fold_top f accu w =
  match w with
  | [] ->
      accu
  | (symbol, states) :: _ ->
      f (representeds states) symbol

(* ------------------------------------------------------------------------ *)
(* Explain how the stack should be deconstructed when an error is found.

   We sometimes have a choice as to how many stack cells should be popped.
   Indeed, several cells in the known suffix of the stack may physically hold
   a state. If neither of these states handles errors, then we could jump to
   either. (Indeed, if we jump to one that's nearer, it will in turn pop
   further stack cells and jump to one that's farther.) In the interest of
   code size, we should pop as few stack cells as possible. So, we jump to the
   topmost represented state in the known suffix. *)

type state =
  | Represented
  | UnRepresented of Lr1.node

type instruction =
  | Die
  | DownTo of word * state

let rewind node : instruction =
  let w = stack node in

  let rec rewind w =
    match w with
    | [] ->

        (* I believe that every stack description either is definite
           (that is, ends with [TailEmpty]) or contains at least one
           represented state. Thus, if we find an empty [w], this
           means that the stack is definitely empty. *)

        Die

    | ((_, states) as cell) :: w ->

        if representeds states then

          (* Here is a represented state. We will pop this
             cell and no more. *)

          DownTo ([ cell ], Represented)

        else if handlers states then begin

          (* Here is an unrepresented state that can handle
             errors. The cell must hold a singleton set of states, so
             we know which state to jump to, even though it isn't
             represented. *)

          assert (Lr1.NodeSet.cardinal states = 1);
          let state = Lr1.NodeSet.choose states in
          DownTo ([ cell ], UnRepresented state)

        end
        else

          (* Here is an unrepresented state that does not handle
             errors. Pop this cell and look further. *)

          match rewind w with
          | Die ->
              Die
          | DownTo (w, st) ->
              DownTo (cell :: w, st)

  in
  rewind w

(* ------------------------------------------------------------------------ *)

(* Machinery for the computation of which symbols must keep track of their
   start or end positions. *)

open Keyword

type variable =
  Symbol.t * where (* WhereStart or WhereEnd *)

module M : Fix.IMPERATIVE_MAPS with type key = variable = struct
  type key = variable
  type 'data t = {
    mutable startp: 'data SymbolMap.t;
    mutable endp:   'data SymbolMap.t;
  }
  open SymbolMap
  let create() =
    { startp = empty; endp = empty }
  let clear m =
    m.startp <- empty; m.endp <- empty
  let add (sym, where) data m =
    match where with
    | WhereStart ->
        m.startp <- add sym data m.startp
    | WhereEnd ->
        m.endp <- add sym data m.endp
    | WhereSymbolStart ->
        assert false
  let find (sym, where) m =
    match where with
    | WhereStart ->
        find sym m.startp
    | WhereEnd ->
        find sym m.endp
    | WhereSymbolStart ->
        assert false
  let iter f m =
    iter (fun sym -> f (sym, WhereStart)) m.startp;
    iter (fun sym -> f (sym, WhereEnd)) m.endp
end

(* ------------------------------------------------------------------------ *)

(* We now determine which positions must be kept track of. For simplicity, we
   do this on a per-symbol basis. That is, for each symbol, either we never
   keep track of position information, or we always do. In fact, we do
   distinguish start and end positions. This leads to computing two sets of
   symbols -- those that keep track of their start position and those that
   keep track of their end position.

   A symbol on the right-hand side of a production must keep track of its
   (start or end) position if that position is explicitly requested by a
   semantic action.

   Furthermore, if the left-hand symbol of a production must keep track of its
   start (resp. end) position, then the first (resp. last) symbol of its
   right-hand side (if there is one) must do so as well. That is, unless the
   right-hand side is empty. *)

(* 2015/11/11. When a production [prod] is reduced, the top stack cell may be
   consulted for its end position. This implies that this cell must exist
   and must store an end position! Now, when does this happen?

   1- This happens if [prod] is an epsilon production and the left-hand symbol
      of the production, [nt prod], keeps track of its start or end position.
   2- This happens if the semantic action explicitly mentions the keyword
      [$endpos($0)].

   Now, if this happens, what should we do?

   a- If this happens in a state [s] whose incoming symbol is [sym], then [sym]
      must keep track of its end position.
   b- If this happens in an initial state, where the stack may be empty, then
      the sentinel cell at the bottom of the stack must contain an end position.

   Point (b) doesn't concern us here, but point (a) does. We must implement the
   constraint (1) \/ (2) -> (a). Point (b) is taken care of in the code back-end,
   where, for simplicity, we always create a sentinel cell. *)

(* I will say that this is a lot more sophisticated than I would like. The code
   back-end has been known for its efficiency and I am trying to maintain this
   property -- in particular, I would like to keep track of no positions at all,
   if the user doesn't use any position keyword. But I am suffering. *)

module F =
  FixSolver.Make(M)(Fix.Prop.Boolean)

let () =

  (* We gather the constraints explained above in two loops. The first loop
     looks at every (non-start) production [prod]. The second loop looks at
     every (non-initial) state [s]. *)

  Production.iterx (fun prod ->

    let nt, rhs = Production.def prod
    and ids = Production.identifiers prod
    and action = Production.action prod in
    let length = Array.length rhs in

    if length > 0 then begin
      (* If [nt] keeps track of its start position, then the first symbol
         in the right-hand side must do so as well. *)
      F.record_VarVar (Symbol.N nt, WhereStart) (rhs.(0), WhereStart);
      (* If [nt] keeps track of its end position, then the last symbol
         in the right-hand side must do so as well. *)
      F.record_VarVar (Symbol.N nt, WhereEnd) (rhs.(length - 1), WhereEnd)
    end;

    KeywordSet.iter (function
      | SyntaxError ->
          ()
      | Position (Before, _, _) ->
          (* Doing nothing here because [$endpos($0)] is dealt with in
             the second loop. *)
          ()
      | Position (Left, _, _) ->
          (* [$startpos] and [$endpos] have been expanded away. *)
          assert false
      | Position (_, _, FlavorLocation) ->
          (* [$loc] and [$sloc] have been expanded away. *)
          assert false
      | Position (RightNamed _, WhereSymbolStart, _) ->
          (* [$symbolstartpos(x)] does not exist. *)
          assert false
      | Position (RightNamed id, where, _) ->
          (* If the semantic action mentions [$startpos($i)], then the
             [i]-th symbol in the right-hand side must keep track of
             its start position. Similarly for end positions. *)
          Array.iteri (fun i id' ->
            if id = id' then
              F.record_ConVar true (rhs.(i), where)
          ) ids
    ) (Action.keywords action)

  ); (* end of loop on productions *)

  Lr1.iterx (fun s ->
    (* Let [sym] be the incoming symbol of state [s]. *)
    let sym = Option.force (Lr1.incoming_symbol s) in

    (* Condition (1) in the long comment above (2015/11/11). If an epsilon
       production [prod] can be reduced in state [s], if its left-hand side
       [nt] keeps track of its start or end position, then [sym] must keep
       track of its end position. *)
    TerminalMap.iter (fun _ prods ->
      let prod = Misc.single prods in
      let nt, rhs = Production.def prod in
      let length = Array.length rhs in
      if length = 0 then begin
        F.record_VarVar (Symbol.N nt, WhereStart) (sym, WhereEnd);
        F.record_VarVar (Symbol.N nt, WhereEnd) (sym, WhereEnd)
      end
    ) (Lr1.reductions s);

    (* Condition (2) in the long comment above (2015/11/11). If a production
       can be reduced in state [s] and mentions [$endpos($0)], then [sym]
       must keep track of its end position. *)
    if Lr1.has_beforeend s then
      F.record_ConVar true (sym, WhereEnd)

  )

let track : variable -> bool option =
  let module S = F.Solve() in
  S.solution

let track : variable -> bool =
  fun x -> Option.value (track x) ~default:false

let startp symbol =
  track (symbol, WhereStart)

let endp symbol =
  track (symbol, WhereEnd)

let for_every_symbol (f : Symbol.t -> unit) : unit =
  Terminal.iter (fun t -> f (Symbol.T t));
  Nonterminal.iter (fun nt -> f (Symbol.N nt))

let sum_over_every_symbol (f : Symbol.t -> bool) : int =
  let c = ref 0 in
  for_every_symbol (fun sym -> if f sym then c := !c + 1);
  !c

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
      "%d out of %d symbols keep track of their start position.\n\
       %d out of %d symbols keep track of their end position.\n"
        (sum_over_every_symbol startp) (Terminal.n + Nonterminal.n)
        (sum_over_every_symbol endp) (Terminal.n + Nonterminal.n))

(* ------------------------------------------------------------------------- *)
(* Miscellaneous. *)

let universal symbol =
  Lr1.fold (fun universal s ->
    universal && (if represented s then SymbolMap.mem symbol (Lr1.transitions s) else true)
  ) true

(* ------------------------------------------------------------------------ *)
(* Discover which states can peek at an error. These are the states
   where an error token may be on the stream. These are the states
   that are targets of a reduce action on [error]. *)

(* 2012/08/25 I am optimizing this code, whose original version I found had
   quadratic complexity. The problem is as follows. We can easily iterate over
   all states to find which states [s] have a reduce action on error. What we
   must find out, then, is into which state [t] this reduce action takes us.
   This is not easy to predict, as it depends on the contents of the stack.
   The original code used an overapproximation, as follows: if the reduction
   concerns a production whose head symbol is [nt], then all of the states
   that have an incoming transition labeled [nt] are potential targets. The
   new version of the code below relies on the same approximation, but uses
   two successive loops instead of two nested loops. *)

let errorpeekers =
  (* First compute a set of symbols [nt]... *)
  let nts : SymbolSet.t =
    Lr1.fold (fun nts node ->
      try
        let prods = TerminalMap.lookup Terminal.error (Lr1.reductions node) in
        let prod = Misc.single prods in
        let nt = Production.nt prod in
        SymbolSet.add (Symbol.N nt) nts
      with Not_found ->
        nts
    ) SymbolSet.empty
  in
  (* ... then compute the set of all target states of all transitions
     labeled by some symbol in the set [nt]. *)
  SymbolSet.fold (fun nt errorpeekers ->
    Lr1.targets (fun errorpeekers _ target ->
      Lr1.NodeSet.add target errorpeekers
    ) errorpeekers nt
  ) nts Lr1.NodeSet.empty

let errorpeeker node =
  Lr1.NodeSet.mem node errorpeekers

(* ------------------------------------------------------------------------ *)

let () =
  Time.tick "Constructing the invariant"

(* ------------------------------------------------------------------------ *)

(* The submodule [Long] computes the known suffix of the stack in each state,
   as a vector of symbols, and it computes a suffix that is as long as
   possible, in contrast with the above code, which computes a suffix whose
   length is predicted by the function [stack_height]. *)

module Long = struct

let debug = true

(* Vectors of symbols. *)

module SymbolVector = struct

  (* As in the right-hand side of a production, the top of the stack
     is the right end of the array. *)

  type property =
    Symbol.t array

  let empty =
    [||]

  let truncate k v =
    (* Keep a suffix of length [k] of [v]. *)
    let n = Array.length v in
    Array.sub v (n-k) k

  (* Given two arrays [v1] and [v2] of lengths [n1] and [n2], the function
     call [lcs v1 v2 n1 n2 (min n1 n2) 0] computes the greatest [k] such that
     [truncate k v1] and [truncate k v2] are equal. *)

  let rec lcs v1 v2 n1 n2 n k =
    (* [n] is [min n1 n2]. *)
    if k = n || v1.(n1 - 1 - k) <> v2.(n2 - 1 - k) then k
    else lcs v1 v2 n1 n2 n (k + 1)

  let leq_join v1 v2 =
    let n1 = Array.length v1
    and n2 = Array.length v2 in
    let n = min n1 n2 in
    let k = lcs v1 v2 n1 n2 n 0 in
    if debug then assert (truncate k v1 = truncate k v2);
    if k = n2 then v2
    else if k = n1 then v1
    else truncate k v1

  let push v x =
    (* Push [x] onto the right end of [v]. *)
    let n = Array.length v in
    Array.init (n+1) (fun i -> if i < n then v.(i) else x)

  let print v =
    if Array.length v = 0 then
      "epsilon"
    else
      Misc.separated_list_to_string Symbol.print "; " (Array.to_list v)

end

open SymbolVector

(* Define the data flow graph. *)

module G = struct

  type variable = Lr1.node

  type property = SymbolVector.property

  (* At each start state of the automaton, the stack is empty. *)

  let foreach_root contribute =
    Lr1.entry |> ProductionMap.iter (fun _prod root ->
      assert (stack_height root = 0);
      contribute root empty
    )

  (* The edges of the data flow graph are the transitions of the automaton. *)

  let foreach_successor source stack contribute =
    Lr1.transitions source |> SymbolMap.iter (fun symbol target ->
      (* The contribution of [source], through this edge, to [target], is the
         stack at [source], extended with a new cell for this transition. *)
      contribute target (push stack symbol)
    )

end

(* Compute the least fixed point. *)

let stack : Lr1.node -> property option =
  let module F = Fix.DataFlow.Run(KeyMap)(SymbolVector)(G) in
  F.solution

(* If every state is reachable, then the least fixed point must be non-[None]
   everywhere, so we may view it as a function that produces a vector of
   symbols. *)

let stack (node : Lr1.node) : property =
  match stack node with
  | None ->
      (* apparently this node is unreachable *)
      assert false
  | Some v ->
      v

(* ------------------------------------------------------------------------ *)
(* If requested, print the information that has been computed above. *)

let () =
  Error.logC 3 (fun f ->
    Lr1.iter (fun node ->
      Printf.fprintf f "longstack(%s) = %s\n"
        (Lr1.print node)
        (print (stack node))
    )
  )

end (* module Long *)

let () =
  Time.tick "Constructing the long invariant"
