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

(* As announced in our specification, we ignore the [error] token. We never
   work with the terminal symbol [#] either. This symbol never appears in the
   maps returned by [Lr1.transitions] and [Lr1.reductions]. Thus, in
   principle, we work with real terminal symbols only. However, we encode
   [any] as [#] -- see below. *)

(* NOTE: Because he performance impact of the assertions in this file is about
   10%, they are turned off by default. Change the value of [debug] to [true]
   if you wish to enable assertions. *)

let debug = false

open Grammar
open Default

(* ------------------------------------------------------------------------ *)

(* We introduce a pseudo-terminal symbol [any]. It is used in several places
   later on, in particular in the [lookahead] field of a fact, to encode the
   absence of a lookahead hypothesis -- i.e., any terminal symbol will do. *)

(* We choose to encode [any] as [#]. There is no risk of confusion, since we
   do not use [#] anywhere. Thus, the assertion [Terminal.real z] implies
   [z <> any]. *)

let any =
  Terminal.sharp

(* [foreach_terminal f] applies the function [f] to every terminal symbol in
   turn, except [error] and [#]. *)

let foreach_terminal =
  Terminal.iter_real

(* [foreach_terminal_not_causing_an_error s f] applies the function [f] to
   every terminal symbol [z] such that [causes_an_error s z] is false. This
   could be implemented in a naive manner using [foreach_terminal] and
   [causes_an_error]. This implementation is significantly more efficient. *)

let foreach_terminal_not_causing_an_error s f =
  match has_default_reduction s with
  | Some _ ->
      (* There is a default reduction. No symbol causes an error. *)
      foreach_terminal f
  | None ->
      (* Enumerate every terminal symbol [z] for which there is a
         reduction. *)
      TerminalMap.iter (fun z _ ->
        (* A reduction on [#] is always a default reduction. (See [lr1.ml].) *)
        if debug then assert (not (Terminal.equal z Terminal.sharp));
        if Terminal.non_error z then
          f z
      ) (Lr1.reductions s);
      (* Enumerate every terminal symbol [z] for which there is a
         transition. *)
      SymbolMap.iter (fun sym _ ->
        match sym with
        | Symbol.T z ->
            if debug then assert (not (Terminal.equal z Terminal.sharp));
            if Terminal.non_error z then
              f z
        | Symbol.N _ ->
            ()
      ) (Lr1.transitions s)

(* Let us say a state [s] is solid if its incoming symbol is a terminal symbol
   (or if it has no incoming symbol at all, i.e., it is an initial state). It
   is fragile if its incoming symbol is a non-terminal symbol. *)

let is_solid s =
  match Lr1.incoming_symbol s with
  | None
  | Some (Symbol.T _) ->
      true
  | Some (Symbol.N _) ->
      false

(* ------------------------------------------------------------------------ *)

(* To delay the side effects performed by this module, we wrap everything in
   in a big functor. The functor also serves to pass verbosity parameters. *)

module Run (X : sig
  (* If [verbose] is set, produce various messages on [stderr]. *)
  val verbose: bool
end) = struct

(* ------------------------------------------------------------------------ *)

(* Because of our encoding of terminal symbols as 8-bit characters, this
   algorithm supports at most 256 terminal symbols. *)

let () =
  if Terminal.n > 256 then
    Error.error []
      "the reachability analysis supports at most 256 terminal symbols.\n\
       The grammar has %d terminal symbols." Terminal.n

(* ------------------------------------------------------------------------ *)

(* Produce a warning if the grammar uses the [error] pseudo-token. *)

let () =
  if grammar_uses_error_token then
    Error.warning []
      "The reachability analysis ignores all productions that involve the error token."

(* ------------------------------------------------------------------------ *)

(* Build a module that represents words as (hash-consed) strings. Note:
   this functor application has a side effect (it allocates memory, and
   more importantly, it may fail). *)

module W = Terminal.Word(struct end)

(* ------------------------------------------------------------------------ *)

(* Instantiate [Trie]. This allocates fresh mutable state, but otherwise has
   no effect. The construction of the tries actually takes place when
   [Trie.stars] is invoked below. *)

module Trie =
  Trie.Make(struct end)

(* ------------------------------------------------------------------------ *)

(* The main algorithm, [LRijkstra], accumulates facts. A fact is a triple of a
   [position] (that is, a sub-trie), a [word], and a [lookahead] assumption.
   Such a fact means that this [position] can be reached, from the source
   state [Trie.source position], by consuming [word], under the assumption
   that the next input symbol is [lookahead]. *)

(* We allow [lookahead] to be [any] so as to indicate that this fact does
   not have a lookahead assumption. *)

(*

type fact = {
  position: Trie.trie;
  word: W.word;
  lookahead: Terminal.t (* may be [any] *)
}

*)

(* To save memory (and therefore time), we encode a fact in a single OCaml
   integer value. This is made possible by the fact that tries, words, and
   terminal symbols are represented as (or can be encoded as) integers.
   This admittedly horrible hack allows us to save roughly a factor of 2
   in space, and to gain 10% in time. *)

type fact = int

let dummy : fact =
  -1 (* should never be accessed! *)

(* Encoding and decoding facts. *)

(* We encode [position|word|lookahead] in a single word of memory. *)

(* The lookahead symbol fits in 8 bits. *)

(* In the largest grammars that we have seen, the number of unique words is
   about 3.10^5, so a word should fit in about 19 bits (2^19 = 524288). In the
   largest grammars that we have seen, the total star size is about 64000, so a
   trie should fit in about 17 bits (2^17 = 131072). *)

(* On a 64-bit machine, we have ample space in a 63-bit word! We allocate 30
   bits for [word] and the rest (i.e., 25 bits) for [position]. *)

(* On a 32-bit machine, we are a bit more cramped! In Menhir's own fancy-parser,
   the number of terminal symbols is 27, the number of unique words is 566, and
   the total star size is 546. We allocate 12 bits for [word] and 11 bits for
   [position]. This is better than refusing to work altogether, but still not
   great. A more satisfactory approach might be to revert to heap allocation of
   facts when in 32-bit mode, but that would make the code somewhat ugly. *)

let w_lookahead =
  8

let w_word =
  if Sys.word_size < 64 then 12 else 30

let w_position  =
  Sys.word_size - 1 - (w_word + w_lookahead) (* 25, on a 64-bit machine *)

let identity (fact : fact) : int =
  if debug then assert (fact <> dummy);
  fact lsr (w_word + w_lookahead)

let position (fact : fact) : Trie.trie =
  if debug then assert (fact <> dummy);
  Trie.decode (identity fact)

let word (fact : fact) : W.word =
  if debug then assert (fact <> dummy);
  (fact lsr w_lookahead) land (1 lsl w_word - 1)

let lookahead (fact : fact) : Terminal.t =
  Terminal.i2t (fact land (1 lsl w_lookahead - 1))

let mkfact position (word : W.word) lookahead =
  let position : int = Trie.encode position
  and word : int = word
  and lookahead : int = Terminal.t2i lookahead in
  if debug then begin
    assert (0 <= position && 0 <= word && 0 <= lookahead);
    assert (lookahead < 1 lsl w_lookahead);
  end;
  if position < 1 lsl w_position && word < 1 lsl w_word then
    (* [lsl] binds tighter than [lor] *)
    (position lsl w_word lor word) lsl w_lookahead lor lookahead
  else
    let advice =
       if Sys.word_size < 64 then
         "Please use a 64-bit machine."
       else
         "Please report this error to Menhir's developers."
    in
    Error.error []
      "an internal limit was exceeded.\n\
       Sys.word_size = %d. Position = %d. Word = %d.\n\
       %s%!"
      Sys.word_size position word advice

let mkfact p w l =
  let fact = mkfact p w l in
  if debug then begin
    assert (word fact == w);      (* round-trip property *)
    assert (lookahead fact == l); (* round-trip property *)
    assert (position fact == p);  (* round-trip property *)
  end;
  fact

(* Two invariants reduce the number of facts that we consider:

   1. If [lookahead] is a real terminal symbol [z] (i.e., not [any]),
      then [z] does not cause an error in the [current] state.
      It would be useless to consider a fact that violates this property;
      this cannot possibly lead to a successful reduction. In practice,
      this refinement allows reducing the number of facts that go through
      the queue by a factor of two.

   2. [lookahead] is [any] iff the [current] state is
      solid. This sounds rather reasonable (when a state is entered
      by shifting, it is entered regardless of which symbol follows)
      and simplifies the implementation of the sub-module [F].

*)

let invariant1 position _word lookahead =
  let current = Trie.current position in
  lookahead = any || not (causes_an_error current lookahead)

let invariant2 position _word lookahead =
  let current = Trie.current position in
  (lookahead = any) = is_solid current

(* [compatible z a] checks whether the terminal symbol [a] satisfies the
   lookahead assumption [z] -- which can be [any]. *)

let compatible z a =
  if debug then begin
    assert (Terminal.non_error z);
    assert (Terminal.real a);
  end;
  z = any || z = a

(* ------------------------------------------------------------------------ *)

(* As in Dijkstra's algorithm, a priority queue contains the facts that await
   examination. The length of [word fact] serves as the priority of a fact.
   This guarantees that we discover shortest paths. (We never insert into the
   queue a fact whose priority is less than the priority of the last fact
   extracted out of the queue.) *)

(* [LowIntegerPriorityQueue] offers very efficient operations (essentially
   constant time, for a small constant). It exploits the fact that priorities
   are low nonnegative integers. *)

module Q = LowIntegerPriorityQueue

let q =
  Q.create dummy

(* In principle, there is no need to insert the fact into the queue if [F]
   already stores a comparable fact. We could perform this test in [enqueue].
   However, a few experiments suggests that this is not worthwhile. The run
   time augments (because membership in [F] is tested twice, upon inserting
   and upon extracting) and the memory consumption does not seem to go down
   significantly. *)

let enqueue position word lookahead =
  (* [lookahead] can be [any], but cannot be [error] *)
  if debug then begin
    assert (Terminal.non_error lookahead);
    assert (invariant1 position word lookahead);
    assert (invariant2 position word lookahead);
  end;
  (* The length of [word] serves as the priority of this fact. *)
  let priority = W.length word in
  (* Encode and enqueue this fact. *)
  Q.add q (mkfact position word lookahead) priority

(* ------------------------------------------------------------------------ *)

(* Construct the [star] of every state [s]. Initialize the priority queue. *)

let () =
  (* For every state [s], if the trie rooted at [s] is nontrivial, ... *)
  Trie.stars (fun s position ->
    (* ...then insert an initial fact into the priority queue. *)
    (* In order to respect invariants 1 and 2, we must distinguish two
       cases. If [s] is solid, then we insert a single fact, whose
       lookahead assumption is [any]. Otherwise, we must insert one
       initial fact for every terminal symbol [z] that does not cause
       an error in state [s]. *)
    let word = W.epsilon in
    if is_solid s then
      enqueue position word any
    else
      foreach_terminal_not_causing_an_error s (fun z ->
        enqueue position word z
      )
  );
  if X.verbose then
    Trie.verbose()

(* ------------------------------------------------------------------------ *)

(* The module [F] maintains a set of known facts. *)

(* Three aspects of a fact are of particular interest:
   - its position [position], given by [position fact];
   - its first symbol [a], given by [W.first (word fact) (lookahead fact)];
   - its lookahead assumption [z], given by [lookahead fact].

   For every triple of [position], [a], and [z], we store at most one fact,
   (whose word has minimal length). Indeed, we are not interested in keeping
   track of several words that produce the same effect. Only the shortest such
   word is of interest.

   Thus, the total number of facts accumulated by the algorithm is at most
   [T.n^2], where [T] is the total size of the tries that we have constructed,
   and [n] is the number of terminal symbols. (This number can be quite large.
   [T] can be in the tens of thousands, and [n] can be over one hundred. These
   figures lead to a theoretical upper bound of 100M. In practice, for T=25K
   and n=108, we observe that the algorithm gathers about 7M facts.) *)

module F : sig

  (* [register fact] registers the fact [fact]. It returns [true] if this fact
     is new, i.e., no fact concerning the same triple of [position], [a], and
     [z] was previously known. *)
  val register: fact -> bool

  (* [query current z f] enumerates all known facts whose current state is
     [current] and whose lookahead assumption is compatible with [z]. The
     symbol [z] must a real terminal symbol, i.e., cannot be [any]. *)
  val query: Lr1.node -> Terminal.t -> (fact -> unit) -> unit

  (* [size()] returns the number of facts currently stored in the set. *)
  val size: unit -> int

  (* [verbose()] outputs debugging & performance information. *)
  val verbose: unit -> unit

end = struct

  (* We need to query the set of facts in two ways. In [register], we must test
     whether a proposed triple of [position], [a], [z] already appears in the
     set. In [query], we must find all facts that match a pair [current, z],
     where [current] is a state. (Note that [position] determines [current], but
     the converse is not true: a position contains more information besides the
     current state.)

     To address these needs, we use a two-level table. The first level is a
     matrix indexed by [current] and [z]. At the second level, we find sets of
     facts, where two facts are considered equal if they have the same triple of
     [position], [a], and [z]. In fact, we know at this level that all facts
     have the same [z] component, so only [position] and [a] are compared.

     Because our facts satisfy invariant 2, [z] is [any] if and only if the
     state [current] is solid. This means that we are wasting quite a
     lot of space in the matrix (for a solid state, the whole line is empty,
     except for the [any] column). *)

  (* The level-2 sets. *)

  module M =
    MySet.Make(struct
      type t = fact
      let compare fact1 fact2 =
        if debug then assert (lookahead fact1 = lookahead fact2);
        (* Compare the two positions first. This can be done without going
           through [Trie.decode], by directly comparing the two integer
           identities. *)
        let c = Generic.compare (identity fact1) (identity fact2) in
        if debug then assert (c = Trie.compare (position fact1) (position fact2));
        if c <> 0 then c else
        let z = lookahead fact1 in
        let a1 = W.first (word fact1) z
        and a2 = W.first (word fact2) z in
        (* note: [a1] and [a2] can be [any] here *)
        Terminal.compare a1 a2
    end)

  (* The level-1 matrix. *)

  let table =
    Array.make (Lr1.n * Terminal.n) M.empty

  let index current z =
    Terminal.n * (Lr1.number current) + Terminal.t2i z

  let count = ref 0

  let register fact =
    let current = Trie.current (position fact) in
    let z = lookahead fact in
    let i = index current z in
    let m = table.(i) in
    (* We crucially rely on the fact that [M.add] guarantees not to
       change the set if an ``equal'' fact already exists. Thus, a
       later, longer path is ignored in favor of an earlier, shorter
       path. *)
    let m' = M.add fact m in
    m != m' && begin
      incr count;
      table.(i) <- m';
      true
    end

  let query current z f =
    if debug then assert (not (Terminal.equal z any));
    (* If the state [current] is solid then the facts that concern it are
       stored in the column [any], and all of them are compatible with [z].
       Otherwise, they are stored in all columns except [any], and only
       those stored in the column [z] are compatible with [z]. *)
    let i = index current (if is_solid current then any else z) in
    let m = table.(i) in
    M.iter f m

  let size () =
    !count

  let verbose () =
    Printf.eprintf "F stores %d facts.\n%!" (size())

end

(* ------------------------------------------------------------------------ *)

(* The module [E] is in charge of recording the non-terminal edges that we have
   discovered, or more precisely, the conditions under which these edges can be
   taken.

   It maintains a set of quadruples [s, nt, w, z], where such a quadruple means
   that in the state [s], the outgoing edge labeled [nt] can be taken by
   consuming the word [w], under the assumption that the next symbol is [z].

   Again, the terminal symbol [a], given by [W.first w z], plays a role. For
   each quadruple [s, nt, a, z], we store at most one quadruple [s, nt, w, z].
   Thus, internally, we maintain a mapping of [s, nt, a, z] to [w].

   For greater simplicity, we do not allow [z] to be [any] in [register] or
   [query]. Allowing it would complicate things significantly, it seems. *)

module E : sig

  (* [register s nt w z] records that, in state [s], the outgoing edge labeled
     [nt] can be taken by consuming the word [w], if the next symbol is [z].
     It returns [true] if this information is new, i.e., if the underlying
     quadruple [s, nt, a, z] is new. The symbol [z] cannot be [any]. *)
  val register: Lr1.node -> Nonterminal.t -> W.word -> Terminal.t -> bool

  (* [query s nt a foreach] enumerates all words [w] and all real symbols [z]
     such that, in state [s], the outgoing edge labeled [nt] can be taken by
     consuming the word [w], under the assumption that the next symbol is [z],
     and the first symbol of the word [w.z] is [a]. The symbol [a] can be [any].
     The function [foreach] can be either [foreach_terminal] or of the form
     [foreach_terminal_not_causing_an_error _]. It limits the symbols [z] that
     are considered. *)
  val query: Lr1.node -> Nonterminal.t -> Terminal.t ->
             (* foreach: *) ((Terminal.t -> unit) -> unit) ->
             (W.word -> Terminal.t -> unit) -> unit

  (* [size()] returns the number of edges currently stored in the set. *)
  val size: unit -> int

  (* [verbose()] outputs debugging & performance information. *)
  val verbose: unit -> unit

end = struct

  (* At a high level, we must implement a mapping of [s, nt, a, z] to [w]. In
     practice, we can implement this specification using any combination of
     arrays, hash tables, balanced binary trees, and perfect hashing (i.e.,
     packing several of [s], [nt], [a], [z] in one word.) Here, we choose to
     use an array, indexed by [s], of hash tables, indexed by a key that packs
     [nt], [a], and [z] in one word. According to a quick experiment, the
     final population of the hash table [table.(index s)] seems to be roughly
     [Terminal.n * Trie.size s]. We note that using an initial capacity
     of 0 and relying on the hash table's resizing mechanism has a significant
     cost, which is why we try to guess a good initial capacity. *)

  module H = Hashtbl

  let table =
    Array.init Lr1.n (fun i ->
      let size = Trie.size i in
      H.create (if size = 1 then 0 else Terminal.n * size)
    )

  let index s =
    Lr1.number s

  let pack nt a z : int =
    (* We rely on the fact that we have at most 256 terminal symbols. *)
    (Nonterminal.n2i nt lsl 16) lor
    (Terminal.t2i a lsl 8) lor
    (Terminal.t2i z)

  let count = ref 0

  let register s nt w z =
    if debug then assert (Terminal.real z);
    let i = index s in
    let m = table.(i) in
    let a = W.first w z in
    (* Note that looking at [a] in state [s] cannot cause an error. *)
    if debug then assert (not (causes_an_error s a));
    let key = pack nt a z in
    if H.mem m key then
      false
    else begin
      incr count;
      H.add m key w;
      true
    end

  let rec query s nt a foreach f =
    if Terminal.equal a any then begin
      (* If [a] is [any], we query the table for every real symbol [a].
         We can limit ourselves to symbols that do not cause an error
         in state [s]. Those that do certainly do not have an entry;
         see the assertion in [register] above. *)
      foreach_terminal_not_causing_an_error s (fun a ->
        query s nt a foreach f
      )
    end
    else
      let i = index s in
      let m = table.(i) in
      foreach (fun z ->
        if debug then assert (Terminal.real z);
        let key = pack nt a z in
        match H.find m key with
        | w -> f w z
        | exception Not_found -> ()
      )

  let size () =
    !count

  let verbose () =
    Printf.eprintf "E stores %d edges.\n%!" (size())

end

(* ------------------------------------------------------------------------ *)

(* [new_edge s nt w z] is invoked when we discover that in the state [s], the
   outgoing edge labeled [nt] can be taken by consuming the word [w], under
   the assumption that the next symbol is [z]. We check whether this quadruple
   already exists in the set [E]. If not, then we add it, and we compute its
   consequences, in the form of new facts, which we insert into the priority
   queue for later examination. *)

let new_edge s nt w z =
  if debug then assert (Terminal.real z);
  if E.register s nt w z then
    let sym = Symbol.N nt in
    (* Query [F] for existing facts which could be extended by following
       this newly discovered edge. They must be facts whose current state
       is [s] and whose lookahead assumption is compatible with [a]. For
       each such fact, ... *)
    F.query s (W.first w z) (fun fact ->
      if debug then assert (compatible (lookahead fact) (W.first w z));
      (* ... try to take one step in the trie along an edge labeled [nt]. *)
      match Trie.step sym (position fact) with
      | position ->
          (* This takes us to a new state whose incoming symbol is [nt].
             Hence, this state is not solid. In order to satisfy invariant 2,
             we must create fact whose lookahead assumption is not [any].
             That's fine, since our lookahead assumption is [z]. In order to
             satisfy invariant 1, we must check that [z] does not cause an
             error in this state. *)
          if debug then assert (not (is_solid (Trie.current position)));
          if not (causes_an_error (Trie.current position) z) then
            let word = W.append (word fact) w in
            enqueue position word z
      | exception Not_found ->
          (* Could not take a step in the trie. This means this branch
             leads nowhere of interest, and was pruned when the trie
             was constructed. *)
          ()
    )

(* ------------------------------------------------------------------------ *)

(* [new_fact fact] is invoked when we discover a new fact (i.e., one that was
   not previously known). It studies the consequences of this fact. These
   consequences are of two kinds:

   - As in Dijkstra's algorithm, the new fact can be viewed as a newly
     discovered vertex. We study its (currently known) outgoing edges,
     and enqueue new facts in the priority queue.

   - Sometimes, a fact can also be viewed as a newly discovered edge. This is
     the case when the word that took us from [source] to [current]
     represents a production of the grammar and [current] is willing to
     reduce this production. We record the existence of this edge, and
     re-inspect any previously discovered vertices which are interested in
     this outgoing edge. *)

let new_fact fact =

  (* Throughout this rather long function, there is just one [fact]. Let's
     name its components right now, so as to avoid accessing them several
     times. (That could be costly, as it requires decoding the fact.) *)
  let position = position fact
  and lookahead = lookahead fact
  and word = word fact in
  let source = Trie.source position
  and current = Trie.current position in

  (* 1. View [fact] as a vertex. Examine the transitions out of [current].
     For every transition labeled by a symbol [sym] and into a state
     [target], ... *)

  Lr1.transitions current |> SymbolMap.iter (fun sym target ->
    (* ... try to follow this transition in the trie [position],
       down to a child which we call [child]. *)
    match Trie.step sym position, sym with

    | exception Not_found ->

        (* Could not take a step in the trie. This means this transition
           leads nowhere of interest. *)
        ()

    | child, Symbol.T t ->

        (* 1a. The transition exists in the trie, and [sym] is in fact a
           terminal symbol [t]. We note that [t] cannot be the [error] token,
           because the trie does not have any edges labeled [error]. *)
        if debug then begin
          assert (Lr1.Node.compare (Trie.current child) target = 0);
          assert (is_solid target);
          assert (Terminal.non_error t);
        end;

        (* If the lookahead assumption [lookahead] is compatible with
           [t], then we derive a new fact, where one more edge has been taken,
           and enqueue this new fact for later examination. *)

        (* The state [target] is solid, i.e., its incoming symbol is terminal.
           This state is always entered without consideration for the next
           lookahead symbol. Thus, we can use [any] as the lookahead assumption
           in the new fact that we produce. If we did not have [any], we would
           have to produce one fact for every possible lookahead symbol. *)

        if compatible lookahead t then
          let word = W.append word (W.singleton t) in
          enqueue child word any

    | child, Symbol.N nt ->

        (* 1b. The transition exists in the trie, and [sym] is in fact a
           nonterminal symbol [nt]. *)
        if debug then begin
          assert (Lr1.Node.compare (Trie.current child) target = 0);
          assert (not (is_solid target));
        end;

        (* We need to know how this nonterminal edge can be taken. We query
           [E] for a word [w] that allows us to take this edge. In general,
           the answer depends on the terminal symbol [z] that comes *after*
           this word: we try all such symbols. We must make sure that the
           first symbol of the word [w.z] satisfies the lookahead assumption
           [lookahead]; this is ensured by passing this information to
           [E.query]. *)

        (* It could be the case that, due to a default reduction, the answer
           to our query does not depend on [z], and we are wasting work.
           However, allowing [z] to be [any] in [E.query], and taking
           advantage of this to increase performance, seems difficult. *)

        let foreach = foreach_terminal_not_causing_an_error target in
        E.query current nt lookahead foreach (fun w z ->
          if debug then assert (compatible lookahead (W.first w z));
          let word = W.append word w in
          enqueue child word z
        )

  );

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [source] to the [current] state represents a production [prod] and
     [current] is willing to reduce this production. Then, reducing [prod]
     takes us all the way back to [source]. Thus, this production gives
     rise to an edge labeled [nt] -- the left-hand side of [prod] -- out of
     [source]. *)

  let z = lookahead in
  if not (Terminal.equal z any) then begin

    (* 2a. The lookahead assumption [z] is a real terminal symbol. We check
       whether [current] is willing to reduce some production [prod] on [z],
       and whether the sub-trie [position] accepts [prod], which means
       that this reduction takes us back to the root of the trie. If so, we
       have discovered a new edge. *)

    match has_reduction current z with
    | Some prod when Trie.accepts prod position ->
        new_edge source (Production.nt prod) word z
    | _ ->
        ()

  end
  else begin

    (* 2b. The lookahead assumption is [any]. We must consider every pair
       [prod, z] such that the [current] state can reduce [prod] on [z]
       and [position] accepts [prod]. *)

    match has_default_reduction current with
    | Some (prod, _) ->
        if Trie.accepts prod position then
          (* [new_edge] does not accept [any] as its 4th parameter, so we
             must iterate over all terminal symbols. *)
          foreach_terminal (fun z ->
            new_edge source (Production.nt prod) word z
          )
    | None ->
       TerminalMap.iter (fun z prods ->
         if Terminal.non_error z then
           let prod = Misc.single prods in
           if Trie.accepts prod position then
             new_edge source (Production.nt prod) word z
       ) (Lr1.reductions current)

  end

(* ------------------------------------------------------------------------ *)

(* The main loop of the algorithm. *)

(* [level] is the length of [word fact] for the facts that we are examining
   at the moment. [extracted] counts how many facts we have extracted out of
   the priority queue. [considered] counts how many of these were found to
   be new, and subsequently passed to [new_fact]. *)

let level, extracted, considered =
  ref 0, ref 0, ref 0

let done_with_level () =
  Printf.eprintf "Done with level %d.\n" !level;
  W.verbose();
  F.verbose();
  E.verbose();
  Printf.eprintf "Q stores %d facts.\n" (Q.cardinal q);
  Printf.eprintf "%d facts extracted out of Q, of which %d considered.\n%!"
    !extracted !considered

let () =
  Q.repeat q (fun fact ->
    incr extracted;
    if F.register fact then begin
      if X.verbose && W.length (word fact) > !level then begin
        done_with_level();
        level := W.length (word fact);
      end;
      incr considered;
      new_fact fact
    end
  );
  if X.verbose then
    done_with_level();
  Time.tick "Running LRijkstra"

(* ------------------------------------------------------------------------ *)

(* We are done. Expose accessor functions. *)

(* We expose [E.query], but simplify its interface by specializing it with
   [foreach_terminal]. We also restrict it to the case where [a] is real. *)

let query s nt a =
  if debug then assert (Terminal.real a);
  E.query s nt a foreach_terminal

(* Expose some numbers. *)

let facts, edge_facts =
  F.size(), E.size()

let total_trie_size =
  Trie.total_size()

end
