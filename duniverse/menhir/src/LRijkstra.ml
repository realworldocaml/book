(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module implements [--list-errors]. Its purpose is to find, for each
   pair of a state [s] and a terminal symbol [z] such that looking at [z] in
   state [s] causes an error, a minimal path (starting in some initial state)
   that actually triggers this error. *)

(* This is potentially useful for grammar designers who wish to better
   understand the properties of their grammar, or who wish to produce a
   list of all possible syntax errors (or, at least, one syntax error in
   each automaton state where an error may occur). *)

(* The problem seems rather tricky. One might think that it suffices to
   compute shortest paths in the automaton, and to use [Analysis.minimal] to
   replace each non-terminal symbol in a path with a minimal word that this
   symbol generates. One can indeed do so, but this yields only a lower bound
   on the actual shortest path to the error at [s, z]. Indeed, several
   difficulties arise, including the fact that reductions are subject to a
   lookahead hypothesis; the fact that some states have a default reduction,
   hence will never trigger an error; the fact that conflict resolution
   removes some (shift or reduce) actions, hence may suppress the shortest
   path. *)

open Grammar

module type REACHABILITY_RESULT = sig
  module Word : sig
    type t
    val singleton : Terminal.t -> t
    val elements : t -> Terminal.t list
    val compare : t -> t -> int
    val length : t -> int
  end

  module Graph : sig
    (* Graph nodes. *)
    type node
    include Hashtbl.HashedType with type t := node

    val state : node -> Lr1.node
    val lookaheads : node -> TerminalSet.t

    (* Edge labels. *)
    type label
    val append_word : label -> Word.t -> Word.t

    (* The source node(s). *)

    val sources: (node -> unit) -> unit

    (* [successors n f] presents each of [n]'s successors, in
       an arbitrary order, to [f], together with the cost of
       the edge that was followed. *)
    val successors: node -> (label -> int -> node -> unit) -> unit
  end

  module Statistics : sig
    val header : string
    val print : out_channel -> time:float -> heap:int -> unit
  end
end

module type REACHABILITY_ALGORITHM = functor () -> REACHABILITY_RESULT

(* ------------------------------------------------------------------------ *)

(* To delay the side effects performed by this module, we wrap everything in
   in a big functor. The functor also serves to pass verbosity parameters. *)

module Run
    (X : sig
       (* If [verbose] is set, produce various messages on [stderr]. *)
       val verbose: bool

       (* If [statistics] is defined, it is interpreted as the name of
          a file to which one line of statistics is appended. *)
       val statistics: string option
     end)
    (Alg : REACHABILITY_ALGORITHM)
    () =
struct

open Default

(* ------------------------------------------------------------------------ *)

(* Record our start time. *)

let now () =
  match X.statistics with
  | Some _ ->
      Unix.((times()).tms_utime)
  | None ->
      0.0

let start =
  now()

(* ------------------------------------------------------------------------ *)

(* Run the core reachability analysis, which finds out exactly under
   what conditions each nonterminal transition in the automaton can be
   taken. *)

module Core = Alg()
module Word = Core.Word

(* ------------------------------------------------------------------------ *)

(* The following code validates the fact that an error can be triggered in
   state [s'] by beginning at the start symbol [nt] and reading the sequence
   of terminal symbols [elements]. We use this for debugging purposes.
   Furthermore, this gives us a list of spurious reductions, which we use to
   produce a comment. *)

let fail msg =
  Printf.eprintf "LRijkstra: internal error: %s.\n%!" msg;
  exit 1

let fail format =
  Printf.ksprintf fail format

let validate nt s' elements : ReferenceInterpreter.target =
  let open ReferenceInterpreter in
  match
    check_error_path Logging.never nt elements
  with
  | OInputReadPastEnd ->
      fail "input was read past its end"
  | OInputNotFullyConsumed ->
      fail "input was not fully consumed"
  | OUnexpectedAccept ->
      fail "input was unexpectedly accepted"
  | OK ((state, _) as target) ->
      if Lr1.Node.compare state s' <> 0 then
        fail "error occurred in state %d instead of %d"
          (Lr1.number state)
          (Lr1.number s')
      else
        target


(* ------------------------------------------------------------------------ *)

(* We now wish to determine, given a state [s'] and a terminal symbol [z], a
   minimal path that takes us from some entry state to state [s'] with [z] as
   the next (unconsumed) symbol. *)

(* This can be formulated as a search for a shortest path in a graph. The
   graph is not just the automaton, though. It is a (much) larger graph whose
   vertices are pairs [s, z] and whose edges are obtained by querying the
   module [E] above. For this purpose, we use Dijkstra's algorithm,
   unmodified. Experiments show that the running time of this phase is
   typically 10x shorter than the running time of the main loop above. *)

module A = Astar.Make(struct
    include Core.Graph

    (* Algorithm A*, used with a zero estimate, is Dijkstra's algorithm.
       We have experimented with a non-zero estimate, but the performance
       increase was minimal. *)
    let estimate _ =
      0
  end)

(* ------------------------------------------------------------------------ *)

(* [explored] counts how many graph nodes we have discovered during the
   search. *)

let explored =
  ref 0

(* We wish to store a set of triples [nt, w, (s', spurious)], meaning that an
   error can be triggered in state [s'] by beginning in the initial state that
   corresponds to [nt] and by reading the sequence of terminal symbols [w]. We
   wish to store at most one such triple for every state [s'], so we organize
   the data as a set [domain] of states [s'] and a list [data] of triples [nt,
   w, (s', spurious)]. The list [spurious] documents the spurious reductions
   that are performed by the parser at the end. *)

(* We could print this data as we go, which would naturally result in sorting
   the output by increasing word sizes. However, it seems preferable to sort
   the sentences lexicographically, so that similar sentences end up close to
   one another. (We could also sort them by state number. The result would be
   roughly similar.) This is why we store a list of triples and sort it before
   printing it out. *)

let domain =
  ref Lr1.NodeSet.empty

let data : (Nonterminal.t * Word.t * ReferenceInterpreter.target) list ref =
  ref []

(* The set [reachable] stores every reachable state (regardless of whether an
   error can be triggered in that state). *)

let reachable =
  ref Lr1.NodeSet.empty

(* Perform the forward search. *)

let _, _ =
  A.search (fun (node', path) ->
      incr explored;
      let s' = Core.Graph.state node' in
      let zs = Core.Graph.lookaheads node' in
      reachable := Lr1.NodeSet.add s' !reachable;
      (* If [z] causes an error in state [s'] and this is the first time
         we are able to trigger an error in this state, ... *)
      if not (Lr1.NodeSet.mem s' !domain) then
        begin match find_erroneous s' zs with
          | None -> ()
          | Some z ->
            (* Reconstruct the initial state [s] and the word [w] that lead
               to this error. *)
            let node, ws = A.reverse path in
            let w = List.fold_right Core.Graph.append_word ws (Word.singleton z) in
            (* Check that the reference interpreter confirms our finding.
               At the same time, compute a list of spurious reductions. *)
            let nt = Lr1.nt_of_entry (Core.Graph.state node) in
            let target = validate nt s' (Word.elements w) in
            (* Store this new data. *)
            domain := Lr1.NodeSet.add s' !domain;
            data := (nt, w, target) :: !data
        end
    )

(* Sort and output the data. *)

let () =
  !data
  |> List.fast_sort (fun (nt1, w1, (s1, _)) (nt2, w2, (s2, _)) ->
      let c = Int.compare (Lr1.number s1) (Lr1.number s2) in
      if c <> 0 then c else
        let c = Nonterminal.compare nt1 nt2 in
        if c <> 0 then c else Word.compare w2 w1
    )
  |> List.map (fun (nt, w, target) -> (nt, Word.elements w, target))
  |> List.iter Interpret.print_messages_item

(* ------------------------------------------------------------------------ *)

(* Verbosity. *)

let max_heap_size =
  if X.verbose || X.statistics <> None then
    let stat = Gc.quick_stat() in
    (stat.Gc.top_heap_words * (Sys.word_size / 8) / 1024 / 1024)
  else
    0 (* dummy *)

let () =
  Time.tick "Forward search";
  if X.verbose then begin
    Printf.eprintf
      "%d graph nodes explored by forward search.\n\
       %d out of %d states are reachable.\n\
       Found %d states where an error can occur.\n%!"
      !explored
      (Lr1.NodeSet.cardinal !reachable) Lr1.n
      (Lr1.NodeSet.cardinal !domain)
  end

(* ------------------------------------------------------------------------ *)

(* If requested by the client, write one line of statistics to a .csv file. *)

let stop =
  now()

let () =
  X.statistics |> Option.iter (fun filename ->
      let need_header = not (Sys.file_exists filename) in
      let c = open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 filename in
      if need_header then (
        output_string c Core.Statistics.header;
        output_char c '\n';
      );
      Core.Statistics.print c
        (* Elapsed user time, in seconds. *)
        ~time:(stop -. start)
        (* Max heap size, in megabytes. *)
        ~heap:max_heap_size;
      close_out c
    )

(* ------------------------------------------------------------------------ *)
end
