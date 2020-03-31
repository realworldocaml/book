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

module InfiniteArray =
  MenhirLib.InfiniteArray

let postincrement =
  Misc.postincrement

let unSome =
  Misc.unSome

module Make () = struct

  type polarity =
    bool

  type variable =
    int

  type literal =
    polarity * variable

  type clause =
    literal list

  let size clause =
    match clause with
    | [] ->
        0
    | [ _ ] ->
        1
    | _ :: _ :: _ ->
        2 (* Two means two or more. *)

  type solution =
    variable -> polarity

  (* Clauses are numbered internally. An index is a clause number. *)

  type index =
    int

  (* The set [unresolved] contains all unresolved variables (and possibly
     some resolved variables, too, which we ignore). It is set up as a
     priority queue, so we can easily extract a variable of highest priority
     when we so wish. *)

  let unresolved : variable LowIntegerPriorityQueue.t =
    LowIntegerPriorityQueue.create (-1) (* dummy *)

  (* This extensible array of clauses is used to implement [declare]. *)

  let clauses =
    InfiniteArray.make []

  (* The counter [c] is used to allocate new clauses. *)

  let c : index ref =
    ref 0

  (* The counter [v] is used to allocate new variables. *)

  let v : variable ref =
    ref 0

  (* This Boolean flag is used to forbid uses of [new_variable] or [declare]
     after [solve] has been called. *)

  let declarations_permitted =
    ref true

  (* A new variable is allocated as follows. *)

  type priority =
    int

  let new_variable (p : priority) =
    assert !declarations_permitted;
    let x = postincrement v in
    LowIntegerPriorityQueue.add unresolved x p;
    x

  (* We assume the property ALON: every clause has at least one negative
     literal. This implies that the formulae we are looking at are always
     satisfiable: they can be satisfied by setting all variables to [false].
     Our goal is to set as many variables as possible to [true] while
     retaining satisfiability. *)

  (* In fact, we assume the property AMOP: every clause has at most one
     positive literal. This property is preserved as variables are resolved,
     so it is an invariant: AMOP always holds. Furthermore, in the absence
     of empty clauses and unit clauses, AMOP implies ALON. (Indeed, if a
     clause has at least two literals and at most one positive literal, then
     it has at least one negative literal.) Thus, if unit propagation
     succeeds and leaves us in a state where no unit clauses exist, then we
     have ALON, hence the current formula can be satisfied by setting all
     variables to [false]. *)

  (* The property AMOP can also be stated as follows: every clause must be a
     Horn clause. *)

  (* A new clause is declared as follows. *)

  let declare clause =
    assert !declarations_permitted;
    (* Allocate a new clause index. *)
    let i = postincrement c in
    (* Record this clause. *)
    InfiniteArray.set clauses i clause

  (* Information about the variables and clauses declared so far. *)

  let stats () =
    !v, !c

  (* Here begins the solver. *)

  module Solve () = struct

    (* Declarations are no longer permitted. *)

    let () =
      declarations_permitted := false

    (* The number of variables is now fixed. *)

    let v =
      !v

    (* The array [resolved] maps every variable [x] to a Boolean flag that tells
       whether this variable has been resolved. *)

    let resolved : bool array =
      Array.make v false

    (* The array [value] maps every resolved variable [x] to its Boolean value. *)

    let value : bool array =
      Array.make v false (* dummy *)

    (* The array [positive] maps an variable to the indices of the clauses
       where this variable occurs positively. We do not update this array as
       we make progress towards a solution, so: 1- it should be looked up at
       unresolved variables only; 2- the list [positive.(x)] can contain
       clauses that have become true, so [x] no longer appears in them. *)

    let positive : index list array =
      Array.make v []

    (* The array [negative] maps a variable to the indices of the clauses where
       this variable occurs negatively. *)

    let negative : index list array =
      Array.make v []

    (* The expression [(occurrences polarity).(x)] denotes the indices of
       the clauses where the literal [(polarity, x)] occurs. It is also a
       left-value, i.e., it can be used on the left-hand side of an assignment. *)

    let occurrences polarity =
      if polarity then positive else negative

    (* The array [clauses] maps a clause index to the current form of this
       clause. A clause becomes simpler and simpler over time, as the
       variables that appear in it are resolved. A clause can also disappear
       entirely; this means that it has become satisfied. *)

    (* Thus, an empty clause [Some []] means [false], whereas a missing
       clause [None] means [true]. *)

    (* We maintain the invariant that all of the variables that appear in a
       a clause are unresolved. *)

    let clauses : clause option array =
      Array.map (fun clause -> Some clause) (InfiniteArray.domain clauses)

    (* The bag [unit] holds the indices of the unit clauses (that is, the
       clauses that have exactly one literal), in an arbitrary order.
       Because a unit clause can become satisfied while it is in this bag,
       we must also be prepared for this bag to contain indices of clauses
       that have disappeared. *)

    let unit : index Stack.t =
      Stack.create()

    (* Initialize [positive], [negative], and [unit]. *)

    let () =
      Array.iteri (fun i oclause ->
        let clause = unSome oclause in
        List.iter (fun (polarity, x) ->
          (* Record that the literal [(polarity, x)] occurs in clause [i]. *)
          let occurrences = occurrences polarity in
          occurrences.(x) <- i :: occurrences.(x)
        ) clause;
        (* If this is a unit clause, record this fact. *)
        if size clause = 1 then
          Stack.push i unit
      ) clauses

    (* Support for backtracking. *)

    (* In general, it is possible for unit propagation to discover that the
       problem is unsatisfiable. (This can happen after we speculatively
       select a variable [x] and set it to [true].) Thus, we must be prepared
       to undo the changes made during unit propagation to the global arrays
       [resolved] and [clauses]. For this purpose, we keep an undo trail,
       represented as a closure. *)

    (* Sometimes, however, we know that the problem is satisfiable and
       therefore that unit propagation cannot fail. This is the case on the
       very first run of unit propagation (where we have not made any
       arbitrary decision yet) and on every run propagation that follows
       backtracking (where we have made a wrong decision and reversed it). *)

    (* For this reason, we allow [propagate] and [resolve] to run in a safe
       mode where no undo information is recorded. *)

    let safe_mode =
      ref false

    let nothing () = ()

    let trail : (unit -> unit) ref =
      ref nothing

    let speculatively_resolve x =
      assert (not resolved.(x));
      resolved.(x) <- true;
      if not !safe_mode then begin
        let undo = !trail in
        let undo () = resolved.(x) <- false; undo() in
        trail := undo
      end

    let speculatively_update_clause i clause =
      if !safe_mode then
        clauses.(i) <- clause
      else begin
        let current = clauses.(i) in
        clauses.(i) <- clause;
        let undo = !trail in
        let undo () = clauses.(i) <- current; undo() in
        trail := undo
      end

    (* [safely f] executes [f()] in safe mode. This means that no undo
       information is recorded while [f()] is executed, and [f()] is
       expected to not raise [UNSAT]. *)

    exception UNSAT

    let safely f =
      assert (!trail == nothing);
      assert (not !safe_mode);
      safe_mode := true;
      begin try
        f()
      with UNSAT ->
        assert false (* should not happen *)
      end;
      safe_mode := false

    (* Unit propagation. *)

    let rec propagate () =
      (* Pick a unit clause [i]. *)
      if not (Stack.is_empty unit) then
        let i = Stack.pop unit in
        match clauses.(i) with
        | None ->
            (* This clause has been satisfied already. Forget about it. *)
            propagate()
        | Some [] ->
            (* This clause has been falsified already. Impossible; we would
               have aborted. *)
            assert false
        | Some (_ :: _ :: _) ->
            (* This is not a unit clause. Impossible; we would not have put
               it in the bag. *)
            assert false
        | Some [ (polarity, x) ] ->
            (* This is a unit clause. *)
            (* The variable [x] is not yet resolved. We now can and must
               resolve it: its value should be [polarity]. This allows us
               to simplify or satisfy the clauses where [x] occurs; in
               particular, this unit clause is satisfied and disappears. *)
            resolve x polarity;
            (* Continue. *)
            propagate()

    and resolve x polarity =
      speculatively_resolve x;
      (* This write to the [value] array does not need to be undone. *)
      value.(x) <- polarity;
      (* The clauses where [x] appears must now be visited. Those where
         [x] appears with polarity [polarity] become satisfied. *)
      List.iter (fun i ->
        if clauses.(i) <> None then (* optional test *)
          speculatively_update_clause i None
      ) (occurrences polarity).(x);
      (* Those where [x] occurs with opposite polarity can be simplified.
         This may cause them to become unit clauses. This may also cause
         them to become empty clauses, in which case we have detected a
         contradiction. *)
      List.iter (fun i ->
        match clauses.(i) with
        | None ->
            ()
        | Some clause ->
            (* The manner in which this is written relies on the fact that
               we cannot have both [x] and [~x] in a clause. It suffices to
               check for the equality [x = y], ignoring the polarity of [y]. *)
            let clause = List.filter (fun (_polarity, y) -> x <> y) clause in
            speculatively_update_clause i (Some clause);
            match size clause with
            | 0 ->
                (* This clause becomes empty! Fail. *)
                raise UNSAT
            | 1 ->
                (* This clause becomes a unit clause. *)
                Stack.push i unit
            | _ ->
                ()
      ) (occurrences (not polarity)).(x)

    (* [pick()] extracts a variable with minimum priority from [unresolved].
       If this variable is in fact resolved, we drop it and pick again. If it
       is unresolved, we return it. If there are no more unresolved variables,
       it returns [None]. *)

    let rec pick () =
      match LowIntegerPriorityQueue.remove unresolved with
      | None ->
          None
      | Some x ->
          if resolved.(x) then pick() else Some x

    (* The main loop of the solver. *)

    let picks, backtracks =
      ref 0, ref 0

    let rec main () =
      (* At this point, we assume that the problem is currently satisfiable by
         setting all variables to [false] and there are no unit clauses. (Unit
         propagation has just been performed.) If all variables are resolved,
         then we are done. Otherwise, we pick an unresolved variable and
         attempt to set it to [true]. *)
      assert (Stack.is_empty unit);
      match pick() with
      | None ->
          ()
      | Some x ->
          incr picks;
          resolve x true;
          (* Perform unit propagation. *)
          match propagate() with
          | () ->
              (* Unit propagation did not fail. Commit. Clear the undo trail
                 and continue. *)
              trail := nothing;
              main()
          | exception UNSAT ->
              (* Unit propagation failed. Revert to the state prior to unit
                 propagation by executing the undo trail and emptying the
                 bag of [unit] clauses. *)
              incr backtracks;
              let undo = !trail in
              undo();
              trail := nothing;
              Stack.clear unit;
              (* We now know that setting [x] to [true] was a mistake. We
                 can therefore set it to [false] and explore the consequences
                 of this discovery before continuing. *)
              safely (fun () -> resolve x false; propagate());
              main()

    (* Run the solver. *)

    let () =
      (* The very first run of unit propagation cannot fail, since the
         problem must be satisfiable. (We have not made any arbitrary
         decision.) *)
      safely (fun () -> propagate());
      (* Iterate the main loop. *)
      main()

    (* Informational output. *)

    let () =
      Error.logA 3 (fun f ->
        (* Count how many variables have been set to [true]. *)
        let c = ref 0 in
        Array.iter (fun value -> if value then incr c) value;
        (* *)
        Printf.fprintf f
          "Found a solution where %d out of %d variables are set to true.\n"
          !c v;
        Printf.fprintf f
          "%d heuristic choices of variables were made.\n"
            !picks;
        Printf.fprintf f
          "%d backtracks were necessary.\n"
            !backtracks
      )

    (* Export the solution. *)

    let solution (x : variable) : polarity =
      assert resolved.(x);
      value.(x)

  end

  (* Re-package the functor [Solve] as a function [solve]. *)

  let solve() =
    let module S = Solve() in
    S.solution

end
