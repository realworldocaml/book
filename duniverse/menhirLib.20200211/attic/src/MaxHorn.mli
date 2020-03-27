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

(* A heuristic MaxSAT solver, tailored to the special case where the clauses
   are in fact Horn clauses. *)

module Make () : sig

  (* A polarity is [true] or [false]. *)

  type polarity =
    bool

  (* A propositional variable [x] denotes an unknown Boolean value. *)

  type variable

  (* A literal is either a variable [x] or the negation of a variable [~x].
     It can also be read as an equation [x = true] or [x = false]. *)

  type literal =
    polarity * variable

  (* A clause is a disjunction of literals. The clauses built by the user
     must satisfy the following conditions:

     1. In a clause, two distinct literals must concern two distinct variables.
        Thus, [x \/ x] and [~x \/ ~x], which are redundant, is forbidden,
        while [x \/ ~x], which is trivially true, is forbidden as well.

     2. Every clause must have at least one negative literal.

     3. Every clause has at most one positive literal.
        In other words, every clause is a Horn clause. *)

  (* Condition 1 is basic hygiene. Condition 2 implies that the problem, a
     conjunction of clauses, is satisfiable: indeed, it can be satisfied by
     setting every variable to [false]. Condition 3 guarantees that condition
     2 remains verified as the solver makes progress towards a solution. *)

  type clause =
    literal list

  (* A solution is a mapping of variables to Boolean values. *)

  type solution =
    variable -> polarity

  (* A new variable is produced by [new_variable p], where [p] is a
     nonnegative integer priority. The priority [p] plays a role in
     the heuristics used by the solver: a variable whose priority is
     smaller runs a better chance of being assigned the value [true]. *)

  type priority =
    int

  val new_variable: priority -> variable

  (* A clause [clause] is declared to the solver by [declare clause]. *)

  val declare: clause -> unit

  (* [stats()] indicates how many variables and clauses have been declared.
     It can be called at any time. *)

  val stats: unit -> int * int

  (* After [new_variable] and [declare] have been used to create all variables
     and declare all clauses, the call [solve()] runs the solver and returns a
     solution. (Because the problem is always satisfiable by construction, the
     solver always succeeds.) The solver attempts to find a solution where as
     many variables as possible are set to [true]. The solver uses a heuristic
     algorithm and does not necessarily find an optimal solution. *)

  val solve: unit -> solution

end
