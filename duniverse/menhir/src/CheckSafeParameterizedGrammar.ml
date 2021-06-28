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

let value = Positions.value
open Syntax

(* This test accepts a parameterized grammar, with the restriction that all
   parameters must have sort [*]. This implies that the head of every
   application must be a toplevel nonterminal symbol: it cannot be a formal
   parameter of the current rule. *)

(* -------------------------------------------------------------------------- *)

(* This flag causes graph edges to be logged on the standard error channel. *)

let debug = false

(* -------------------------------------------------------------------------- *)

(* For syntactic convenience, the code is wrapped in a functor. *)

module Run (G : sig val g : grammar end) = struct
open G

(* -------------------------------------------------------------------------- *)

(* We build a graph whose vertices are all formal parameters of all rules. A
   formal parameter is represented as a pair of a nonterminal symbol and a
   0-based integer index (the number of this parameter within this rule).
   We use OCaml's generic equality and hash functions at this type. *)

type formal =
  symbol * int

let formals (nt, rule) : formal list =
  let arity = List.length rule.pr_parameters in
  Misc.mapi arity (fun i -> nt, i)

let formals : formal array =
  StringMap.bindings g.p_rules
  |> List.map formals
  |> List.concat
  |> Array.of_list

(* If [nt/i] is a formal parameter, we may need to find the rule that defines
   the symbol [nt] as well as the name of the [i]-th formal parameter in this
   rule. *)

let info ((nt, i) : formal) : parameterized_rule * symbol =
  let rule = try StringMap.find nt g.p_rules with Not_found -> assert false in
  let x  = try List.nth rule.pr_parameters i with Failure _ -> assert false in
  rule, x

(* -------------------------------------------------------------------------- *)

(* For each formal parameter [nt/i], we want to know whether this parameter is
   actually used, that is, whether it occurs in the right-hand side. *)

(* Note that we look for syntactic occurrences *anywhere* in the right-hand
   side. We do *not* ignore occurrences that appear as the actual argument
   of a parameterized symbol that happens to ignore its argument... That
   would probably require a fixed point computation, and might be unsound:
   expansion might diverge as soon as there is a syntactic occurrence
   in the right-hand side. *)

let used_in_producer x ((_, param, _) : producer) =
  Parameters.occurs x param

let used_in_branch x (branch : parameterized_branch) =
  List.exists (used_in_producer x) branch.pr_producers

let used (formal : formal) : bool =
  let rule, x = info formal in
  List.exists (used_in_branch x) rule.pr_branches

(* Memoize this function. *)

let used : formal -> bool =
  let module M = Fix.Memoize.ForType(struct type t = formal end) in
  M.memoize used

(* -------------------------------------------------------------------------- *)

(* The graph edges are as follows. First, for every rule of the following form:

     F(..., X, ...):           # where X is the i-th formal parameter of F
       ... G(..., X, ...) ...  # where X is the j-th actual parameter of G

   there is a "safe" edge from the formal parameter F/i to the formal G/j. This
   reflects the fact that there is a flow from F/i to G/j. It is "safe" in the
   sense that it is not size-increasing: the same parameter X is passed from F
   to G.

   Second, for every rule of the following form:

     F(..., X, ...):           # where X is the i-th formal parameter of F
       ... G(..., H(..., X, ...) , ...) ...
                               # where H(...) is the j-th actual parameter of G

   there is a "dangerous" edge from the formal parameter F/i to the formal G/j.
   This reflects the fact that there is a flow from F/i to G/j. This flow is
   "dangerous" in the sense that it is size-increasing: X is transformed to
   H(..., X, ...). In this example, there should also be a safe edge from the
   formal F/i to the formal H/k.

   More generally, an occurrence of X in the right-hand side can occur deeply
   nested in a context K:

     F(..., X, ...):
        ... K[X] ...

   In that case, we must create as many edges as the context K is deep, and
   all of these edges should be dangerous, except the one that corresponds
   to the innermost layer of K, which is safe.

   Another way of putting this is, when the left-hand side is:

     F(..., X, ...):           # where X is the i-th formal parameter of F

   and the right-hand side contains a possibly-nested occurrence of:

       G(..., K[X], ...)       # where K[X] is the j-th actual parameter of G

   then we must create an edge of F/i to G/j, and this edge is safe if and
   only if the context K is empty, i.e., X occurs at depth 0 in K[x]. *)

(* As an exception to the previous rule, if it is known that the parameterized
   symbol G does not use its [j]-th parameter, then the edge of F/i to G/j
   should not be created, nor should the applications inside K be inspected. *)

(* The code below has quadratic complexity because [Parameters.occurs_deep]
   is expensive. In principle, we could achieve linear complexity by first
   annotating subterm (bottom-up) with a Boolean flag that indicates whether
   [x] occurs (shallowly/deeply) in it; that would allow us to implement
   [occurs_deep] in constant time. However, in practice, quadratic complexity
   is probably good enough. *)

type edge =
  | Safe
  | Dangerous

let rec successors_parameter (f : edge -> formal -> unit) x (param : parameter) =
  match param with
  | ParameterVar _ ->
      (* This is not an application. No successors. *)
      ()
  | ParameterApp (sym, params) ->
      let nt = value sym in
      List.iteri (fun i param ->
        (* If it is known that [nt] does not use its [i]-th parameter, then
           there is nothing to do here. *)
        if used (nt, i) then begin
          (* Check, recursively, the applications that appear inside [param]. *)
          successors_parameter f x param;
          (* If [x] occurs in the [i]-th actual parameter of this application,
             then there is an edge to the formal [nt, i]. Whether it is a safe
             or dangerous edge depends on whether [x] occurs shallow or deep. *)
          if Parameters.occurs_shallow x param then
            f Safe (nt, i)
          else if Parameters.occurs_deep x param then
            f Dangerous (nt, i)
        end
      ) params
  | ParameterAnonymous _ ->
      assert false

let successors_producer f x ((_, param, _) : producer) =
  successors_parameter f x param

let successors_branch f x (branch : parameterized_branch) =
  List.iter (successors_producer f x) branch.pr_producers

let successors f (formal : formal) =
  let rule, x = info formal in
  List.iter (successors_branch f x) rule.pr_branches

(* -------------------------------------------------------------------------- *)

(* We now have a full description of the graph. *)

module G = struct
  type node = formal
  let n = Array.length formals
  let index = Misc.inverse formals
  let successors f = successors (fun _ target -> f target)
  let iter f = Array.iter f formals
end

(* -------------------------------------------------------------------------- *)

(* Display the graph. *)

let () =
  if debug then
    G.iter (fun (x, i) ->
      successors (fun edge (y, j) ->
        let kind = match edge with Safe -> "safe" | Dangerous -> "dangerous" in
        Printf.eprintf "%s/%d ->(%s) %s/%d\n" x i kind y j
      ) (x, i)
    )

(* -------------------------------------------------------------------------- *)

(* Compute its strongly connected components, ignoring the distinction between
   safe and dangerous edges. *)

module T = Tarjan.Run(G)

(* -------------------------------------------------------------------------- *)

(* The safety criterion is: no dangerous edge is part of a cycle. Indeed, if
   this criterion is satisfied, then expansion must terminate: only a finite
   number of well-sorted terms (involving toplevel symbols and applications)
   can arise. (This sentence is not a proof!) Conversely, if a dangerous edge
   appears in a cycle, then expansion will not terminate. (That is, unless the
   dangerous cycle is unreachable. We choose to reject it anyway in that case.)
   In other words, this criterion is sound and complete. *)

(* Checking that no dangerous edge is part of a cycle is done by examining the
   source and destination of every dangerous edge and ensuring that they lie
   in distinct components. *)

let () =
  G.iter (fun source ->
    successors (fun edge target ->
      match edge with
      | Safe ->
          ()
      | Dangerous ->
          if T.representative source = T.representative target then
            let (nt, i) = source in
            Error.error []
              "the parameterized nonterminal symbols in this grammar\n\
               cannot be expanded away: expansion would not terminate.\n\
               The %s formal parameter of \"%s\" grows without bound."
              (Misc.nth (i + 1)) nt

    ) source
  )

end (* of the functor *)

(* -------------------------------------------------------------------------- *)

(* Re-package the above functor as a function. *)

let check g =
  let module T = Run(struct let g = g end) in
  ()
