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
   H(..., X, ...). *)

type edge =
  | Safe
  | Dangerous

let successors_parameter (f : edge -> formal -> unit) x (param : parameter) =
  match param with
  | ParameterVar _ ->
      (* This is not an application. No successors. *)
      ()
  | ParameterApp (sym, params) ->
      let nt = value sym in
      (* If [x] occurs in the [i]-th actual parameter of this application,
         then there is an edge to the formal [nt, i]. Whether it is a safe
         or dangerous edge depends on whether [x] occurs shallow or deep. *)
      List.iteri (fun i param ->
        if Parameters.occurs_shallow x param then
          f Safe (nt, i)
        else if Parameters.occurs_deep x param then
          f Dangerous (nt, i)
      ) params
  | ParameterAnonymous _ ->
      assert false

let successors_producer f x ((_, param, _) : producer) =
  successors_parameter f x param

let successors_branch f x (branch : parameterized_branch) =
  List.iter (successors_producer f x) branch.pr_producers

let successors f ((nt, i) : formal) =
  let rule = try StringMap.find nt g.p_rules with Not_found -> assert false in
  let x  = try List.nth rule.pr_parameters i with Failure _ -> assert false in
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
