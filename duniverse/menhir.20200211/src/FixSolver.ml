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

module Make
(M : Fix.IMPERATIVE_MAPS)
(P : sig
  include Fix.PROPERTY
  val union: property -> property -> property
end)
= struct

  type variable =
    M.key

  type property =
    P.property

  (* A constraint is represented as a mapping of each variable to an
     expression, which represents its lower bound. We could represent
     an expression as a list of constants and variables; we can also
     represent it as a binary tree, as follows. *)

  type expression =
    | EBottom
    | ECon of property
    | EVar of variable
    | EJoin of expression * expression

  type constraint_ =
    expression M.t

  (* Looking up a variable's lower bound. *)

  let consult (m : constraint_) (x : variable) : expression =
    try
      M.find x m
    with Not_found ->
      EBottom

  (* Evaluation of an expression in an environment. *)

  let rec evaluate get e =
    match e with
    | EBottom ->
        P.bottom
    | ECon p ->
        p
    | EVar x ->
        get x
    | EJoin (e1, e2) ->
        P.union (evaluate get e1) (evaluate get e2)

  (* Solving a constraint. *)

  let solve (m : constraint_) : variable -> property =
    let module F = Fix.Make(M)(P) in
    F.lfp (fun x get ->
      evaluate get (consult m x)
    )

  (* The imperative interface. *)

  let create () =
    let m = M.create() in
    let record_ConVar p y =
      M.add y (EJoin (ECon p, consult m y)) m
    and record_VarVar x y =
      M.add y (EJoin (EVar x, consult m y)) m
    in
    record_ConVar,
    record_VarVar,
    fun () -> solve m

end

