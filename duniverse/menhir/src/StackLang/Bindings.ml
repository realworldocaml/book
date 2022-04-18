(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLangBasics

(* In the following, we write [bs] for a set of bindings. *)

type t =
  value Reg.Map.t

let empty, is_empty, mem, domain, to_list, iter, fold, restrict =
  Reg.Map.(
    empty, is_empty, mem, domain, bindings, iter, fold, restrict
  )

let remove bs rs =
  Reg.Set.fold Reg.Map.remove rs bs

let apply bs v =
  match v with
  | VReg r ->
      (try Reg.Map.find r bs with Not_found -> v)
  | VTag _ | VUnit ->
      v

(* [add] is an internal function; it is used in the definition of [assign]
   and [compose]. *)

let add r v bs =
  match v with
  (* We drop identity bindings. This is not essential, but may help
     avoid a certain amount of noise. Also, the definition of
     [Sync.def] assumes that we cannot have an identity binding. *)
  | VReg r' when r = r' ->
      bs
  | _ ->
      Reg.Map.add r v bs

(* [assign] with an accumulator. *)

let assign1 bs p v =
  match (p, v) with
  | PWildcard, _ ->
      bs
  | PReg r, v ->
      assert (not (Reg.Map.mem r bs));
      add r v bs

let assign bs ps vs =
  assert (List.length ps = List.length vs);
  List.fold_left2 assign1 bs ps vs

(* [assign] without an accumulator. We assume that no register is assigned
   twice by the pattern [p]. *)

let assign ps vs =
  assign empty ps vs

(* To compute the sequential composition of [bs1] and [bs2], we must apply
   [bs1] to each of the values in the codomain of [bs2]. We can then add
   each binding in [bs2] to [bs1], possibly overriding previous bindings. *)

let seq bs1 bs2 =
  (* We fold on [bs2] with an accumulator whose initial value is [bs1]. *)
  fold (fun r v bs -> add r (apply bs1 v) bs) bs2 bs1

let par bs1 bs2 =
  assert (Reg.Set.(is_empty (inter (domain bs1) (domain bs2))));
  (* A union of disjoint maps. *)
  fold add bs2 bs1

let codomain bs =
  fold (fun _r v accu -> Value.add_registers accu v) bs Reg.Set.empty
