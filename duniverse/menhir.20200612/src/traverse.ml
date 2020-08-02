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

(* Code for traversing or transforming [IL] terms. *)

open IL
open CodeBits

(* This turns a list of value definitions into a hash table. It also
counts and numbers the definitions. We assume that the left-hand
side of every definition is a variable. *)

let tabulate_defs (defs : valdef list) : int * (string, int * valdef) Hashtbl.t =
  let count = ref 0 in
  let table = Hashtbl.create 1023 in
  List.iter (fun def ->
          let k = !count in
          count := k + 1;
          Hashtbl.add table (pat2var def.valpat) (k, def)
    ) defs;
  !count, table

(* This mixin class, used by [map] and [fold] below, helps maintain
   environments, which can be used to keep track of local variable
   bindings. *)

class virtual ['env] env = object(self)

  (* The virtual method [pvar] records a local variable binding in
     the environment. *)

  method virtual pvar: 'env -> string -> 'env

  method pat env = function
    | PWildcard
    | PUnit ->
        env
    | PVar id ->
        self#pvar env id
    | PVarLocated id ->
        let id = Positions.value id in
        self#pvar env id
    | PTuple ps
    | POr ps
    | PData (_, ps) ->
        self#pats env ps
    | PAnnot (p, _) ->
        self#pat env p
    | PRecord fps ->
        self#fpats env fps

  method pats env ps =
    List.fold_left self#pat env ps

  method fpats env fps =
    List.fold_left self#fpat env fps

  method fpat env (_, p) =
    self#pat env p

end

(* A class that helps transform expressions. The environment [env] can be
   used to keep track of local variable bindings. *)

exception NoChange

class virtual ['env] map = object (self)

  inherit ['env] env

  method expr (env : 'env) e =
    try
      match e with
      | EVar x ->
          self#evar env x
      | EFun (ps, e) ->
          self#efun env ps e
      | EApp (e, es) ->
          self#eapp env e es
      | ELet (bs, e) ->
          self#elet env bs e
      | EMatch (e, bs) ->
          self#ematch env e bs
      | EIfThen (e, e1) ->
          self#eifthen env e e1
      | EIfThenElse (e, e1, e2) ->
          self#eifthenelse env e e1 e2
      | ERaise e ->
          self#eraise env e
      | ETry (e, bs) ->
          self#etry env e bs
      | EUnit ->
          self#eunit env
      | EIntConst k ->
          self#eintconst env k
      | EStringConst s ->
          self#estringconst env s
      | EData (d, es) ->
          self#edata env d es
      | ETuple es ->
          self#etuple env es
      | EAnnot (e, t) ->
          self#eannot env e t
      | EMagic e ->
          self#emagic env e
      | ERepr _ ->
          self#erepr env e
      | ERecord fs ->
          self#erecord env fs
      | ERecordAccess (e, f) ->
          self#erecordaccess env e f
      | ERecordWrite (e, f, e1) ->
          self#erecordwrite env e f e1
      | ETextual action ->
          self#etextual env action
      | EComment (s, e) ->
          self#ecomment env s e
      | EPatComment (s, p, e) ->
          self#epatcomment env s p e
      | EArray es ->
          self#earray env es
      | EArrayAccess (e, i) ->
          self#earrayaccess env e i
    with NoChange ->
      e

  method evar _env _x =
    raise NoChange

  method efun env ps e =
    let e' = self#expr (self#pats env ps) e in
    if e == e' then
      raise NoChange
    else
      EFun (ps, e')

  method eapp env e es =
    let e' = self#expr env e
    and es' = self#exprs env es in
    if e == e' && es == es' then
      raise NoChange
    else
      EApp (e', es')

  method elet env bs e =
    let env, bs' = self#bindings env bs in
    let e' = self#expr env e in
    if bs == bs' && e == e' then
      raise NoChange
    else
      ELet (bs', e')

  method ematch env e bs =
    let e' = self#expr env e
    and bs' = self#branches env bs in
    if e == e' && bs == bs' then
      raise NoChange
    else
      EMatch (e', bs')

  method eifthen env e e1 =
    let e' = self#expr env e
    and e1' = self#expr env e1 in
    if e == e' && e1 == e1' then
      raise NoChange
    else
      EIfThen (e', e1')

  method eifthenelse env e e1 e2 =
    let e' = self#expr env e
    and e1' = self#expr env e1
    and e2' = self#expr env e2 in
    if e == e' && e1 == e1' && e2 == e2' then
      raise NoChange
    else
      EIfThenElse (e', e1', e2')

  method eraise env e =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      ERaise e'

  method etry env e bs =
    let e' = self#expr env e
    and bs' = self#branches env bs in
    if e == e' && bs == bs' then
      raise NoChange
    else
      ETry (e', bs')

  method eunit _env =
    raise NoChange

  method eintconst _env _k =
    raise NoChange

  method estringconst _env _s =
    raise NoChange

  method edata env d es =
    let es' = self#exprs env es in
    if es == es' then
      raise NoChange
    else
      EData (d, es')

  method etuple env es =
    let es' = self#exprs env es in
    if es == es' then
      raise NoChange
    else
      ETuple es'

  method eannot env e t =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      EAnnot (e', t)

  method emagic env e =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      EMagic e'

  method erepr env e =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      ERepr e'

  method erecord env fs =
    let fs' = self#fields env fs in
    if fs == fs' then
      raise NoChange
    else
      ERecord fs'

  method erecordaccess env e f =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      ERecordAccess (e', f)

  method erecordwrite env e f e1 =
    let e' = self#expr env e
    and e1' = self#expr env e1 in
    if e == e' && e1 == e1' then
      raise NoChange
    else
      ERecordWrite (e', f, e1')

  method earray env es =
    let es' = self#exprs env es in
    if es == es' then
      raise NoChange
    else
      EArray es'

  method earrayaccess env e i =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      EArrayAccess (e', i)

  method etextual _env _action =
    raise NoChange

  method ecomment env s e =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      EComment (s, e')

  method epatcomment env s p e =
    let e' = self#expr env e in
    if e == e' then
      raise NoChange
    else
      EPatComment (s, p, e')

  method exprs env es =
    Misc.smap (self#expr env) es

  method fields env fs =
    Misc.smap (self#field env) fs

  method field env ((f, e) as field) =
    let e' = self#expr env e in
    if e == e' then
      field
    else
      (f, e')

  method branches env bs =
    Misc.smap (self#branch env) bs

  method branch env b =
    let e = b.branchbody in
    let e' = self#expr (self#pat env b.branchpat) e in
    if e == e' then
      b
    else
      { b with branchbody = e' }

  (* The method [binding] produces a pair of an updated environment
     and a transformed binding. *)

  method binding env ((p, e) as b) =
    let e' = self#expr env e in
    self#pat env p,
    if e == e' then
      b
    else
      (p, e')

  (* For nested non-recursive bindings, the environment produced by
     each binding is used to traverse the following bindings. The
     method [binding] produces a pair of an updated environment
     and a transformed list of bindings. *)

  method bindings env bs =
    Misc.smapa self#binding env bs

  method valdef env def =
    let e = def.valval in
    let e' = self#expr env e in
    if e == e' then
      def
    else
      { def with valval = e' }

  method valdefs env defs =
    Misc.smap (self#valdef env) defs

end

(* A class that helps iterate, or fold, over expressions. *)

class virtual ['env, 'a] fold = object (self)

  inherit ['env] env

  method expr (env : 'env) (accu : 'a) e =
    match e with
    | EVar x ->
        self#evar env accu x
    | EFun (ps, e) ->
        self#efun env accu ps e
    | EApp (e, es) ->
        self#eapp env accu e es
    | ELet (bs, e) ->
        self#elet env accu bs e
    | EMatch (e, bs) ->
        self#ematch env accu e bs
    | EIfThen (e, e1) ->
        self#eifthen env accu e e1
    | EIfThenElse (e, e1, e2) ->
        self#eifthenelse env accu e e1 e2
    | ERaise e ->
        self#eraise env accu e
    | ETry (e, bs) ->
        self#etry env accu e bs
    | EUnit ->
        self#eunit env accu
    | EIntConst k ->
        self#eintconst env accu k
    | EStringConst s ->
        self#estringconst env accu s
    | EData (d, es) ->
        self#edata env accu d es
    | ETuple es ->
        self#etuple env accu es
    | EAnnot (e, t) ->
        self#eannot env accu e t
    | EMagic e ->
        self#emagic env accu e
    | ERepr _ ->
        self#erepr env accu e
    | ERecord fs ->
        self#erecord env accu fs
    | ERecordAccess (e, f) ->
        self#erecordaccess env accu e f
    | ERecordWrite (e, f, e1) ->
        self#erecordwrite env accu e f e1
    | ETextual action ->
        self#etextual env accu action
    | EComment (s, e) ->
        self#ecomment env accu s e
    | EPatComment (s, p, e) ->
        self#epatcomment env accu s p e
    | EArray es ->
        self#earray env accu es
    | EArrayAccess (e, i) ->
        self#earrayaccess env accu e i

  method evar (_env : 'env) (accu : 'a) _x =
    accu

  method efun (env : 'env) (accu : 'a) ps e =
    let accu = self#expr (self#pats env ps) accu e in
    accu

  method eapp (env : 'env) (accu : 'a) e es =
    let accu = self#expr env accu e in
    let accu = self#exprs env accu es in
    accu

  method elet (env : 'env) (accu : 'a) bs e =
    let env, accu = self#bindings env accu bs in
    let accu = self#expr env accu e in
    accu

  method ematch (env : 'env) (accu : 'a) e bs =
    let accu = self#expr env accu e in
    let accu = self#branches env accu bs in
    accu

  method eifthen (env : 'env) (accu : 'a) e e1 =
    let accu = self#expr env accu e in
    let accu = self#expr env accu e1 in
    accu

  method eifthenelse (env : 'env) (accu : 'a) e e1 e2 =
    let accu = self#expr env accu e in
    let accu = self#expr env accu e1 in
    let accu = self#expr env accu e2 in
    accu

  method eraise (env : 'env) (accu : 'a) e =
    let accu = self#expr env accu e in
    accu

  method etry (env : 'env) (accu : 'a) e bs =
    let accu = self#expr env accu e in
    let accu = self#branches env accu bs in
    accu

  method eunit (_env : 'env) (accu : 'a) =
    accu

  method eintconst (_env : 'env) (accu : 'a) _k =
    accu

  method estringconst (_env : 'env) (accu : 'a) _s =
    accu

  method edata (env : 'env) (accu : 'a) _d es =
    let accu = self#exprs env accu es in
    accu

  method etuple (env : 'env) (accu : 'a) es =
    let accu = self#exprs env accu es in
    accu

  method eannot (env : 'env) (accu : 'a) e _t =
    let accu = self#expr env accu e in
    accu

  method emagic (env : 'env) (accu : 'a) e =
    let accu = self#expr env accu e in
    accu

  method erepr (env : 'env) (accu : 'a) e =
    let accu = self#expr env accu e in
    accu

  method erecord (env : 'env) (accu : 'a) fs =
    let accu = self#fields env accu fs in
    accu

  method erecordaccess (env : 'env) (accu : 'a) e _f =
    let accu = self#expr env accu e in
    accu

  method erecordwrite (env : 'env) (accu : 'a) e _f e1 =
    let accu = self#expr env accu e in
    let accu = self#expr env accu e1 in
    accu

  method earray (env : 'env) (accu : 'a) es =
    let accu = self#exprs env accu es in
    accu

  method earrayaccess (env : 'env) (accu : 'a) e _i =
    let accu = self#expr env accu e in
    accu

  method etextual (_env : 'env) (accu : 'a) _action =
    accu

  method ecomment (env : 'env) (accu : 'a) _s e =
    let accu = self#expr env accu e in
    accu

  method epatcomment (env : 'env) (accu : 'a) _s _p e =
    let accu = self#expr env accu e in
    accu

  method exprs (env : 'env) (accu : 'a) es =
    List.fold_left (self#expr env) accu es

  method fields (env : 'env) (accu : 'a) fs =
    List.fold_left (self#field env) accu fs

  method field (env : 'env) (accu : 'a) (_f, e) =
    let accu = self#expr env accu e in
    accu

  method branches (env : 'env) (accu : 'a) bs =
    List.fold_left (self#branch env) accu bs

  method branch (env : 'env) (accu : 'a) b =
    let accu = self#expr (self#pat env b.branchpat) accu b.branchbody in
    accu

  method binding ((env, accu) : 'env * 'a) (p, e) =
    let accu = self#expr env accu e in
    self#pat env p,
    accu

  method bindings (env : 'env) (accu : 'a) bs =
    List.fold_left self#binding (env, accu) bs

  method valdef (env : 'env) (accu : 'a) def =
    let accu = self#expr env accu def.valval in
    accu

  method valdefs (env : 'env) (accu : 'a) defs =
    List.fold_left (self#valdef env) accu defs

end
