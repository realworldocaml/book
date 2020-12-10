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

(* This is an implementation of Patricia trees, following Chris Okasaki's paper at the 1998 ML Workshop in Baltimore.
   Both big-endian and little-endian trees are provided. Both sets and maps are implemented on top of Patricia
   trees. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Little-endian vs big-endian trees} *)

  (* A tree is little-endian if it expects the key's least significant bits to be tested first during a search. It is
     big-endian if it expects the key's most significant bits to be tested first.

     Most of the code is independent of this design choice, so it is written as a functor, parameterized by a small
     structure which defines endianness. Here is the interface which must be adhered to by such a structure. *)

module Endianness = struct

  module type S = sig

    (* A mask is an integer with a single one bit (i.e. a power of 2). *)

    type mask = int

    (* [branching_bit] accepts two distinct integers and returns a mask which identifies the first bit where they
       differ. The meaning of ``first'' varies according to the endianness being implemented. *)

    val branching_bit: int -> int -> mask

    (* [mask i m] returns an integer [i'], where all bits which [m] says are relevant are identical to those in [i],
       and all others are set to some unspecified, but fixed value. Which bits are ``relevant'' according to a given
       mask varies according to the endianness being implemented. *)

    val mask: int -> mask -> int

    (* [shorter m1 m2] returns [true] if and only if [m1] describes a shorter prefix than [m2], i.e. if it makes fewer
       bits relevant. Which bits are ``relevant'' according to a given mask varies according to the endianness being
       implemented. *)

    val shorter: mask -> mask -> bool

  end

  (* Now, let us define [Little] and [Big], two possible [Endiannness] choices. *)

  module Little = struct

    type mask = int

    let lowest_bit x =
      x land (-x)

    (* Performing a logical ``xor'' of [i0] and [i1] yields a bit field where all differences between [i0] and [i1]
       show up as one bits. (There must be at least one, since [i0] and [i1] are distinct.) The ``first'' one is
       the lowest bit in this bit field, since we are checking least significant bits first. *)

    let branching_bit i0 i1 =
      lowest_bit (i0 lxor i1)

    (* The ``relevant'' bits in an integer [i] are those which are found (strictly) to the right of the single one bit
       in the mask [m]. We keep these bits, and set all others to 0. *)

    let mask i m =
      i land (m-1)

    (* The smaller [m] is, the fewer bits are relevant. *)

    let shorter =
      (<)

  end

  module Big = struct

    type mask = int

    let lowest_bit x =
      x land (-x)

    let rec highest_bit x =
      let m = lowest_bit x in
      if x = m then
        m
      else
        highest_bit (x - m)

    (* Performing a logical ``xor'' of [i0] and [i1] yields a bit field where all differences between [i0] and [i1]
       show up as one bits. (There must be at least one, since [i0] and [i1] are distinct.) The ``first'' one is
       the highest bit in this bit field, since we are checking most significant bits first.

       In Okasaki's paper, this loop is sped up by computing a conservative initial guess. Indeed, the bit at which
       the two prefixes disagree must be somewhere within the shorter prefix, so we can begin searching at the
       least-significant valid bit in the shorter prefix. Unfortunately, to allow computing the initial guess, the
       main code has to pass in additional parameters, e.g. a mask which describes the length of each prefix. This
       ``pollutes'' the endianness-independent code. For this reason, this optimization isn't implemented here. *)

    let branching_bit i0 i1 =
      highest_bit (i0 lxor i1)

    (* The ``relevant'' bits in an integer [i] are those which are found (strictly) to the left of the single one bit
       in the mask [m]. We keep these bits, and set all others to 0. Okasaki uses a different convention, which allows
       big-endian Patricia trees to masquerade as binary search trees. This feature does not seem to be useful here. *)

    let mask i m =
      i land (lnot (2*m-1))

    (* The smaller [m] is, the more bits are relevant. *)

    let shorter =
      (>)

  end

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Patricia-tree-based maps} *)

module Make (X : Endianness.S) = struct

  (* Patricia trees are maps whose keys are integers. *)

  type key = int

  (* A tree is either empty, or a leaf node, containing both the integer key and a piece of data, or a binary node.
     Each binary node carries two integers. The first one is the longest common prefix of all keys in this
     sub-tree. The second integer is the branching bit. It is an integer with a single one bit (i.e. a power of 2),
     which describes the bit being tested at this node. *)

  type 'a t =
    | Empty
    | Leaf of int * 'a
    | Branch of int * X.mask * 'a t * 'a t

  (* The empty map. *)

  let empty =
    Empty

  (* [choose m] returns an arbitrarily chosen binding in [m], if [m]
     is nonempty, and raises [Not_found] otherwise. *)

  let rec choose = function
    | Empty ->
        raise Not_found
    | Leaf (key, data) ->
        key, data
    | Branch (_, _, tree0, _) ->
        choose tree0

  (* [lookup k m] looks up the value associated to the key [k] in the map [m], and raises [Not_found] if no value is
     bound to [k].

     This implementation takes branches \emph{without} checking whether the key matches the prefix found at the
     current node. This means that a query for a non-existent key shall be detected only when finally reaching
     a leaf, rather than higher up in the tree. This strategy is better when (most) queries are expected to be
     successful. *)

  let rec lookup key = function
    | Empty ->
        raise Not_found
    | Leaf (key', data) ->
        if key = key' then
          data
        else
          raise Not_found
    | Branch (_, mask, tree0, tree1) ->
        lookup key (if (key land mask) = 0 then tree0 else tree1)

  let find =
    lookup

  (* [mem k m] tells whether the key [k] appears in the domain of the
     map [m]. *)

  let mem k m =
    try
      let _ = lookup k m in
      true
    with Not_found ->
      false

  (* The auxiliary function [join] merges two trees in the simple case where their prefixes disagree.

     Assume $t_0$ and $t_1$ are non-empty trees, with longest common prefixes $p_0$ and $p_1$, respectively. Further,
     suppose that $p_0$ and $p_1$ disagree, that is, neither prefix is contained in the other. Then, no matter how
     large $t_0$ and $t_1$ are, we can merge them simply by creating a new [Branch] node that has $t_0$ and $t_1$
     as children! *)

  let join p0 t0 p1 t1 =
    let m = X.branching_bit p0 p1 in
    let p = X.mask p0 (* for instance *) m in
    if (p0 land m) = 0 then
      Branch(p, m, t0, t1)
    else
      Branch(p, m, t1, t0)

  (* The auxiliary function [match_prefix] tells whether a given key has a given prefix. More specifically,
     [match_prefix k p m] returns [true] if and only if the key [k] has prefix [p] up to bit [m].

     Throughout our implementation of Patricia trees, prefixes are assumed to be in normal form, i.e. their
     irrelevant bits are set to some predictable value. Formally, we assume [X.mask p m] equals [p] whenever [p]
     is a prefix with [m] relevant bits. This allows implementing [match_prefix] using only one call to
     [X.mask]. On the other hand, this requires normalizing prefixes, as done e.g. in [join] above, where
     [X.mask p0 m] has to be used instead of [p0]. *)

  let match_prefix k p m =
    X.mask k m = p

  (* [fine_add decide k d m] returns a map whose bindings are all bindings in [m], plus a binding of the key [k] to
     the datum [d]. If a binding from [k] to [d0] already exists, then the resulting map contains a binding from
     [k] to [decide d0 d]. *)

  type 'a decision = 'a -> 'a -> 'a

  exception Unchanged

  let basic_add decide k d m =

    let rec add t =
      match t with
      | Empty ->
          Leaf (k, d)
      | Leaf (k0, d0) ->
          if k = k0 then
            let d' = decide d0 d in
            if d' == d0 then
              raise Unchanged
            else
              Leaf (k, d')
          else
            join k (Leaf (k, d)) k0 t
      | Branch (p, m, t0, t1) ->
          if match_prefix k p m then
            if (k land m) = 0 then Branch (p, m, add t0, t1)
            else Branch (p, m, t0, add t1)
          else
            join k (Leaf (k, d)) p t in

    add m

  let strict_add k d m =
    basic_add (fun _ _ -> raise Unchanged) k d m

  let fine_add decide k d m =
    try
      basic_add decide k d m
    with Unchanged ->
      m

  (* [add k d m] returns a map whose bindings are all bindings in [m], plus a binding of the key [k] to the datum
     [d]. If a binding already exists for [k], it is overridden. *)

  let add k d m =
    fine_add (fun _old_binding new_binding -> new_binding) k d m

  (* [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  let singleton k d =
    Leaf (k, d)

  (* [is_singleton m] returns [Some (k, d)] if [m] is a singleton map
     that maps [k] to [d]. Otherwise, it returns [None]. *)

  let is_singleton = function
    | Leaf (k, d) ->
        Some (k, d)
    | Empty
    | Branch _ ->
        None

  (* [is_empty m] returns [true] if and only if the map [m] defines no bindings at all. *)

  let is_empty = function
    | Empty ->
        true
    | Leaf _
    | Branch _ ->
        false

  (* [cardinal m] returns [m]'s cardinal, that is, the number of keys it binds, or, in other words, its domain's
     cardinal. *)

  let rec cardinal = function
    | Empty ->
        0
    | Leaf _ ->
        1
    | Branch (_, _, t0, t1) ->
        cardinal t0 + cardinal t1

  (* [remove k m] returns the map [m] deprived from any binding involving [k]. *)

  let remove key m =

    let rec remove = function
      | Empty ->
          raise Not_found
      | Leaf (key', _) ->
          if key = key' then
            Empty
          else
            raise Not_found
      | Branch (prefix, mask, tree0, tree1) ->
          if (key land mask) = 0 then
            match remove tree0 with
            | Empty ->
                tree1
            | tree0 ->
                Branch (prefix, mask, tree0, tree1)
          else
            match remove tree1 with
            | Empty ->
                tree0
            | tree1 ->
                Branch (prefix, mask, tree0, tree1) in

    try
      remove m
    with Not_found ->
      m

  (* [lookup_and_remove k m] looks up the value [v] associated to the key [k] in the map [m], and raises [Not_found]
     if no value is bound to [k]. The call returns the value [v], together with the map [m] deprived from the binding
     from [k] to [v]. *)

  let rec lookup_and_remove key = function
    | Empty ->
        raise Not_found
    | Leaf (key', data) ->
        if key = key' then
          data, Empty
        else
          raise Not_found
    | Branch (prefix, mask, tree0, tree1) ->
        if (key land mask) = 0 then
          match lookup_and_remove key tree0 with
          | data, Empty ->
              data, tree1
          | data, tree0 ->
              data, Branch (prefix, mask, tree0, tree1)
        else
          match lookup_and_remove key tree1 with
          | data, Empty ->
              data, tree0
          | data, tree1 ->
              data, Branch (prefix, mask, tree0, tree1)

  let find_and_remove =
    lookup_and_remove

  (* [fine_union decide m1 m2] returns the union of the maps [m1] and
     [m2]. If a key [k] is bound to [x1] (resp. [x2]) within [m1]
     (resp. [m2]), then [decide] is called. It is passed [x1] and
     [x2], and must return the value which shall be bound to [k] in
     the final map. The operation returns [m2] itself (as opposed to a
     copy of it) when its result is equal to [m2]. *)

  let reverse decision elem1 elem2 =
    decision elem2 elem1

  let fine_union decide m1 m2 =

    let rec union s t =
      match s, t with

      | Empty, _ ->
          t
      | (Leaf _ | Branch _), Empty ->
          s

      | Leaf(key, value), _ ->
          fine_add (reverse decide) key value t
      | Branch _, Leaf(key, value) ->
          fine_add decide key value s

      | Branch(p, m, s0, s1), Branch(q, n, t0, t1) ->
          if (p = q) && (m = n) then

            (* The trees have the same prefix. Merge their sub-trees. *)

            let u0 = union s0 t0
            and u1 = union s1 t1 in
            if t0 == u0 && t1 == u1 then t
            else Branch(p, m, u0, u1)

          else if (X.shorter m n) && (match_prefix q p m) then

            (* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)

            if (q land m) = 0 then
              Branch(p, m, union s0 t, s1)
            else
              Branch(p, m, s0, union s1 t)

          else if (X.shorter n m) && (match_prefix p q n) then

            (* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)

            if (p land n) = 0 then
              let u0 = union s t0 in
              if t0 == u0 then t
              else Branch(q, n, u0, t1)
            else
              let u1 = union s t1 in
              if t1 == u1 then t
              else Branch(q, n, t0, u1)

          else

            (* The prefixes disagree. *)

            join p s q t in

    union m1 m2

  (* [union m1 m2] returns the union of the maps [m1] and
     [m2]. Bindings in [m2] take precedence over those in [m1]. *)

  let union m1 m2 =
    fine_union (fun _d d' -> d') m1 m2

  (* [iter f m] invokes [f k x], in turn, for each binding from key [k] to element [x] in the map [m]. Keys are
     presented to [f] according to some unspecified, but fixed, order. *)

  let rec iter f = function
    | Empty ->
        ()
    | Leaf (key, data) ->
        f key data
    | Branch (_, _, tree0, tree1) ->
        iter f tree0;
        iter f tree1

  (* [fold f m seed] invokes [f k d accu], in turn, for each binding from key [k] to datum [d] in the map
     [m]. Keys are presented to [f] in increasing order according to the map's ordering. The initial value of
     [accu] is [seed]; then, at each new call, its value is the value returned by the previous invocation of [f]. The
     value returned by [fold] is the final value of [accu]. *)

  let rec fold f m accu =
    match m with
    | Empty ->
        accu
    | Leaf (key, data) ->
        f key data accu
    | Branch (_, _, tree0, tree1) ->
        fold f tree1 (fold f tree0 accu)

  (* [fold_rev] performs exactly the same job as [fold], but presents keys to [f] in the opposite order. *)

  let rec fold_rev f m accu =
    match m with
    | Empty ->
        accu
    | Leaf (key, data) ->
        f key data accu
    | Branch (_, _, tree0, tree1) ->
        fold_rev f tree0 (fold_rev f tree1 accu)

  (* It is valid to evaluate [iter2 f m1 m2] if and only if [m1] and [m2] have the same domain. Doing so invokes
     [f k x1 x2], in turn, for each key [k] bound to [x1] in [m1] and to [x2] in [m2]. Bindings are presented to [f]
     according to some unspecified, but fixed, order. *)

  let rec iter2 f t1 t2 =
    match t1, t2 with
    | Empty, Empty ->
        ()
    | Leaf (key1, data1), Leaf (key2, data2) ->
        assert (key1 = key2);
        f key1 (* for instance *) data1 data2
    | Branch (p1, m1, left1, right1), Branch (p2, m2, left2, right2) ->
        assert (p1 = p2);
        assert (m1 = m2);
        iter2 f left1 left2;
        iter2 f right1 right2
    | _, _ ->
        assert false

  (* [map f m] returns the map obtained by composing the map [m] with the function [f]; that is, the map
     $k\mapsto f(m(k))$. *)

  let rec map f = function
    | Empty ->
        Empty
    | Leaf (key, data) ->
        Leaf(key, f data)
    | Branch (p, m, tree0, tree1) ->
        Branch (p, m, map f tree0, map f tree1)

  (* [endo_map] is similar to [map], but attempts to physically share its result with its input. This saves
     memory when [f] is the identity function. *)

  let rec endo_map f tree =
    match tree with
    | Empty ->
        tree
    | Leaf (key, data) ->
        let data' = f data in
        if data == data' then
          tree
        else
          Leaf(key, data')
    | Branch (p, m, tree0, tree1) ->
        let tree0' = endo_map f tree0 in
        let tree1' = endo_map f tree1 in
        if (tree0' == tree0) && (tree1' == tree1) then
          tree
        else
          Branch (p, m, tree0', tree1')

  (* [filter f m] returns a copy of the map [m] where only the bindings
     that satisfy [f] have been retained. *)

  let filter f m =
    fold (fun key data accu ->
      if f key data then
        add key data accu
      else
        accu
    ) m empty

  (* [iterator m] returns a stateful iterator over the map [m]. *)

  (* TEMPORARY performance could be improved, see JCF's paper *)

  let iterator m =

    let remainder = ref [ m ] in

    let rec next () =
      match !remainder with
      | [] ->
          None
      | Empty :: parent ->
          remainder := parent;
          next()
      | (Leaf (key, data)) :: parent ->
          remainder := parent;
          Some (key, data)
      | (Branch(_, _, s0, s1)) :: parent ->
          remainder := s0 :: s1 :: parent;
          next () in

    next

  (* If [dcompare] is an ordering over data, then [compare dcompare]
     is an ordering over maps. *)

  exception Got of int

  let compare dcompare m1 m2 =
    let iterator2 = iterator m2 in
    try
      iter (fun key1 data1 ->
        match iterator2() with
        | None ->
            raise (Got 1)
        | Some (key2, data2) ->
            let c = Generic.compare key1 key2 in
            if c <> 0 then
              raise (Got c)
            else
              let c = dcompare data1 data2 in
              if c <> 0 then
                raise (Got c)
      ) m1;
      match iterator2() with
      | None ->
          0
      | Some _ ->
          -1
    with Got c ->
      c

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Patricia-tree-based sets} *)

(* To enhance code sharing, it would be possible to implement maps as sets of pairs, or (vice-versa) to implement
   sets as maps to the unit element. However, both possibilities introduce some space and time inefficiency. To
   avoid it, we define each structure separately. *)

module Domain = struct

  type element = int

  type t =
    | Empty
    | Leaf of int
    | Branch of int * X.mask * t * t

  (* The empty set. *)

  let empty =
    Empty

  (* [is_empty s] returns [true] if and only if the set [s] is empty. *)

  let is_empty = function
    | Empty ->
        true
    | Leaf _
    | Branch _ ->
        false

  (* [singleton x] returns a set whose only element is [x]. *)

  let singleton x =
    Leaf x

  (* [is_singleton s] tests whether [s] is a singleton set. *)

  let is_singleton = function
    | Leaf _ ->
        true
    | Empty
    | Branch _ ->
        false

  (* [choose s] returns an arbitrarily chosen element of [s], if [s]
     is nonempty, and raises [Not_found] otherwise. *)

  let rec choose = function
    | Empty ->
        raise Not_found
    | Leaf x ->
        x
    | Branch (_, _, tree0, _) ->
        choose tree0

  (* [cardinal s] returns [s]'s cardinal. *)

  let rec cardinal = function
    | Empty ->
        0
    | Leaf _ ->
        1
    | Branch (_, _, t0, t1) ->
        cardinal t0 + cardinal t1

  (* [mem x s] returns [true] if and only if [x] appears in the set [s]. *)

  let rec mem x = function
    | Empty ->
        false
    | Leaf x' ->
        x = x'
    | Branch (_, mask, tree0, tree1) ->
        mem x (if (x land mask) = 0 then tree0 else tree1)

  (* The auxiliary function [join] merges two trees in the simple case where their prefixes disagree. *)

  let join p0 t0 p1 t1 =
    let m = X.branching_bit p0 p1 in
    let p = X.mask p0 (* for instance *) m in
    if (p0 land m) = 0 then
      Branch(p, m, t0, t1)
    else
      Branch(p, m, t1, t0)

  (* [add x s] returns a set whose elements are all elements of [s], plus [x]. *)

  exception Unchanged

  let rec strict_add x t =
    match t with
    | Empty ->
        Leaf x
    | Leaf x0 ->
        if x = x0 then
          raise Unchanged
        else
          join x (Leaf x) x0 t
    | Branch (p, m, t0, t1) ->
        if match_prefix x p m then
          if (x land m) = 0 then Branch (p, m, strict_add x t0, t1)
          else Branch (p, m, t0, strict_add x t1)
        else
          join x (Leaf x) p t

  let add x s =
    try
      strict_add x s
    with Unchanged ->
      s

  (* [remove x s] returns a set whose elements are all elements of [s], except [x]. *)

  let remove x s =

    let rec strict_remove = function
      | Empty ->
          raise Not_found
      | Leaf x' ->
        if x = x' then
          Empty
        else
          raise Not_found
      | Branch (prefix, mask, tree0, tree1) ->
          if (x land mask) = 0 then
            match strict_remove tree0 with
            | Empty ->
                tree1
            | tree0 ->
                Branch (prefix, mask, tree0, tree1)
          else
            match strict_remove tree1 with
            | Empty ->
                tree0
            | tree1 ->
                Branch (prefix, mask, tree0, tree1) in

    try
      strict_remove s
    with Not_found ->
      s

  (* [union s1 s2] returns the union of the sets [s1] and [s2]. *)

  let rec union s t =
    match s, t with

    | Empty, _ ->
        t
    | _, Empty ->
        s

    | Leaf x, _ ->
        add x t
    | _, Leaf x ->
        add x s

    | Branch(p, m, s0, s1), Branch(q, n, t0, t1) ->
        if (p = q) && (m = n) then

          (* The trees have the same prefix. Merge their sub-trees. *)

          let u0 = union s0 t0
          and u1 = union s1 t1 in
          if t0 == u0 && t1 == u1 then t
          else Branch(p, m, u0, u1)

        else if (X.shorter m n) && (match_prefix q p m) then

          (* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)

          if (q land m) = 0 then
            Branch(p, m, union s0 t, s1)
          else
            Branch(p, m, s0, union s1 t)

        else if (X.shorter n m) && (match_prefix p q n) then

          (* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)

          if (p land n) = 0 then
            let u0 = union s t0 in
            if t0 == u0 then t
            else Branch(q, n, u0, t1)
          else
            let u1 = union s t1 in
            if t1 == u1 then t
            else Branch(q, n, t0, u1)

        else

          (* The prefixes disagree. *)

          join p s q t

  (* [build] is a ``smart constructor''. It builds a [Branch] node with the specified arguments, but ensures
     that the newly created node does not have an [Empty] child. *)

  let build p m t0 t1 =
    match t0, t1 with
    |   Empty, Empty ->
        Empty
    |   Empty, _ ->
        t1
    |   _, Empty ->
        t0
    |   _, _ ->
        Branch(p, m, t0, t1)

  (* [inter s t] returns the set intersection of [s] and [t], that is, $s\cap t$. *)

  let rec inter s t =
    match s, t with

    | Empty, _
    | _, Empty ->
        Empty

    | (Leaf x as s), t
    | t, (Leaf x as s) ->
        if mem x t then s else Empty

    | Branch(p, m, s0, s1), Branch(q, n, t0, t1) ->
        if (p = q) && (m = n) then

          (* The trees have the same prefix. Compute the intersections of their sub-trees. *)

          build p m (inter s0 t0) (inter s1 t1)

        else if (X.shorter m n) && (match_prefix q p m) then

          (* [q] contains [p]. Intersect [t] with a sub-tree of [s]. *)

          inter (if (q land m) = 0 then s0 else s1) t

        else if (X.shorter n m) && (match_prefix p q n) then

          (* [p] contains [q]. Intersect [s] with a sub-tree of [t]. *)

          inter s (if (p land n) = 0 then t0 else t1)

        else

          (* The prefixes disagree. *)

          Empty

  (* [disjoint s1 s2] returns [true] if and only if the sets [s1] and [s2] are disjoint, i.e. iff their intersection
     is empty. It is a specialized version of [inter], which uses less space. *)

  exception NotDisjoint

  let disjoint s t =

    let rec inter s t =
      match s, t with

      | Empty, _
      | _, Empty ->
          ()

      | Leaf x, _ ->
          if mem x t then
            raise NotDisjoint
      | _, Leaf x ->
          if mem x s then
            raise NotDisjoint

      | Branch(p, m, s0, s1), Branch(q, n, t0, t1) ->
          if (p = q) && (m = n) then begin
            inter s0 t0;
            inter s1 t1
          end
          else if (X.shorter m n) && (match_prefix q p m) then
            inter (if (q land m) = 0 then s0 else s1) t
          else if (X.shorter n m) && (match_prefix p q n) then
            inter s (if (p land n) = 0 then t0 else t1)
          else
            () in

    try
      inter s t;
      true
    with NotDisjoint ->
      false

  (* [iter f s] invokes [f x], in turn, for each element [x] of the set [s]. Elements are presented to [f] according
     to some unspecified, but fixed, order. *)

  let rec iter f = function
    | Empty ->
        ()
    | Leaf x ->
        f x
    | Branch (_, _, tree0, tree1) ->
        iter f tree0;
        iter f tree1

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x] of the set [s]. Elements are presented to [f]
     according to some unspecified, but fixed, order. The initial value of [accu] is [seed]; then, at each new call,
     its value is the value returned by the previous invocation of [f]. The value returned by [fold] is the final
     value of [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 < \ldots < x_n$, then
     [fold f s seed] computes $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  let rec fold f s accu =
    match s with
    |   Empty ->
        accu
    |   Leaf x ->
        f x accu
    |   Branch (_, _, s0, s1) ->
        fold f s1 (fold f s0 accu)

  (* [elements s] is a list of all elements in the set [s]. *)

  let elements s =
    fold (fun tl hd -> tl :: hd) s []

  (* [iterator s] returns a stateful iterator over the set [s]. That is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where
     $x_1 < x_2 < \ldots < x_n$, then [iterator s] is a function which, when invoked for the $k^{\text{th}}$ time,
     returns [Some]$x_k$, if $k\leq n$, and [None] otherwise. Such a function can be useful when one wishes to
     iterate over a set's elements, without being restricted by the call stack's discipline.

     For more comments about this algorithm, please see module [Baltree], which defines a similar one. *)

  let iterator s =

    let remainder = ref [ s ] in

    let rec next () =
      match !remainder with
      | [] ->
          None
      | Empty :: parent ->
          remainder := parent;
          next()
      | (Leaf x) :: parent ->
          remainder := parent;
          Some x
      | (Branch(_, _, s0, s1)) :: parent ->
          remainder := s0 :: s1 :: parent;
          next () in

    next

  (* [compare] is an ordering over sets. *)

  exception Got of int

  let compare s1 s2 =
    let iterator2 = iterator s2 in
    try
      iter (fun x1 ->
        match iterator2() with
        | None ->
            raise (Got 1)
        | Some x2 ->
            let c = Generic.compare x1 x2 in
            if c <> 0 then
              raise (Got c)
      ) s1;
      match iterator2() with
      | None ->
          0
      | Some _ ->
          -1
    with Got c ->
      c

  (* [equal] implements equality over sets. *)

  let equal s1 s2 =
    compare s1 s2 = 0

  (* [subset] implements the subset predicate over sets. In other words, [subset s t] returns [true] if and only if
     $s\subseteq t$. It is a specialized version of [diff]. *)

  exception NotSubset

  let subset s t =

    let rec diff s t =
      match s, t with

      | Empty, _ ->
          ()
      | _, Empty

      | Branch _, Leaf _ ->
          raise NotSubset
      | Leaf x, _ ->
          if not (mem x t) then
            raise NotSubset

      | Branch(p, m, s0, s1), Branch(q, n, t0, t1) ->

          if (p = q) && (m = n) then begin

            diff s0 t0;
            diff s1 t1

          end
          else if (X.shorter n m) && (match_prefix p q n) then

            diff s (if (p land n) = 0 then t0 else t1)

          else

            (* Either [q] contains [p], which means at least one of [s]'s sub-trees is not contained within [t],
               or the prefixes disagree. In either case, the subset relationship cannot possibly hold. *)

            raise NotSubset in

    try
      diff s t;
      true
    with NotSubset ->
      false

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Relating sets and maps} *)

  (* Back to the world of maps. Let us now describe the relationship which exists between maps and their domains. *)

  (* [domain m] returns [m]'s domain. *)

  let rec domain = function
    | Empty ->
        Domain.Empty
    | Leaf (k, _) ->
        Domain.Leaf k
    | Branch (p, m, t0, t1) ->
        Domain.Branch (p, m, domain t0, domain t1)

  (* [lift f s] returns the map $k\mapsto f(k)$, where $k$ ranges over a set of keys [s]. *)

  let rec lift f = function
    | Domain.Empty ->
        Empty
    | Domain.Leaf k ->
        Leaf (k, f k)
    | Domain.Branch (p, m, t0, t1) ->
        Branch(p, m, lift f t0, lift f t1)

  (* [build] is a ``smart constructor''. It builds a [Branch] node with the specified arguments, but ensures
     that the newly created node does not have an [Empty] child. *)

  let build p m t0 t1 =
    match t0, t1 with
    | Empty, Empty ->
        Empty
    | Empty, _ ->
        t1
    | _, Empty ->
        t0
    | _, _ ->
        Branch(p, m, t0, t1)

  (* [corestrict m d] performs a co-restriction of the map [m] to the domain [d]. That is, it returns the map
     $k\mapsto m(k)$, where $k$ ranges over all keys bound in [m] but \emph{not} present in [d]. Its code resembles
     [diff]'s. *)

    let rec corestrict s t =
      match s, t with

      | Empty, _
      | _, Domain.Empty ->
          s

      | Leaf (k, _), _ ->
          if Domain.mem k t then Empty else s
      | _, Domain.Leaf k ->
          remove k s

      | Branch(p, m, s0, s1), Domain.Branch(q, n, t0, t1) ->
          if (p = q) && (m = n) then

            build p m (corestrict s0 t0) (corestrict s1 t1)

          else if (X.shorter m n) && (match_prefix q p m) then

            if (q land m) = 0 then
              build p m (corestrict s0 t) s1
            else
              build p m s0 (corestrict s1 t)

          else if (X.shorter n m) && (match_prefix p q n) then

            corestrict s (if (p land n) = 0 then t0 else t1)

          else

            s

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Instantiating the functor} *)

module Little = Make(Endianness.Little)

module Big = Make(Endianness.Big)
