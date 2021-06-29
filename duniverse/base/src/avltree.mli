(** A low-level, mutable AVL tree.

    It is not intended to be used directly by casual users. It is used for implementing
    other data structures. The interface is somewhat ugly, and it's that way for a
    reason: the goal of this module is minimum memory overhead and maximum performance.

    {2 Caveats}

    1. [compare] is passed to every function where it is used. If you pass a different
    [compare] to functions on the same tree, then behavior is indeterminate. Why? Because
    otherwise we'd need a top-level record to store [compare], and when building a hash
    table, or other structure, that little [t] is a block that increases memory
    overhead. However, if an empty tree is just a constructor [Empty], then it's just a
    number, and uses no extra memory beyond the array bucket that holds it. That's the
    first secret of how Hashtbl's memory overhead isn't higher than INRIA's, even though
    it uses a tree instead of a list for buckets.

    2. But if it's mutable, why do all the "mutators" return [t]? Answer: it is mutable,
    but the root node might change due to balancing. Since we have no top-level record to
    hold the current root node (see point 1), you have to do it. If you fail to do it, and
    use an old root node, you're responsible for the (sure to be nasty) consequences.

    3. What on earth is up with the [~removed] argument to some functions? See point 1:
    since there is no top-level node, it isn't possible to keep track of how many nodes
    are in the tree unless each mutator tells you whether or not it added or removed a
    node (vs. replacing an existing one). If you intend to keep a count (as you must in a
    hash table), then you will need to pay attention to this flag.

    After all this, you're probably asking yourself whether all these hacks are worth
    it. Yes! They are! With them, we built a hash table that is faster than INRIA's (no
    small feat) with the same memory overhead, sane add semantics (the add semantics they
    used were a performance hack), and worst-case log(N) insertion, lookup, and
    removal. *)

open! Import

(** We expose [t] to allow an optimization in Hashtbl that makes iter and fold more than
    twice as fast.  We keep the type private to reduce opportunities for external code to
    violate avltree invariants. *)
type ('k, 'v) t = private
  | Empty
  | Node of
      { mutable left : ('k, 'v) t
      ; key : 'k
      ; mutable value : 'v
      ; mutable height : int
      ; mutable right : ('k, 'v) t
      }
  | Leaf of
      { key : 'k
      ; mutable value : 'v
      }

val empty : ('k, 'v) t
val is_empty : _ t -> bool

(** Checks invariants, raising an exception if any invariants fail. *)
val invariant : ('k, 'v) t -> compare:('k -> 'k -> int) -> unit

(** Adds the specified key and data to the tree destructively (previous [t]'s are no
    longer valid) using the specified comparison function. O(log(N)) time, O(1) space.

    The returned [t] is the new root node of the tree, and should be used on all further
    calls to any other function in this module. The bool [ref], added, will be set to
    [true] if a new node is added to the tree, or [false] if an existing node is replaced
    (in the case that the key already exists).

    If [replace] (default true) is true then [add] will overwrite any existing mapping for
    [key]. If [replace] is false, and there is an existing mapping for key, then [add] has
    no effect. *)
val add
  :  ('k, 'v) t
  -> replace:bool
  -> compare:('k -> 'k -> int)
  -> added:bool ref
  -> key:'k
  -> data:'v
  -> ('k, 'v) t

(** Returns the first (leftmost) or last (rightmost) element in the tree. *)

val first : ('k, 'v) t -> ('k * 'v) option
val last : ('k, 'v) t -> ('k * 'v) option

(** If the specified key exists in the tree, returns the corresponding value.  O(log(N))
    time and O(1) space. *)
val find : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> 'v option

(** [find_and_call t ~compare k ~if_found ~if_not_found]

    is equivalent to:

    [match find t ~compare k with Some v -> if_found v | None -> if_not_found k]

    except that it doesn't allocate the option. *)
val find_and_call
  :  ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> 'k
  -> if_found:('v -> 'a)
  -> if_not_found:('k -> 'a)
  -> 'a

val find_and_call1
  :  ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> 'k
  -> a:'a
  -> if_found:('v -> 'a -> 'b)
  -> if_not_found:('k -> 'a -> 'b)
  -> 'b

val find_and_call2
  :  ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> 'k
  -> a:'a
  -> b:'b
  -> if_found:('v -> 'a -> 'b -> 'c)
  -> if_not_found:('k -> 'a -> 'b -> 'c)
  -> 'c

val findi_and_call
  :  ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> 'k
  -> if_found:(key:'k -> data:'v -> 'a)
  -> if_not_found:('k -> 'a)
  -> 'a

val findi_and_call1
  :  ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> 'k
  -> a:'a
  -> if_found:(key:'k -> data:'v -> 'a -> 'b)
  -> if_not_found:('k -> 'a -> 'b)
  -> 'b

val findi_and_call2
  :  ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> 'k
  -> a:'a
  -> b:'b
  -> if_found:(key:'k -> data:'v -> 'a -> 'b -> 'c)
  -> if_not_found:('k -> 'a -> 'b -> 'c)
  -> 'c

(** Returns true if key is present in the tree, and false otherwise. *)
val mem : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> bool

(** Removes key destructively from the tree if it exists, returning the new root node.
    Previous root nodes are not usable anymore; do so at your peril. The [removed] ref
    will be set to true if a node was actually removed, and false otherwise. *)
val remove
  :  ('k, 'v) t
  -> removed:bool ref
  -> compare:('k -> 'k -> int)
  -> 'k
  -> ('k, 'v) t

(** Folds over the tree. *)
val fold : ('k, 'v) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

(** Iterates over the tree. *)
val iter : ('k, 'v) t -> f:(key:'k -> data:'v -> unit) -> unit

(** Map over the the tree, changing the data in place. *)
val mapi_inplace : ('k, 'v) t -> f:(key:'k -> data:'v -> 'v) -> unit

val choose_exn : ('k, 'v) t -> 'k * 'v
