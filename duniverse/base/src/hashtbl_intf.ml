open! Import

module Key = struct
  module type S = sig
    type t [@@deriving_inline compare, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]

    (** Two [t]s that [compare] equal must have equal hashes for the hashtable
        to behave properly. *)
    val hash : t -> int
  end

  type 'a t = (module S with type t = 'a)
end

module Merge_into_action = struct
  type 'a t =
    | Remove
    | Set_to of 'a
end

module type Accessors = sig
  (** {2 Accessors} *)

  type ('a, 'b) t
  type 'a key

  val sexp_of_key : ('a, _) t -> 'a key -> Sexp.t
  val clear : (_, _) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t

  (** Attempting to modify ([set], [remove], etc.) the hashtable during iteration ([fold],
      [iter], [iter_keys], [iteri]) will raise an exception. *)
  val fold : ('a, 'b) t -> init:'c -> f:(key:'a key -> data:'b -> 'c -> 'c) -> 'c

  val iter_keys : ('a, _) t -> f:('a key -> unit) -> unit
  val iter : (_, 'b) t -> f:('b -> unit) -> unit

  (** Iterates over both keys and values.

      Example:

      {v
      let h = Hashtbl.of_alist_exn (module Int) [(1, 4); (5, 6)] in
      Hashtbl.iteri h ~f:(fun ~key ~data ->
        print_endline (Printf.sprintf "%d-%d" key data));;
      1-4
      5-6
      - : unit = ()
      v} *)
  val iteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit

  val existsi : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> bool
  val exists : (_, 'b) t -> f:('b -> bool) -> bool
  val for_alli : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> bool
  val for_all : (_, 'b) t -> f:('b -> bool) -> bool
  val counti : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> int
  val count : (_, 'b) t -> f:('b -> bool) -> int
  val length : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val mem : ('a, _) t -> 'a key -> bool
  val remove : ('a, _) t -> 'a key -> unit
  val choose : ('a, 'b) t -> ('a key * 'b) option
  val choose_exn : ('a, 'b) t -> 'a key * 'b

  (** Sets the given [key] to [data]. *)
  val set : ('a, 'b) t -> key:'a key -> data:'b -> unit

  (** [add] and [add_exn] leave the table unchanged if the key was already present. *)
  val add : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]

  val add_exn : ('a, 'b) t -> key:'a key -> data:'b -> unit

  (** [change t key ~f] changes [t]'s value for [key] to be [f (find t key)]. *)
  val change : ('a, 'b) t -> 'a key -> f:('b option -> 'b option) -> unit

  (** [update t key ~f] is [change t key ~f:(fun o -> Some (f o))]. *)
  val update : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> unit

  (** [map t f] returns a new table with values replaced by the result of applying [f]
      to the current values.

      Example:

      {v
      let h = Hashtbl.of_alist_exn (module Int) [(1, 4); (5, 6)] in
      let h' = Hashtbl.map h ~f:(fun x -> x * 2) in
      Hashtbl.to_alist h';;
      - : (int * int) list = [(5, 12); (1, 8)]
      v} *)
  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

  (** Like [map], but the function [f] takes both key and data as arguments. *)
  val mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t

  (** Returns a new table by filtering the given table's values by [f]: the keys for which
      [f] applied to the current value returns [Some] are kept, and those for which it
      returns [None] are discarded.

      Example:

      {v
      let h = Hashtbl.of_alist_exn (module Int) [(1, 4); (5, 6)] in
      Hashtbl.filter_map h ~f:(fun x -> if x > 5 then Some x else None)
      |> Hashtbl.to_alist;;
      - : (int * int) list = [(5, 6)]
      v} *)
  val filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t

  (** Like [filter_map], but the function [f] takes both key and data as arguments. *)
  val filter_mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t

  val filter_keys : ('a, 'b) t -> f:('a key -> bool) -> ('a, 'b) t
  val filter : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t
  val filteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t

  (** Returns new tables with bound values partitioned by [f] applied to the bound
      values. *)
  val partition_map
    :  ('a, 'b) t
    -> f:('b -> ('c, 'd) Either.t)
    -> ('a, 'c) t * ('a, 'd) t

  (** Like [partition_map], but the function [f] takes both key and data as arguments. *)
  val partition_mapi
    :  ('a, 'b) t
    -> f:(key:'a key -> data:'b -> ('c, 'd) Either.t)
    -> ('a, 'c) t * ('a, 'd) t

  (** Returns a pair of tables [(t1, t2)], where [t1] contains all the elements of the
      initial table which satisfy the predicate [f], and [t2] contains the rest. *)
  val partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t

  (** Like [partition_tf], but the function [f] takes both key and data as arguments. *)
  val partitioni_tf
    :  ('a, 'b) t
    -> f:(key:'a key -> data:'b -> bool)
    -> ('a, 'b) t * ('a, 'b) t

  (** [find_or_add t k ~default] returns the data associated with key [k] if it is in the
      table [t], and otherwise assigns [k] the value returned by [default ()]. *)
  val find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b

  (** Like [find_or_add] but [default] takes the key as an argument. *)
  val findi_or_add : ('a, 'b) t -> 'a key -> default:('a key -> 'b) -> 'b

  (** [find t k] returns [Some] (the current binding) of [k] in [t], or [None] if no such
      binding exists. *)
  val find : ('a, 'b) t -> 'a key -> 'b option

  (** [find_exn t k] returns the current binding of [k] in [t], or raises [Caml.Not_found]
      or [Not_found_s] if no such binding exists. *)
  val find_exn : ('a, 'b) t -> 'a key -> 'b

  (** [find_and_call t k ~if_found ~if_not_found]

      is equivalent to:

      [match find t k with Some v -> if_found v | None -> if_not_found k]

      except that it doesn't allocate the option. *)
  val find_and_call
    :  ('a, 'b) t
    -> 'a key
    -> if_found:('b -> 'c)
    -> if_not_found:('a key -> 'c)
    -> 'c

  (** Just like [find_and_call], but takes an extra argument which is passed to [if_found]
      and [if_not_found], so that the client code can avoid allocating closures or using
      refs to pass this additional information.  This function is only useful in code
      which tries to minimize heap allocation. *)
  val find_and_call1
    :  ('a, 'b) t
    -> 'a key
    -> a:'d
    -> if_found:('b -> 'd -> 'c)
    -> if_not_found:('a key -> 'd -> 'c)
    -> 'c

  val find_and_call2
    :  ('a, 'b) t
    -> 'a key
    -> a:'d
    -> b:'e
    -> if_found:('b -> 'd -> 'e -> 'c)
    -> if_not_found:('a key -> 'd -> 'e -> 'c)
    -> 'c

  val findi_and_call
    :  ('a, 'b) t
    -> 'a key
    -> if_found:(key:'a key -> data:'b -> 'c)
    -> if_not_found:('a key -> 'c)
    -> 'c

  val findi_and_call1
    :  ('a, 'b) t
    -> 'a key
    -> a:'d
    -> if_found:(key:'a key -> data:'b -> 'd -> 'c)
    -> if_not_found:('a key -> 'd -> 'c)
    -> 'c

  val findi_and_call2
    :  ('a, 'b) t
    -> 'a key
    -> a:'d
    -> b:'e
    -> if_found:(key:'a key -> data:'b -> 'd -> 'e -> 'c)
    -> if_not_found:('a key -> 'd -> 'e -> 'c)
    -> 'c

  (** [find_and_remove t k] returns Some (the current binding) of k in t and removes it,
      or None is no such binding exists. *)
  val find_and_remove : ('a, 'b) t -> 'a key -> 'b option

  (** Merges two hashtables.

      The result of [merge f h1 h2] has as keys the set of all [k] in the union of the
      sets of keys of [h1] and [h2] for which [d(k)] is not None, where:

      d(k) =
      - [f ~key:k (`Left d1)]
        if [k] in [h1] maps to d1, and [h2] does not have data for [k];

      - [f ~key:k (`Right d2)]
        if [k] in [h2] maps to d2, and [h1] does not have data for [k];

      - [f ~key:k (`Both (d1, d2))]
        otherwise, where [k] in [h1] maps to [d1] and [k] in [h2] maps to [d2].

      Each key [k] is mapped to a single piece of data [x], where [d(k) = Some x].

      Example:

      {v
      let h1 = Hashtbl.of_alist_exn (module Int) [(1, 5); (2, 3232)] in
      let h2 = Hashtbl.of_alist_exn (module Int) [(1, 3)] in
      Hashtbl.merge h1 h2 ~f:(fun ~key:_ -> function
        | `Left x -> Some (`Left x)
        | `Right x -> Some (`Right x)
        | `Both (x, y) -> if x=y then None else Some (`Both (x,y))
      ) |> Hashtbl.to_alist;;
      - : (int * [> `Both of int * int | `Left of int | `Right of int ]) list =
      [(2, `Left 3232); (1, `Both (5, 3))]
      v} *)
  val merge
    :  ('k, 'a) t
    -> ('k, 'b) t
    -> f:(key:'k key -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option)
    -> ('k, 'c) t


  (** Every [key] in [src] will be removed or set in [dst] according to the return value
      of [f]. *)
  val merge_into
    :  src:('k, 'a) t
    -> dst:('k, 'b) t
    -> f:(key:'k key -> 'a -> 'b option -> 'b Merge_into_action.t)
    -> unit

  (** Returns the list of all keys for given hashtable. *)
  val keys : ('a, _) t -> 'a key list

  (** Returns the list of all data for given hashtable. *)
  val data : (_, 'b) t -> 'b list

  (** [filter_inplace t ~f] removes all the elements from [t] that don't satisfy [f]. *)
  val filter_keys_inplace : ('a, _) t -> f:('a key -> bool) -> unit

  val filter_inplace : (_, 'b) t -> f:('b -> bool) -> unit
  val filteri_inplace : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> unit

  (** [map_inplace t ~f] applies [f] to all elements in [t], transforming them in
      place. *)
  val map_inplace : (_, 'b) t -> f:('b -> 'b) -> unit

  val mapi_inplace : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'b) -> unit

  (** [filter_map_inplace] combines the effects of [map_inplace] and [filter_inplace]. *)
  val filter_map_inplace : (_, 'b) t -> f:('b -> 'b option) -> unit

  val filter_mapi_inplace : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'b option) -> unit

  (** [equal f t1 t2] and [similar f t1 t2] both return true iff [t1] and [t2] have the
      same keys and for all keys [k], [f (find_exn t1 k) (find_exn t2 k)].  [equal] and
      [similar] only differ in their types. *)
  val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val similar : ('b1 -> 'b2 -> bool) -> ('a, 'b1) t -> ('a, 'b2) t -> bool

  (** Returns the list of all (key, data) pairs for given hashtable. *)
  val to_alist : ('a, 'b) t -> ('a key * 'b) list

  val validate
    :  name:('a key -> string)
    -> 'b Validate.check
    -> ('a, 'b) t Validate.check


  (** [remove_if_zero]'s default is [false]. *)
  val incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit

  val decr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
end

module type Multi = sig
  type ('a, 'b) t
  type 'a key

  (** [add_multi t ~key ~data] if [key] is present in the table then cons
      [data] on the list, otherwise add [key] with a single element list. *)
  val add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit

  (** [remove_multi t key] updates the table, removing the head of the list bound to
      [key]. If the list has only one element (or is empty) then the binding is
      removed. *)
  val remove_multi : ('a, _ list) t -> 'a key -> unit

  (** [find_multi t key] returns the empty list if [key] is not present in the table,
      returns [t]'s values for [key] otherwise. *)
  val find_multi : ('a, 'b list) t -> 'a key -> 'b list
end

type ('key, 'data, 'z) create_options =
  ?growth_allowed:bool (** defaults to [true] *)
  -> ?size:int (** initial size -- default 0 *)
  -> 'key Key.t
  -> 'z

type ('key, 'data, 'z) create_options_without_first_class_module =
  ?growth_allowed:bool (** defaults to [true] *)
  -> ?size:int (** initial size -- default 0 *)
  -> 'z

module type Creators_generic = sig
  type ('a, 'b) t
  type 'a key
  type ('key, 'data, 'z) create_options

  val create : ('a key, 'b, unit -> ('a, 'b) t) create_options


  val of_alist
    : ( 'a key
      , 'b
      , ('a key * 'b) list -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a key ] )
        create_options

  val of_alist_report_all_dups
    : ( 'a key
      , 'b
      , ('a key * 'b) list -> [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a key list ] )
        create_options

  val of_alist_or_error
    : ('a key, 'b, ('a key * 'b) list -> ('a, 'b) t Or_error.t) create_options

  val of_alist_exn : ('a key, 'b, ('a key * 'b) list -> ('a, 'b) t) create_options

  val of_alist_multi
    : ('a key, 'b list, ('a key * 'b) list -> ('a, 'b list) t) create_options


  (** {[ create_mapped get_key get_data [x1,...,xn]
         = of_alist [get_key x1, get_data x1; ...; get_key xn, get_data xn] ]} *)
  val create_mapped
    : ( 'a key
      , 'b
      , get_key:('r -> 'a key)
        -> get_data:('r -> 'b)
        -> 'r list
        -> [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a key list ] )
        create_options


  (** {[ create_with_key ~get_key [x1,...,xn]
         = of_alist [get_key x1, x1; ...; get_key xn, xn] ]} *)
  val create_with_key
    : ( 'a key
      , 'r
      , get_key:('r -> 'a key)
        -> 'r list
        -> [ `Ok of ('a, 'r) t | `Duplicate_keys of 'a key list ] )
        create_options

  val create_with_key_or_error
    : ( 'a key
      , 'r
      , get_key:('r -> 'a key) -> 'r list -> ('a, 'r) t Or_error.t )
        create_options

  val create_with_key_exn
    : ('a key, 'r, get_key:('r -> 'a key) -> 'r list -> ('a, 'r) t) create_options


  val group
    : ( 'a key
      , 'b
      , get_key:('r -> 'a key)
        -> get_data:('r -> 'b)
        -> combine:('b -> 'b -> 'b)
        -> 'r list
        -> ('a, 'b) t )
        create_options
end

module type Creators = sig
  type ('a, 'b) t

  (** {2 Creators} *)

  (** The module you pass to [create] must have a type that is hashable, sexpable, and
      comparable.

      Example:

      {v
        Hashtbl.create (module Int);;
        - : (int, '_a) Hashtbl.t = <abstr>;;
      v} *)
  val create
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> ('a, 'b) t

  (** Example:

      {v
         Hashtbl.of_alist (module Int) [(3, "something"); (2, "whatever")]
         - : [ `Duplicate_key of int | `Ok of (int, string) Hashtbl.t ] = `Ok <abstr>
      v} *)
  val of_alist
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> ('a * 'b) list
    -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]

  (** Whereas [of_alist] will report [Duplicate_key] no matter how many dups there are in
      your list, [of_alist_report_all_dups] will report each and every duplicate entry.

      For example:

      {v
        Hashtbl.of_alist (module Int) [(1, "foo"); (1, "bar"); (2, "foo"); (2, "bar")];;
        - : [ `Duplicate_key of int | `Ok of (int, string) Hashtbl.t ] = `Duplicate_key 1

        Hashtbl.of_alist_report_all_dups (module Int) [(1, "foo"); (1, "bar"); (2, "foo"); (2, "bar")];;
        - : [ `Duplicate_keys of int list | `Ok of (int, string) Hashtbl.t ] = `Duplicate_keys [1; 2]
      v} *)
  val of_alist_report_all_dups
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> ('a * 'b) list
    -> [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a list ]

  val of_alist_or_error
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> ('a * 'b) list
    -> ('a, 'b) t Or_error.t

  val of_alist_exn
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> ('a * 'b) list
    -> ('a, 'b) t

  (** Creates a {{!Multi} "multi"} hashtable, i.e., a hashtable where each key points to a
      list potentially containing multiple values. So instead of short-circuiting with a
      [`Duplicate_key] variant on duplicates, as in [of_alist], [of_alist_multi] folds
      those values into a list for the given key:

      {v
      let h = Hashtbl.of_alist_multi (module Int) [(1, "a"); (1, "b"); (2, "c"); (2, "d")];;
      val h : (int, string list) Hashtbl.t = <abstr>

      Hashtbl.find_exn h 1;;
      - : string list = ["b"; "a"]
      v} *)
  val of_alist_multi
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> ('a * 'b) list
    -> ('a, 'b list) t

  (** Applies the [get_key] and [get_data] functions to the ['r list] to create the
      initial keys and values, respectively, for the new hashtable.

      {[ create_mapped get_key get_data [x1;...;xn]
         = of_alist [get_key x1, get_data x1; ...; get_key xn, get_data xn]
      ]}

      Example:

      {v
        let h =
          Hashtbl.create_mapped (module Int)
            ~get_key:(fun x -> x)
            ~get_data:(fun x -> x + 1)
           [1; 2; 3];;
        val h : [ `Duplicate_keys of int list | `Ok of (int, int) Hashtbl.t ] = `Ok <abstr>

        let h =
          match h with
          | `Ok x -> x
          | `Duplicate_keys _ -> failwith ""
        in
        Hashtbl.find_exn h 1;;
        - : int = 2
      v} *)
  val create_mapped
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> get_key:('r -> 'a)
    -> get_data:('r -> 'b)
    -> 'r list
    -> [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a list ]

  (** {[ create_with_key ~get_key [x1;...;xn]
         = of_alist [get_key x1, x1; ...; get_key xn, xn] ]} *)
  val create_with_key
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> get_key:('r -> 'a)
    -> 'r list
    -> [ `Ok of ('a, 'r) t | `Duplicate_keys of 'a list ]

  val create_with_key_or_error
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> get_key:('r -> 'a)
    -> 'r list
    -> ('a, 'r) t Or_error.t

  val create_with_key_exn
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> get_key:('r -> 'a)
    -> 'r list
    -> ('a, 'r) t

  (** Like [create_mapped], applies the [get_key] and [get_data] functions to the ['r
      list] to create the initial keys and values, respectively, for the new hashtable --
      and then, like [add_multi], folds together values belonging to the same keys. Here,
      though, the function used for the folding is given by [combine] (instead of just
      being a [cons]).

      Example:

      {v
         Hashtbl.group (module Int)
           ~get_key:(fun x -> x / 2)
           ~get_data:(fun x -> x)
           ~combine:(fun x y -> x * y)
            [ 1; 2; 3; 4]
         |> Hashtbl.to_alist;;
         - : (int * int) list = [(2, 4); (1, 6); (0, 1)]
       v} *)
  val group
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> get_key:('r -> 'a)
    -> get_data:('r -> 'b)
    -> combine:('b -> 'b -> 'b)
    -> 'r list
    -> ('a, 'b) t
end

module type S_without_submodules = sig
  val hash : 'a -> int
  val hash_param : int -> int -> 'a -> int

  type ('a, 'b) t

  (** We provide a [sexp_of_t] but not a [t_of_sexp] for this type because one needs to be
      explicit about the hash and comparison functions used when creating a hashtable.
      Note that [Hashtbl.Poly.t] does have [[@@deriving sexp]], and uses OCaml's built-in
      polymorphic comparison and and polymorphic hashing. *)
  val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t

  include Creators with type ('a, 'b) t := ('a, 'b) t (** @inline *)

  include
    Accessors with type ('a, 'b) t := ('a, 'b) t with type 'a key = 'a
  (** @inline *)


  include
    Multi with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a key
  (** @inline *)

  val hashable_s : ('key, _) t -> 'key Key.t

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t
end

module type S_poly = sig
  type ('a, 'b) t [@@deriving_inline sexp]

  include Ppx_sexp_conv_lib.Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t

  [@@@end]

  val hashable : 'a Hashable.t

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

  include
    Creators_generic
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key = 'a
    with type ('key, 'data, 'z) create_options :=
      ('key, 'data, 'z) create_options_without_first_class_module

  include Accessors with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a key
  include Multi with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a key
end

module type For_deriving = sig
  type ('k, 'v) t

  module type Sexp_of_m = sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end

  module type M_of_sexp = sig
    type t [@@deriving_inline of_sexp]

    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

    [@@@end]

    include Key.S with type t := t
  end

  val sexp_of_m__t
    :  (module Sexp_of_m with type t = 'k)
    -> ('v -> Sexp.t)
    -> ('k, 'v) t
    -> Sexp.t

  val m__t_of_sexp
    :  (module M_of_sexp with type t = 'k)
    -> (Sexp.t -> 'v)
    -> Sexp.t
    -> ('k, 'v) t
end

module type Hashtbl = sig
  (** A hash table is a mutable data structure implementing a map between keys and values.
      It supports constant-time lookup and in-place modification.

      {1 Usage}

      As a simple example, we'll create a hash table with string keys using the
      {{!create}[create]} constructor, which expects a module defining the key's type:

      {[
        let h = Hashtbl.create (module String);;
        val h : (string, '_a) Hashtbl.t = <abstr>
      ]}

      We can set the values of individual keys with {{!set}[set]}. If the key already has
      a value, it will be overwritten.

      {v
      Hashtbl.set h ~key:"foo" ~data:5;;
      - : unit = ()

      Hashtbl.set h ~key:"foo" ~data:6;;
      - : unit = ()

      Hashtbl.set h ~key:"bar" ~data:6;;
      - : unit = ()
      v}

      We can access values by key, or dump all of the hash table's data:

      {v
      Hashtbl.find h "foo";;
      - : int option = Some 6

      Hashtbl.find_exn h "foo";;
      - : int = 6

      Hashtbl.to_alist h;;
      - : (string * int) list = [("foo", 6); ("bar", 6)]
      v}

      {{!change}[change]} lets us change a key's value by applying the given function:

      {v
      Hashtbl.change h "foo" (fun x ->
       match x with
       | Some x -> Some (x * 2)
       | None -> None
      );;
      - : unit = ()

      Hashtbl.to_alist h;;
      - : (string * int) list = [("foo", 12); ("bar", 6)]
      v}


      We can use {{!merge}[merge]} to merge two hashtables with fine-grained control over
      how we choose values when a key is present in the first ("left") hashtable, the
      second ("right"), or both. Here, we'll cons the values when both hashtables have a
      key:

      {v
      let h1 = Hashtbl.of_alist_exn (module Int) [(1, 5); (2, 3232)] in
      let h2 = Hashtbl.of_alist_exn (module Int) [(1, 3)] in
      Hashtbl.merge h1 h2 ~f:(fun ~key:_ -> function
        | `Left x -> Some (`Left x)
        | `Right x -> Some (`Right x)
        | `Both (x, y) -> if x=y then None else Some (`Both (x,y))
      ) |> Hashtbl.to_alist;;
      - : (int * [> `Both of int * int | `Left of int | `Right of int ]) list =
      [(2, `Left 3232); (1, `Both (5, 3))]
      v}

      {1 Interface} *)

  include S_without_submodules (** @inline *)

  module type Accessors = Accessors
  module type Creators = Creators
  module type Key = Key.S [@@deprecated "[since 2019-03] Use [Hashtbl.Key.S]"]
  module type Multi = Multi
  module type S_poly = S_poly
  module type S_without_submodules = S_without_submodules
  module type For_deriving = For_deriving

  module Key = Key
  module Merge_into_action = Merge_into_action

  type nonrec ('key, 'data, 'z) create_options = ('key, 'data, 'z) create_options

  module Creators (Key : sig
      type 'a t

      val hashable : 'a t Hashable.t
    end) : sig
    type ('a, 'b) t_ = ('a Key.t, 'b) t

    val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_

    include
      Creators_generic
      with type ('a, 'b) t := ('a, 'b) t_
      with type 'a key := 'a Key.t
      with type ('key, 'data, 'a) create_options :=
        ('key, 'data, 'a) create_options_without_first_class_module
  end

  module Poly : S_poly with type ('a, 'b) t = ('a, 'b) t

  (** [M] is meant to be used in combination with OCaml applicative functor types:

      {[
        type string_to_int_table = int Hashtbl.M(String).t
      ]}

      which stands for:

      {[
        type string_to_int_table = (String.t, int) Hashtbl.t
      ]}

      The point is that [int Hashtbl.M(String).t] supports deriving, whereas the second
      syntax doesn't (because [t_of_sexp] doesn't know what comparison/hash function to
      use). *)
  module M (K : T.T) : sig
    type nonrec 'v t = (K.t, 'v) t
  end

  include For_deriving with type ('a, 'b) t := ('a, 'b) t

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module type Creators_generic = Creators_generic

    type nonrec ('key, 'data, 'z) create_options_without_first_class_module =
      ('key, 'data, 'z) create_options_without_first_class_module

    val hashable : ('key, _) t -> 'key Hashable.t
  end
end
