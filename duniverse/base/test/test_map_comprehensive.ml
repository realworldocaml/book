(** Comprehensive testing of [Base.Map].

    This file tests all exports of [Base.Map]. Every time a new export is added, we have
    to add a new definition somewhere here. Every time we add a definition, we should add
    a test unless the definition is untestable (e.g., a module type) or trivial (e.g., a
    module containing only ppx-derived definitions). We should document categories of
    untested definitions, mark them as untested, and keep them separate from definitions
    that need tests. *)

open! Import

open struct
  (** quickcheck configuration *)

  let quickcheck_config =
    let test_count =
      (* In js_of_ocaml, quickcheck is slow due to 64-bit arithmetic, and some map
         operations are especially slow due to use of exceptions and exception handlers.
         So on "other" backends, we turn the test count down. *)
      match Sys.backend_type with
      | Native | Bytecode -> 10_000
      | Other _ -> 1_000
    in
    { Base_quickcheck.Test.default_config with test_count }
  ;;

  let quickcheck_m here m ~f = quickcheck_m here m ~f ~config:quickcheck_config
end

(** The types that distinguish instances of [Map.Creators_and_accessors_generic]. *)
module type Types = sig
  type 'k key
  type 'c cmp
  type ('k, 'v, 'c) t
  type ('k, 'v, 'c) tree
  type ('k, 'c, 'a) create_options
  type ('k, 'c, 'a) access_options
end

(** Like [Map.Creators_and_accessors_generic], but based on [Types] for easier
    instantiation. *)
module type S = sig
  module Types : Types

  include
    Map.Creators_generic
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Types.t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Types.tree
    with type 'a key := 'a Types.key
    with type 'a cmp := 'a Types.cmp
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) Types.create_options

  include
    Map.Accessors_generic
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Types.t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Types.tree
    with type 'a key := 'a Types.key
    with type 'a cmp := 'a Types.cmp
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) Types.access_options
end

(** Helpers for testing a tree or map type that is an instance of [S]. *)
module type Instance = sig
  module Types : Types

  module Key : sig
    type t = int Types.key [@@deriving compare, equal, quickcheck, sexp_of]

    include Comparable.Infix with type t := t
  end

  type 'a t = (int, 'a, Int.comparator_witness) Types.t
  [@@deriving equal, quickcheck, sexp_of]

  (** Construct a [Key.t]. *)
  val key : int -> Key.t

  (** Extract an int from a [Key.t]. *)
  val int : Key.t -> int

  (** Extract a tree (without a comparator) from [t]. *)
  val tree
    :  (Key.t, 'a, Int.comparator_witness) Types.tree
    -> (Key.t, 'a, Int.comparator_witness Types.cmp) Map.Using_comparator.Tree.t

  (** Pass a comparator to a creator function, if necessary. *)
  val create : (int, Int.comparator_witness, 'a) Types.create_options -> 'a

  (** Pass a comparator to an accessor function, if necessary *)
  val access : (int, Int.comparator_witness, 'a) Types.access_options -> 'a
end

(** A functor to generate all of [Instance] but [create] and [access] for a map type. *)
module Instance (Cmp : sig
    type comparator_witness

    val comparator : (int, comparator_witness) Comparator.t
  end) =
struct
  module Key = struct
    type t = int [@@deriving quickcheck, sexp_of]
    type comparator_witness = Cmp.comparator_witness

    let comparator = Cmp.comparator
    let compare = comparator.compare
    let equal = [%compare.equal: t]
    let quickcheck_generator = Base_quickcheck.Generator.small_strictly_positive_int

    include Comparable.Infix (struct
        type nonrec t = t

        let compare = compare
      end)
  end

  type 'a t = 'a Map.M(Key).t [@@deriving equal, sexp_of]

  let key x = x
  let int x = x
  let tree x = x

  let quickcheck_generator gen =
    Base_quickcheck.Generator.map_t_m
      (module Key)
      Base_quickcheck.Generator.small_strictly_positive_int
      gen
  ;;

  let quickcheck_observer obs =
    Base_quickcheck.Observer.map_t Base_quickcheck.Observer.int obs
  ;;

  let quickcheck_shrinker shr =
    Base_quickcheck.Shrinker.map_t Base_quickcheck.Shrinker.int shr
  ;;
end

(** Instantiating key and data both as [int]. *)
module Instance_int = struct
  module I = Instance (Int)

  type t = int I.t [@@deriving equal, quickcheck, sexp_of]
end

(** A functor like [Instance], but for tree types. *)
module Instance_tree (Cmp : sig
    type comparator_witness

    val comparator : (int, comparator_witness) Comparator.t
  end) =
struct
  module M = Instance (Cmp)
  include M

  type 'a t = (int, 'a, Cmp.comparator_witness) Map.Using_comparator.Tree.t

  let of_tree tree = Map.Using_comparator.of_tree ~comparator:Cmp.comparator tree
  let to_tree t = Map.Using_comparator.to_tree t

  let quickcheck_generator gen =
    Base_quickcheck.Generator.map (M.quickcheck_generator gen) ~f:to_tree
  ;;

  let quickcheck_observer obs =
    Base_quickcheck.Observer.unmap (M.quickcheck_observer obs) ~f:of_tree
  ;;

  let quickcheck_shrinker shr =
    Base_quickcheck.Shrinker.map (M.quickcheck_shrinker shr) ~f:to_tree ~f_inverse:of_tree
  ;;

  let equal equal_a = Map.Using_comparator.Tree.equal ~comparator:Int.comparator equal_a
  let sexp_of_t sexp_of_a t = M.sexp_of_t sexp_of_a (of_tree t)
end

(** Functor for [List.t] *)
module Lst (T : sig
    type t [@@deriving equal, sexp_of]
  end) =
struct
  type t = T.t list [@@deriving equal, sexp_of]
end

(** Functor for [Or_error], ignoring error contents when comparing. *)
module Ok (T : sig
    type t [@@deriving equal, sexp_of]
  end) =
struct
  type t = (T.t, (Error.t[@equal.ignore])) Result.t [@@deriving equal, sexp_of]
end

(** Functor for [Option.t] *)
module Opt (T : sig
    type t [@@deriving equal, sexp_of]
  end) =
struct
  type t = T.t option [@@deriving equal, sexp_of]
end

(** Functor for pairs of a single type. Random generation frequently generates pairs of
    identical values. *)
module Pair (T : sig
    type t [@@deriving equal, quickcheck, sexp_of]
  end) =
struct
  type t = T.t * T.t [@@deriving equal, quickcheck, sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator.Let_syntax in
    match%bind Base_quickcheck.Generator.bool with
    | true -> [%generator: t]
    | false ->
      let%map x = [%generator: T.t] in
      x, x
  ;;
end

(** Expect tests for everything exported from [Map.Creators_and_accessors_generic]. *)
module Test_creators_and_accessors
    (Types : Types)
    (Impl : S with module Types := Types)
    (Instance : Instance with module Types := Types) : S with module Types := Types =
struct
  open Instance
  open Impl

  open struct
    (** Test helpers, not to be exported. *)

    module Alist = struct
      type t = (Key.t * int) list [@@deriving compare, equal, quickcheck, sexp_of]
    end

    module Alist_merge = struct
      type t = (Key.t * (int, int) Map.Merge_element.t) list [@@deriving equal, sexp_of]
    end

    module Alist_multi = struct
      type t = (Key.t * int list) list [@@deriving equal, quickcheck, sexp_of]
    end

    module Diff = struct
      type t = (Key.t, int) Map.Symmetric_diff_element.t list [@@deriving equal, sexp_of]
    end

    module Inst = struct
      type t = int Instance.t [@@deriving equal, quickcheck, sexp_of]
    end

    module Inst_and_key = struct
      type t = Inst.t * Key.t [@@deriving quickcheck, sexp_of]
    end

    module Inst_and_key_and_data = struct
      type t = Inst.t * Key.t * int [@@deriving quickcheck, sexp_of]
    end

    module Inst_multi = struct
      type t = int list Instance.t [@@deriving equal, quickcheck, sexp_of]
    end

    module Key_and_data = struct
      type t = Key.t * int [@@deriving equal, sexp_of]
    end

    module Maybe_bound = struct
      include Maybe_bound

      type 'a t = 'a Maybe_bound.t =
        | Incl of 'a
        | Excl of 'a
        | Unbounded
      [@@deriving quickcheck, sexp_of]
    end
  end

  (** creators *)

  let empty = empty

  let%expect_test _ =
    print_s [%sexp (create empty : int t)];
    [%expect {| () |}]
  ;;

  let singleton = singleton

  let%expect_test _ =
    print_s [%sexp (create singleton (key 1) 2 : int t)];
    [%expect {| ((1 2)) |}]
  ;;

  let of_alist = of_alist
  let of_alist_or_error = of_alist_or_error
  let of_alist_exn = of_alist_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let t_or_error = create of_alist_or_error alist in
        let t_exn = Or_error.try_with (fun () -> create of_alist_exn alist) in
        let t_or_duplicate =
          match create of_alist alist with
          | `Ok t -> Ok t
          | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
        in
        require_equal
          [%here]
          (module Ok (Alist))
          (Or_error.map t_or_error ~f:to_alist)
          (let compare = Comparable.lift Key.compare ~f:fst in
           if List.contains_dup alist ~compare
           then Or_error.error_string "duplicate"
           else Ok (List.sort alist ~compare));
        require_equal [%here] (module Ok (Inst)) t_exn t_or_error;
        require_equal [%here] (module Ok (Inst)) t_or_duplicate t_or_error);
    [%expect {| |}]
  ;;

  let of_alist_multi = of_alist_multi
  let of_alist_fold = of_alist_fold
  let of_alist_reduce = of_alist_reduce

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let t_multi = create of_alist_multi alist in
        let t_fold =
          create of_alist_fold alist ~init:[] ~f:(fun xs x -> x :: xs) |> map ~f:List.rev
        in
        let t_reduce =
          create of_alist_reduce (List.Assoc.map alist ~f:List.return) ~f:( @ )
        in
        require_equal
          [%here]
          (module Alist_multi)
          (to_alist t_multi)
          (List.Assoc.sort_and_group alist ~compare:Key.compare);
        require_equal [%here] (module Inst_multi) t_fold t_multi;
        require_equal [%here] (module Inst_multi) t_reduce t_multi);
    [%expect {| |}]
  ;;

  let of_sequence = of_sequence
  let of_sequence_or_error = of_sequence_or_error
  let of_sequence_exn = of_sequence_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let seq = Sequence.of_list alist in
        let t_or_error = create of_sequence_or_error seq in
        let t_exn = Or_error.try_with (fun () -> create of_sequence_exn seq) in
        let t_or_duplicate =
          match create of_sequence seq with
          | `Ok t -> Ok t
          | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
        in
        let expect = create of_alist_or_error alist in
        require_equal [%here] (module Ok (Inst)) t_or_error expect;
        require_equal [%here] (module Ok (Inst)) t_exn expect;
        require_equal [%here] (module Ok (Inst)) t_or_duplicate expect);
    [%expect {| |}]
  ;;

  let of_sequence_multi = of_sequence_multi
  let of_sequence_fold = of_sequence_fold
  let of_sequence_reduce = of_sequence_reduce

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let seq = Sequence.of_list alist in
        let t_multi = create of_sequence_multi seq in
        let t_fold =
          create of_sequence_fold seq ~init:[] ~f:(fun xs x -> x :: xs) |> map ~f:List.rev
        in
        let t_reduce =
          create
            of_sequence_reduce
            (alist |> List.Assoc.map ~f:List.return |> Sequence.of_list)
            ~f:( @ )
        in
        let expect = create of_alist_multi alist in
        require_equal [%here] (module Inst_multi) t_multi expect;
        require_equal [%here] (module Inst_multi) t_fold expect;
        require_equal [%here] (module Inst_multi) t_reduce expect);
    [%expect {| |}]
  ;;

  let of_increasing_sequence = of_increasing_sequence

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let seq = Sequence.of_list alist in
        let actual = create of_increasing_sequence seq in
        let expect =
          if List.is_sorted alist ~compare:(Comparable.lift Key.compare ~f:fst)
          then create of_alist_or_error alist
          else Or_error.error_string "decreasing keys"
        in
        require_equal [%here] (module Ok (Inst)) actual expect);
    [%expect {| |}]
  ;;

  let of_sorted_array = of_sorted_array

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let actual = create of_sorted_array (Array.of_list alist) in
        let expect =
          let compare = Comparable.lift Key.compare ~f:fst in
          if List.is_sorted_strictly ~compare alist
          || List.is_sorted_strictly ~compare (List.rev alist)
          then create of_alist_or_error alist
          else Or_error.error_string "unsorted"
        in
        require_equal [%here] (module Ok (Inst)) actual expect);
    [%expect {| |}]
  ;;

  let of_sorted_array_unchecked = of_sorted_array_unchecked

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let alist =
          List.dedup_and_sort alist ~compare:(Comparable.lift Key.compare ~f:fst)
        in
        let actual_fwd = create of_sorted_array_unchecked (Array.of_list alist) in
        let actual_rev = create of_sorted_array_unchecked (Array.of_list_rev alist) in
        let expect = create of_alist_exn alist in
        require_equal [%here] (module Inst) actual_fwd expect;
        require_equal [%here] (module Inst) actual_rev expect);
    [%expect {| |}]
  ;;

  let of_increasing_iterator_unchecked = of_increasing_iterator_unchecked

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let alist =
          List.dedup_and_sort alist ~compare:(Comparable.lift Key.compare ~f:fst)
        in
        let actual =
          let array = Array.of_list alist in
          create
            of_increasing_iterator_unchecked
            ~len:(Array.length array)
            ~f:(Array.get array)
        in
        let expect = create of_alist_exn alist in
        require_equal [%here] (module Inst) actual expect);
    [%expect {| |}]
  ;;

  let of_iteri = of_iteri
  let of_iteri_exn = of_iteri_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Alist)
      ~f:(fun alist ->
        let iteri ~f = List.iter alist ~f:(fun (key, data) -> f ~key ~data) in
        let actual_or_duplicate =
          match create of_iteri ~iteri with
          | `Ok t -> Ok t
          | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
        in
        let actual_exn = Or_error.try_with (fun () -> create of_iteri_exn ~iteri) in
        let expect = create of_alist_or_error alist in
        require_equal [%here] (module Ok (Inst)) actual_or_duplicate expect;
        require_equal [%here] (module Ok (Inst)) actual_exn expect);
    [%expect {| |}]
  ;;

  let map_keys = map_keys
  let map_keys_exn = map_keys_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, k) ->
        let f key = Comparable.min Key.compare k key in
        let actual_or_duplicate =
          match create map_keys t ~f with
          | `Ok t -> Ok t
          | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
        in
        let actual_exn = Or_error.try_with (fun () -> create map_keys_exn t ~f) in
        let expect =
          to_alist t
          |> List.map ~f:(fun (key, data) -> f key, data)
          |> create of_alist_or_error
        in
        require_equal [%here] (module Ok (Inst)) actual_or_duplicate expect;
        require_equal [%here] (module Ok (Inst)) actual_exn expect);
    [%expect {| |}]
  ;;

  (** accessors *)

  let invariants = invariants

  let%expect_test _ =
    quickcheck_m [%here] (module Inst) ~f:(fun t -> require [%here] (access invariants t));
    [%expect {| |}]
  ;;

  let is_empty = is_empty
  let length = length

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        let len = length t in
        require_equal [%here] (module Bool) (is_empty t) (len = 0);
        require_equal [%here] (module Int) len (List.length (to_alist t)));
    [%expect {| |}]
  ;;

  let mem = mem
  let find = find
  let find_exn = find_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, key) ->
        let expect = List.Assoc.find (to_alist t) key ~equal:Key.equal in
        require_equal [%here] (module Bool) (access mem t key) (Option.is_some expect);
        require_equal [%here] (module Opt (Int)) (access find t key) expect;
        require_equal
          [%here]
          (module Opt (Int))
          (Option.try_with (fun () -> access find_exn t key))
          expect);
    [%expect {| |}]
  ;;

  let set = set

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, key, data) ->
        require_equal
          [%here]
          (module Alist)
          (to_alist (access set t ~key ~data))
          (List.sort
             ~compare:(Comparable.lift Key.compare ~f:fst)
             ((key, data) :: List.Assoc.remove (to_alist t) key ~equal:Key.equal)));
    [%expect {| |}]
  ;;

  let add = add
  let add_exn = add_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, key, data) ->
        let t_add =
          match access add t ~key ~data with
          | `Ok t -> Ok t
          | `Duplicate -> Or_error.error_string "duplicate"
        in
        let t_add_exn = Or_error.try_with (fun () -> access add_exn t ~key ~data) in
        let expect =
          if access mem t key
          then Or_error.error_string "duplicate"
          else Ok (access set t ~key ~data)
        in
        require_equal [%here] (module Ok (Inst)) t_add expect;
        require_equal [%here] (module Ok (Inst)) t_add_exn expect);
    [%expect {| |}]
  ;;

  let remove = remove

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, key) ->
        require_equal
          [%here]
          (module Alist)
          (to_alist (access remove t key))
          (List.Assoc.remove (to_alist t) key ~equal:Key.equal));
    [%expect {| |}]
  ;;

  let change = change

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        type t = Inst.t * Key.t * int option [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, key, maybe_data) ->
        let actual =
          access change t key ~f:(fun previous ->
            require_equal [%here] (module Opt (Int)) previous (access find t key);
            [%expect {| |}];
            maybe_data)
        in
        let expect =
          match maybe_data with
          | None -> access remove t key
          | Some data -> access set t ~key ~data
        in
        require_equal [%here] (module Inst) actual expect);
    [%expect {| |}]
  ;;

  let update = update

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, key, data) ->
        let actual =
          access update t key ~f:(fun previous ->
            require_equal [%here] (module Opt (Int)) previous (access find t key);
            [%expect {| |}];
            data)
        in
        let expect = access set t ~key ~data in
        require_equal [%here] (module Inst) actual expect);
    [%expect {| |}]
  ;;

  let find_multi = find_multi
  let add_multi = add_multi
  let remove_multi = remove_multi

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        type t = Inst_multi.t * Key.t * int [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, key, data) ->
        require_equal
          [%here]
          (module Lst (Int))
          (access find_multi t key)
          (access find t key |> Option.value ~default:[]);
        require_equal
          [%here]
          (module Inst_multi)
          (access add_multi t ~key ~data)
          (access update t key ~f:(fun option -> data :: Option.value option ~default:[]));
        require_equal
          [%here]
          (module Inst_multi)
          (access remove_multi t key)
          (access change t key ~f:(function
             | None | Some ([] | [ _ ]) -> None
             | Some (_ :: (_ :: _ as rest)) -> Some rest)));
    [%expect {| |}]
  ;;

  let iter_keys = iter_keys
  let iter = iter
  let iteri = iteri

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        let actuali =
          let q = Queue.create () in
          iteri t ~f:(fun ~key ~data -> Queue.enqueue q (key, data));
          Queue.to_list q
        in
        let actual_keys =
          let q = Queue.create () in
          iter_keys t ~f:(Queue.enqueue q);
          Queue.to_list q
        in
        let actual =
          let q = Queue.create () in
          iter t ~f:(Queue.enqueue q);
          Queue.to_list q
        in
        require_equal [%here] (module Alist) actuali (to_alist t);
        require_equal [%here] (module Lst (Key)) actual_keys (keys t);
        require_equal [%here] (module Lst (Int)) actual (data t));
    [%expect {| |}]
  ;;

  let map = map
  let mapi = mapi

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        require_equal
          [%here]
          (module Inst)
          (map t ~f:Int.succ)
          (t |> to_alist |> List.Assoc.map ~f:Int.succ |> create of_alist_exn);
        require_equal
          [%here]
          (module struct
            type t = (Key.t * int) Instance.t [@@deriving equal, sexp_of]
          end)
          (mapi t ~f:(fun ~key ~data -> key, data))
          (t |> to_alist |> List.map ~f:(fun (k, v) -> k, (k, v)) |> create of_alist_exn));
    [%expect {| |}]
  ;;

  let filter_keys = filter_keys
  let filter = filter
  let filteri = filteri

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, k, d) ->
        require_equal
          [%here]
          (module Alist)
          (to_alist (access filter_keys t ~f:(fun key -> Key.( <= ) key k)))
          (List.filter (to_alist t) ~f:(fun (key, _) -> Key.( <= ) key k));
        require_equal
          [%here]
          (module Alist)
          (to_alist (access filter t ~f:(fun data -> data <= d)))
          (List.filter (to_alist t) ~f:(fun (_, data) -> data <= d));
        require_equal
          [%here]
          (module Alist)
          (to_alist
             (access filteri t ~f:(fun ~key ~data -> Key.( <= ) key k && data <= d)))
          (List.filter (to_alist t) ~f:(fun (key, data) -> Key.( <= ) key k && data <= d)));
    [%expect {| |}]
  ;;

  let filter_map = filter_map
  let filter_mapi = filter_mapi

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, k, d) ->
        require_equal
          [%here]
          (module Alist)
          (to_alist
             (access filter_map t ~f:(fun data -> Option.some_if (data >= d) (data - d))))
          (List.filter_map (to_alist t) ~f:(fun (key, data) ->
             Option.some_if (data >= d) (key, data - d)));
        require_equal
          [%here]
          (module Alist)
          (to_alist
             (access filter_mapi t ~f:(fun ~key ~data ->
                Option.some_if (Key.( <= ) key k && data >= d) (data - d))))
          (List.filter_map (to_alist t) ~f:(fun (key, data) ->
             Option.some_if (Key.( <= ) key k && data >= d) (key, data - d))));
    [%expect {| |}]
  ;;

  let partition_mapi = partition_mapi
  let partition_map = partition_map
  let partitioni_tf = partitioni_tf
  let partition_tf = partition_tf

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, k, d) ->
        require_equal
          [%here]
          (module Pair (Alist))
          (let a, b = access partition_tf t ~f:(fun data -> data <= d) in
           to_alist a, to_alist b)
          (List.partition_tf (to_alist t) ~f:(fun (_, data) -> data <= d));
        require_equal
          [%here]
          (module Pair (Alist))
          (let a, b =
             access partitioni_tf t ~f:(fun ~key ~data -> Key.( <= ) key k && data <= d)
           in
           to_alist a, to_alist b)
          (List.partition_tf (to_alist t) ~f:(fun (key, data) ->
             Key.( <= ) key k && data <= d));
        require_equal
          [%here]
          (module Pair (Alist))
          (let a, b =
             access partition_map t ~f:(fun data ->
               if data >= d then First (data - d) else Second d)
           in
           to_alist a, to_alist b)
          (List.partition_map (to_alist t) ~f:(fun (key, data) ->
             if data >= d then First (key, data - d) else Second (key, d)));
        require_equal
          [%here]
          (module Pair (Alist))
          (let a, b =
             access partition_mapi t ~f:(fun ~key ~data ->
               if Key.( <= ) key k && data >= d then First (data - d) else Second d)
           in
           to_alist a, to_alist b)
          (List.partition_map (to_alist t) ~f:(fun (key, data) ->
             if Key.( <= ) key k && data >= d
             then First (key, data - d)
             else Second (key, d))));
    [%expect {| |}]
  ;;

  let fold = fold
  let fold_right = fold_right

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        require_equal
          [%here]
          (module Alist)
          (fold t ~init:[] ~f:(fun ~key ~data list -> (key, data) :: list))
          (List.rev (to_alist t));
        require_equal
          [%here]
          (module Alist)
          (fold_right t ~init:[] ~f:(fun ~key ~data list -> (key, data) :: list))
          (to_alist t));
    [%expect {| |}]
  ;;

  let fold_until = fold_until
  let iteri_until = iteri_until

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, threshold) ->
        require_equal
          [%here]
          (module struct
            type t = int list * Base.Map.Finished_or_unfinished.t
            [@@deriving equal, sexp_of]
          end)
          (let q = Queue.create () in
           let status =
             iteri_until t ~f:(fun ~key ~data ->
               if Key.( >= ) key threshold
               then Stop
               else (
                 Queue.enqueue q data;
                 Continue))
           in
           Queue.to_list q, status)
          (let list =
             to_alist t
             |> List.take_while ~f:(fun (key, _) -> Key.( < ) key threshold)
             |> List.map ~f:snd
           in
           list, if List.length list = length t then Finished else Unfinished));
    [%expect {| |}]
  ;;

  let combine_errors = combine_errors

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, threshold) ->
        let t =
          mapi t ~f:(fun ~key ~data ->
            if Key.( <= ) key threshold
            then Ok data
            else Or_error.error_string "too big")
        in
        require_equal
          [%here]
          (module Ok (Inst))
          (access combine_errors t)
          (to_alist t
           |> List.map ~f:(fun (key, result) ->
             Or_error.map result ~f:(fun data -> key, data))
           |> Or_error.combine_errors
           |> Or_error.map ~f:(create of_alist_exn)));
    [%expect {| |}]
  ;;

  let equal = equal
  let compare_direct = compare_direct

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Pair (Inst))
      ~f:(fun (a, b) ->
        require_equal
          [%here]
          (module Ordering)
          (Ordering.of_int (access compare_direct Int.compare a b))
          (Ordering.of_int (Alist.compare (to_alist a) (to_alist b)));
        require_equal
          [%here]
          (module Bool)
          (access compare_direct Int.compare a b = 0)
          (access equal Int.equal a b));
    [%expect {| |}]
  ;;

  let keys = keys
  let data = data
  let to_alist = to_alist
  let to_sequence = to_sequence

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        let alist = to_alist t in
        require_equal [%here] (module Inst) (create of_alist_exn alist) t;
        require_equal [%here] (module Lst (Key)) (keys t) (List.map alist ~f:fst);
        require_equal [%here] (module Lst (Int)) (data t) (List.map alist ~f:snd);
        require_equal
          [%here]
          (module Alist)
          (Sequence.to_list ((access to_sequence) t))
          alist);
    [%expect {| |}]
  ;;

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        type t = Inst.t * [ `Decreasing | `Increasing ] [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, key_order) ->
        let alist = to_alist t ~key_order in
        require_equal
          [%here]
          (module Lst (Key_and_data))
          alist
          (match key_order with
           | `Increasing -> to_alist t
           | `Decreasing -> List.rev (to_alist t));
        require_equal
          [%here]
          (module Lst (Key_and_data))
          alist
          (Sequence.to_list
             ((access to_sequence)
                t
                ~order:
                  (match key_order with
                   | `Decreasing -> `Decreasing_key
                   | `Increasing -> `Increasing_key))));
    [%expect {| |}]
  ;;

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        type t = Inst.t * [ `Decreasing_key | `Increasing_key ] * Key.t * Key.t
        [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, order, keys_greater_or_equal_to, keys_less_or_equal_to) ->
        let alist =
          Sequence.to_list
            ((access to_sequence)
               t
               ~order
               ~keys_greater_or_equal_to
               ~keys_less_or_equal_to)
        in
        require_equal
          [%here]
          (module Lst (Key_and_data))
          alist
          (List.filter
             (match order with
              | `Decreasing_key -> List.rev (to_alist t)
              | `Increasing_key -> to_alist t)
             ~f:(fun (key, _) ->
               Key.( <= ) keys_greater_or_equal_to key
               && Key.( <= ) key keys_less_or_equal_to)));
    [%expect {| |}]
  ;;

  let merge = merge
  let iter2 = iter2
  let fold2 = fold2

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        module Inst2 = Pair (Inst)

        type t = Inst2.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun ((a, b), k) ->
        let merge_alist =
          access merge a b ~f:(fun ~key elt ->
            Option.some_if (Key.( > ) key k) (key, elt))
          |> data
        in
        let iter2_alist =
          let q = Queue.create () in
          access iter2 a b ~f:(fun ~key ~data:elt ->
            if Key.( > ) key k then Queue.enqueue q (key, elt));
          Queue.to_list q
        in
        let fold2_alist =
          access fold2 a b ~init:[] ~f:(fun ~key ~data:elt acc ->
            if Key.( > ) key k then (key, elt) :: acc else acc)
          |> List.rev
        in
        let expect =
          [ map a ~f:Either.first; map b ~f:Either.second ]
          |> List.concat_map ~f:to_alist
          |> List.Assoc.sort_and_group ~compare:Key.compare
          |> List.filter_map ~f:(fun (key, list) ->
            let elt =
              match (list : _ Either.t list) with
              | [ First x ] -> `Left x
              | [ Second y ] -> `Right y
              | [ First x; Second y ] -> `Both (x, y)
              | _ -> assert false
            in
            Option.some_if (Key.( > ) key k) (key, elt))
        in
        require_equal [%here] (module Alist_merge) merge_alist expect;
        require_equal [%here] (module Alist_merge) iter2_alist expect;
        require_equal [%here] (module Alist_merge) fold2_alist expect);
    [%expect {| |}]
  ;;

  let merge_skewed = merge_skewed

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Pair (Inst))
      ~f:(fun (a, b) ->
        let actual = access merge_skewed a b ~combine:(fun ~key a b -> int key + a + b) in
        let expect =
          access merge a b ~f:(fun ~key elt ->
            match elt with
            | `Left a -> Some a
            | `Right b -> Some b
            | `Both (a, b) -> Some (int key + a + b))
        in
        require_equal [%here] (module Inst) actual expect);
    [%expect {| |}]
  ;;

  let symmetric_diff = symmetric_diff
  let fold_symmetric_diff = fold_symmetric_diff

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Pair (Inst))
      ~f:(fun (a, b) ->
        let diff_alist =
          access symmetric_diff a b ~data_equal:Int.equal |> Sequence.to_list
        in
        let fold_alist =
          access
            fold_symmetric_diff
            a
            b
            ~data_equal:Int.equal
            ~init:[]
            ~f:(fun acc pair -> pair :: acc)
          |> List.rev
        in
        let expect =
          access merge a b ~f:(fun ~key:_ elt ->
            match elt with
            | `Left x -> Some (`Left x)
            | `Right y -> Some (`Right y)
            | `Both (x, y) -> if x = y then None else Some (`Unequal (x, y)))
          |> to_alist
        in
        require_equal [%here] (module Diff) diff_alist expect;
        require_equal [%here] (module Diff) fold_alist expect);
    [%expect {| |}]
  ;;

  let min_elt = min_elt
  let max_elt = max_elt
  let min_elt_exn = min_elt_exn
  let max_elt_exn = max_elt_exn

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (min_elt t)
          (List.hd (to_alist t));
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (max_elt t)
          (List.last (to_alist t));
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (Option.try_with (fun () -> min_elt_exn t))
          (List.hd (to_alist t));
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (Option.try_with (fun () -> max_elt_exn t))
          (List.last (to_alist t)));
    [%expect {| |}]
  ;;

  let for_all = for_all
  let for_alli = for_alli
  let exists = exists
  let existsi = existsi
  let count = count
  let counti = counti

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key_and_data)
      ~f:(fun (t, k, d) ->
        let f data = data <= d in
        let fi ~key ~data = Key.( <= ) key k && data <= d in
        let fp (key, data) = fi ~key ~data in
        let data = data t in
        let alist = to_alist t in
        require_equal [%here] (module Bool) (for_all t ~f) (List.for_all data ~f);
        require_equal [%here] (module Bool) (for_alli t ~f:fi) (List.for_all alist ~f:fp);
        require_equal [%here] (module Bool) (exists t ~f) (List.exists data ~f);
        require_equal [%here] (module Bool) (existsi t ~f:fi) (List.exists alist ~f:fp);
        require_equal [%here] (module Int) (count t ~f) (List.count data ~f);
        require_equal [%here] (module Int) (counti t ~f:fi) (List.count alist ~f:fp));
    [%expect {| |}]
  ;;

  let split = split

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, k) ->
        require_equal
          [%here]
          (module struct
            type t = Inst.t * (Key.t * int) option * Inst.t [@@deriving equal, sexp_of]
          end)
          (access split t k)
          (let before, equal, after =
             List.partition3_map (to_alist t) ~f:(fun (key, data) ->
               match Ordering.of_int (Key.compare key k) with
               | Less -> `Fst (key, data)
               | Equal -> `Snd (key, data)
               | Greater -> `Trd (key, data))
           in
           create of_alist_exn before, List.hd equal, create of_alist_exn after));
    [%expect {| |}]
  ;;

  let append = append

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Pair (Inst))
      ~f:(fun (a, b) ->
        require_equal
          [%here]
          (module Ok (Inst))
          (match access append ~lower_part:a ~upper_part:b with
           | `Ok t -> Ok t
           | `Overlapping_key_ranges -> Or_error.error_string "overlap")
          (match max_elt a, min_elt b with
           | Some (x, _), Some (y, _) when Key.( >= ) x y ->
             Or_error.error_string "overlap"
           | _ -> Ok (create of_alist_exn (to_alist a @ to_alist b)));
        let a' =
          (* we rely on the fact that the [Inst] generator uses positive keys *)
          create map_keys_exn a ~f:(fun k -> key (-int k))
        in
        require_equal
          [%here]
          (module Ok (Inst))
          (match access append ~lower_part:a' ~upper_part:b with
           | `Ok t -> Ok t
           | `Overlapping_key_ranges -> Or_error.error_string "overlap")
          (Ok (create of_alist_exn (to_alist a' @ to_alist b))));
    [%expect {| |}]
  ;;

  let subrange = subrange
  let fold_range_inclusive = fold_range_inclusive
  let range_to_alist = range_to_alist

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        type t = Inst.t * Key.t Maybe_bound.t * Key.t Maybe_bound.t
        [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, lower_bound, upper_bound) ->
        let subrange_alist = access subrange t ~lower_bound ~upper_bound |> to_alist in
        let min =
          match lower_bound with
          | Unbounded -> key Int.min_value
          | Incl min -> min
          | Excl too_small ->
            (* key generator does not generate [max_value], so this cannot overflow *)
            key (int too_small + 1)
        in
        let max =
          match upper_bound with
          | Unbounded -> key Int.max_value
          | Incl max -> max
          | Excl too_large ->
            (* key generator does not generate [min_value], so this cannot overflow *)
            key (int too_large - 1)
        in
        let fold_alist =
          access fold_range_inclusive t ~min ~max ~init:[] ~f:(fun ~key ~data acc ->
            (key, data) :: acc)
          |> List.rev
        in
        let range_alist = access range_to_alist t ~min ~max in
        let expect =
          if Maybe_bound.bounds_crossed
               ~lower:lower_bound
               ~upper:upper_bound
               ~compare:Key.compare
          then []
          else
            List.filter (to_alist t) ~f:(fun (key, _) ->
              Maybe_bound.interval_contains_exn
                key
                ~lower:lower_bound
                ~upper:upper_bound
                ~compare:Key.compare)
        in
        require_equal [%here] (module Alist) subrange_alist expect;
        require_equal [%here] (module Alist) fold_alist expect;
        require_equal [%here] (module Alist) range_alist expect);
    [%expect {| |}]
  ;;

  let closest_key = closest_key

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, k) ->
        let alist = to_alist t in
        let rev_alist = List.rev alist in
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (access closest_key t `Less_than k)
          (List.find rev_alist ~f:(fun (key, _) -> Key.( < ) key k));
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (access closest_key t `Less_or_equal_to k)
          (List.find rev_alist ~f:(fun (key, _) -> Key.( <= ) key k));
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (access closest_key t `Greater_or_equal_to k)
          (List.find alist ~f:(fun (key, _) -> Key.( >= ) key k));
        require_equal
          [%here]
          (module Opt (Key_and_data))
          (access closest_key t `Greater_than k)
          (List.find alist ~f:(fun (key, _) -> Key.( > ) key k)));
    [%expect {| |}]
  ;;

  let nth = nth
  let nth_exn = nth_exn
  let rank = rank

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, k) ->
        List.iteri (to_alist t) ~f:(fun i (key, data) ->
          require_equal [%here] (module Opt (Key_and_data)) (nth t i) (Some (key, data));
          require_equal
            [%here]
            (module Opt (Key_and_data))
            (Option.try_with (fun () -> nth_exn t i))
            (nth t i);
          require_equal [%here] (module Opt (Int)) (access rank t key) (Some i));
        require_equal [%here] (module Opt (Key_and_data)) (nth t (length t)) None;
        require_equal
          [%here]
          (module Opt (Int))
          (access rank t k)
          (List.find_mapi (to_alist t) ~f:(fun i (key, _) ->
             Option.some_if (Key.equal key k) i)));
    [%expect {| |}]
  ;;

  let binary_search = binary_search

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, k) ->
        let targets = [%all: Binary_searchable.Which_target_by_key.t] in
        let compare (key, _) k = Key.compare key k in
        List.iter targets ~f:(fun which_target ->
          require_equal
            [%here]
            (module Opt (Key_and_data))
            (access
               binary_search
               t
               ~compare:(fun ~key ~data k' ->
                 require_equal [%here] (module Key) k' k;
                 require_equal
                   [%here]
                   (module Opt (Int))
                   (access find t key)
                   (Some data);
                 [%expect {| |}];
                 compare (key, data) k')
               which_target
               k)
            (let array = Array.of_list (to_alist t) in
             Array.binary_search array ~compare which_target k
             |> Option.map ~f:(Array.get array))));
    [%expect {| |}]
  ;;

  let binary_search_segmented = binary_search_segmented

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst_and_key)
      ~f:(fun (t, k) ->
        let targets = [%all: Binary_searchable.Which_target_by_segment.t] in
        let segment_of (key, _) = if Key.( <= ) key k then `Left else `Right in
        List.iter targets ~f:(fun which_target ->
          require_equal
            [%here]
            (module Opt (Key_and_data))
            (access
               binary_search_segmented
               t
               ~segment_of:(fun ~key ~data ->
                 require_equal
                   [%here]
                   (module Opt (Int))
                   (access find t key)
                   (Some data);
                 [%expect {| |}];
                 segment_of (key, data))
               which_target)
            (let array = Array.of_list (to_alist t) in
             Array.binary_search_segmented array ~segment_of which_target
             |> Option.map ~f:(Array.get array))));
    [%expect {| |}]
  ;;

  let binary_search_subrange = binary_search_subrange

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module struct
        type t = Inst.t * Key.t Maybe_bound.t * Key.t Maybe_bound.t
        [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, lower_bound, upper_bound) ->
        require_equal
          [%here]
          (module Inst)
          (access
             binary_search_subrange
             t
             ~compare:(fun ~key ~data bound ->
               require_equal [%here] (module Opt (Int)) (access find t key) (Some data);
               [%expect {| |}];
               Key.compare key bound)
             ~lower_bound
             ~upper_bound)
          (access subrange t ~lower_bound ~upper_bound));
    [%expect {| |}]
  ;;

  (** tree conversion *)

  let to_tree = to_tree
  let of_tree = of_tree

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Inst)
      ~f:(fun t ->
        let tree = to_tree t in
        let round_trip = create of_tree tree in
        require_equal [%here] (module Inst) t round_trip;
        require_equal
          [%here]
          (module Alist)
          (to_alist t)
          (Map.Using_comparator.Tree.to_alist (Instance.tree tree)));
    [%expect {| |}]
  ;;
end

(** Expect tests for all of [Base.Map]'s exports. *)
module _ : module type of struct
  include Base.Map
end [@ocaml.remove_aliases] = struct
  open Base.Map

  (** module types *)

  module type Accessors1 = Accessors1
  module type Accessors2 = Accessors2
  module type Accessors3 = Accessors3
  module type Accessors3_with_comparator = Accessors3_with_comparator
  module type Accessors_generic = Accessors_generic
  module type Creators1 = Creators1
  module type Creators2 = Creators2
  module type Creators3_with_comparator = Creators3_with_comparator
  module type Creators_and_accessors1 = Creators_and_accessors1
  module type Creators_and_accessors2 = Creators_and_accessors2

  module type Creators_and_accessors3_with_comparator =
    Creators_and_accessors3_with_comparator

  module type Creators_and_accessors_generic = Creators_and_accessors_generic
  module type Creators_generic = Creators_generic
  module type For_deriving = For_deriving
  module type S_poly = S_poly

  (** type-only modules for module type instantiation - untested *)

  module With_comparator = With_comparator
  module With_first_class_module = With_first_class_module
  module Without_comparator = Without_comparator

  (** supporting datatypes - untested *)

  module Continue_or_stop = Continue_or_stop
  module Finished_or_unfinished = Finished_or_unfinished
  module Merge_element = Merge_element
  module Or_duplicate = Or_duplicate
  module Symmetric_diff_element = Symmetric_diff_element

  (** types *)

  type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t
  type nonrec ('k, 'c) comparator = ('k, 'c) Comparator.Module.t

  (** module types for ppx deriving *)

  module type Compare_m = Compare_m
  module type Equal_m = Equal_m
  module type Hash_fold_m = Hash_fold_m
  module type M_sexp_grammar = M_sexp_grammar
  module type M_of_sexp = M_of_sexp
  module type Sexp_of_m = Sexp_of_m

  (** functor for ppx deriving - tested below *)

  module M = M

  (** sexp conversions and grammar *)

  let sexp_of_m__t = sexp_of_m__t
  let m__t_of_sexp = m__t_of_sexp

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Instance_int)
      ~f:(fun t ->
        let sexp = [%sexp_of: int M(Int).t] t in
        require_equal [%here] (module Sexp) sexp [%sexp (to_alist t : (int * int) list)];
        let round_trip = [%of_sexp: int M(Int).t] sexp in
        require_equal [%here] (module Instance_int) round_trip t);
    [%expect {| |}]
  ;;

  let m__t_sexp_grammar = m__t_sexp_grammar

  let%expect_test _ =
    print_s [%sexp ([%sexp_grammar: int M(Int).t] : _ Sexp_grammar.t)];
    [%expect {| (List (Many (List (Cons Integer (Cons Integer Empty))))) |}]
  ;;

  (** comparisons *)

  let compare_m__t = compare_m__t
  let equal_m__t = equal_m__t

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Pair (Instance_int))
      ~f:(fun (a, b) ->
        require_equal
          [%here]
          (module Ordering)
          (Ordering.of_int ([%compare: int M(Int).t] a b))
          (Ordering.of_int ([%compare: (int * int) list] (to_alist a) (to_alist b)));
        require_equal
          [%here]
          (module Bool)
          ([%equal: int M(Int).t] a b)
          ([%equal: (int * int) list] (to_alist a) (to_alist b)));
    [%expect {| |}]
  ;;

  (** hash functions *)

  let hash_fold_m__t = hash_fold_m__t
  let hash_fold_direct = hash_fold_direct

  let%expect_test _ =
    quickcheck_m
      [%here]
      (module Instance_int)
      ~f:(fun t ->
        let actual_m = Hash.run [%hash_fold: int M(Int).t] t in
        let actual_direct =
          Hash.run (hash_fold_direct Int.hash_fold_t Int.hash_fold_t) t
        in
        let expect = Hash.run [%hash_fold: (int * int) list] (to_alist t) in
        require_equal [%here] (module Int) actual_m expect;
        require_equal [%here] (module Int) actual_direct expect);
    [%expect {| |}]
  ;;

  (** comparator accessors - untested *)

  let comparator_s = comparator_s
  let comparator = comparator

  (** creators and accessors *)

  include
    Test_creators_and_accessors
      (struct
        type 'k key = 'k
        type 'c cmp = 'c
        type ('k, 'v, 'c) tree = ('k, 'v, 'c) Using_comparator.Tree.t
        type ('k, 'c, 'a) create_options = ('k, 'c) comparator -> 'a
        type ('k, 'c, 'a) access_options = 'a

        include Base.Map
      end)
      (struct
        include Base.Map
      end)
      (struct
        include Instance (Int)

        let create f = f ((module Int) : _ Comparator.Module.t)
        let access x = x
      end)

  (** polymorphic comparison interface *)
  module Poly = struct
    open Poly

    type nonrec ('k, 'v) t = ('k, 'v) t
    type nonrec ('k, 'v) tree = ('k, 'v) tree
    type nonrec comparator_witness = comparator_witness

    include
      Test_creators_and_accessors
        (struct
          type 'k key = 'k
          type 'c cmp = comparator_witness
          type nonrec ('k, 'v, _) t = ('k, 'v) t
          type nonrec ('k, 'v, _) tree = ('k, 'v) tree
          type ('k, 'c, 'a) create_options = 'a
          type ('k, 'c, 'a) access_options = 'a
        end)
        (struct
          include Poly
        end)
        (struct
          include Instance (Comparator.Poly)

          let create x = x
          let access x = x
        end)
  end

  (** comparator interface *)

  module Using_comparator = struct
    open Using_comparator

    (** type *)

    type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

    (** comparator accessor - untested *)

    let comparator = comparator

    (** sexp conversions *)

    let sexp_of_t = sexp_of_t
    let t_of_sexp_direct = t_of_sexp_direct

    let%expect_test _ =
      quickcheck_m
        [%here]
        (module Instance_int)
        ~f:(fun t ->
          let sexp = sexp_of_t Int.sexp_of_t Int.sexp_of_t [%sexp_of: _] t in
          require_equal [%here] (module Sexp) sexp ([%sexp_of: int Map.M(Int).t] t);
          let round_trip =
            t_of_sexp_direct ~comparator:Int.comparator Int.t_of_sexp Int.t_of_sexp sexp
          in
          require_equal [%here] (module Instance_int) round_trip t);
      [%expect {| |}]
    ;;

    (** hash function *)

    let hash_fold_direct = hash_fold_direct

    let%expect_test _ =
      quickcheck_m
        [%here]
        (module Instance_int)
        ~f:(fun t ->
          require_equal
            [%here]
            (module Int)
            (Hash.run (hash_fold_direct Int.hash_fold_t Int.hash_fold_t) t)
            (Hash.run [%hash_fold: int Map.M(Int).t] t));
      [%expect {| |}]
    ;;

    (** functor for polymorphic definition - untested *)

    module Empty_without_value_restriction (Cmp : Comparator.S1) = struct
      open Empty_without_value_restriction (Cmp)

      let empty = empty
    end

    (** creators and accessors *)

    include
      Test_creators_and_accessors
        (struct
          type 'k key = 'k
          type 'c cmp = 'c
          type ('k, 'v, 'c) tree = ('k, 'v, 'c) Tree.t
          type ('k, 'c, 'a) create_options = comparator:('k, 'c) Comparator.t -> 'a
          type ('k, 'c, 'a) access_options = 'a

          include Using_comparator
        end)
        (struct
          include Using_comparator
        end)
        (struct
          include Instance (Int)

          let create f = f ~comparator:Int.comparator
          let access x = x
        end)

    (** tree interface *)

    module Tree = struct
      open Tree

      (** type *)

      type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

      (** sexp conversions *)

      let sexp_of_t = sexp_of_t
      let t_of_sexp_direct = t_of_sexp_direct

      let%expect_test _ =
        let module Tree_int = struct
          module I = Instance_tree (Int)

          type t = int I.t [@@deriving equal, quickcheck, sexp_of]
        end
        in
        quickcheck_m
          [%here]
          (module Tree_int)
          ~f:(fun tree ->
            let sexp = sexp_of_t Int.sexp_of_t Int.sexp_of_t [%sexp_of: _] tree in
            require_equal
              [%here]
              (module Sexp)
              sexp
              ([%sexp_of: int Map.M(Int).t]
                 (Using_comparator.of_tree tree ~comparator:Int.comparator));
            let round_trip =
              t_of_sexp_direct ~comparator:Int.comparator Int.t_of_sexp Int.t_of_sexp sexp
            in
            require_equal [%here] (module Tree_int) round_trip tree);
        [%expect {| |}]
      ;;

      (** polymorphic constructor - untested *)

      let empty_without_value_restriction = empty_without_value_restriction

      (** builders *)

      module Build_increasing = struct
        open Build_increasing

        type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

        (** tree builder functions *)

        let empty = empty
        let add_exn = add_exn
        let to_tree = to_tree

        let%expect_test _ =
          let module Tree_int = struct
            module I = Instance_tree (Int)

            type t = int I.t [@@deriving equal, quickcheck, sexp_of]
          end
          in
          quickcheck_m
            [%here]
            (module struct
              type t =
                ((int[@generator Base_quickcheck.Generator.small_strictly_positive_int])
                 * int)
                  list
              [@@deriving quickcheck, sexp_of]
            end)
            ~f:(fun alist ->
              let actual =
                List.fold_result alist ~init:empty ~f:(fun builder (key, data) ->
                  Or_error.try_with (fun () ->
                    add_exn builder ~comparator:Int.comparator ~key ~data))
                |> Or_error.map ~f:Build_increasing.to_tree
              in
              let expect =
                Map.Using_comparator.Tree.of_increasing_sequence
                  ~comparator:Int.comparator
                  (Sequence.of_list alist)
              in
              require_equal [%here] (module Ok (Tree_int)) actual expect);
          [%expect {| |}]
        ;;
      end

      (** creators and accessors *)

      include
        Test_creators_and_accessors
          (struct
            type 'k key = 'k
            type 'c cmp = 'c
            type ('k, 'v, 'c) tree = ('k, 'v, 'c) t
            type ('k, 'c, 'a) create_options = comparator:('k, 'c) Comparator.t -> 'a
            type ('k, 'c, 'a) access_options = comparator:('k, 'c) Comparator.t -> 'a

            include Tree
          end)
          (struct
            include Tree
          end)
          (struct
            include Instance_tree (Int)

            let create f = f ~comparator:Int.comparator
            let access f = f ~comparator:Int.comparator
          end)
    end
  end
end
