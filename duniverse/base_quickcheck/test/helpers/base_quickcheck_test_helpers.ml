open! Base
open Base_quickcheck
open Expect_test_helpers_core
include Base_quickcheck_test_helpers_intf

let () = sexp_style := Sexp_style.simple_pretty
let set_is_singleton set = Set.length set = 1

let arbitrary_int_gen =
  Generator.create (fun ~size:_ ~random ->
    Splittable_random.int random ~lo:Int.min_value ~hi:Int.max_value)
;;

module Int_list = struct
  module T = struct
    type t = int list [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module type Partition_value = sig
  type t

  include Comparator.S with type t := t
  include With_examples with type t := t
end

(* Used in testing observers, to see which values map to distinct "buckets" by the
   observer's hash function. *)
module Partitioning (Value : Partition_value) : sig
  type t [@@deriving compare, sexp_of]

  (* Create a partitioning of values based on a hash function. *)

  val create : (Value.t -> int) -> t

  (* Combine multiple partitionings to refine the buckets. For example, on different
     initial hash states or different sizes, two runs of an observer might differentiate
     different inputs. *)

  val union : t list -> t

  (* True if the partitioning maps each value to a separate bucket. *)

  val is_complete : t -> bool

  (* True if the partitioning maps every value to the same bucket. *)

  val is_singleton : t -> bool
end = struct
  module Partition = struct
    module T = struct
      type t = (Value.t, Value.comparator_witness) Set.t

      let compare a b = Set.compare_m__t (module Value) a b
      let sexp_of_t t = Set.sexp_of_m__t (module Value) t
    end

    include T
    include Comparator.Make (T)

    let create list = Set.of_list (module Value) list
  end

  type t =
    { hash_keys : (Value.t, int list, Value.comparator_witness) Map.t
    ; partitions : (Partition.t, Partition.comparator_witness) Set.t
    }

  let compare a b = Set.compare_m__t (module Partition) a.partitions b.partitions
  let sexp_of_t t = Set.sexp_of_m__t (module Partition) t.partitions

  let of_hash_keys hash_keys =
    let partitions =
      Map.to_alist hash_keys
      |> List.map ~f:(fun (value, list) -> list, value)
      |> Map.of_alist_multi (module Int_list)
      |> Map.data
      |> List.map ~f:Partition.create
      |> Set.of_list (module Partition)
    in
    { hash_keys; partitions }
  ;;

  let create' hash_list_fn =
    Map.of_alist_exn
      (module Value)
      (List.map Value.examples ~f:(fun value -> value, hash_list_fn value))
    |> of_hash_keys
  ;;

  let create hash_fn = create' (fun value -> [ hash_fn value ])

  let union list =
    create' (fun value ->
      List.concat_map list ~f:(fun t -> Map.find_exn t.hash_keys value))
  ;;

  let is_complete t = Set.for_all ~f:set_is_singleton t.partitions
  let is_singleton t = set_is_singleton t.partitions
end

let test_generator (type a) ?config ?(mode = `exhaustive) ?cr generator m =
  let (module Value : With_examples with type t = a) = m in
  let module Value = struct
    include Value
    include Comparator.Make (Value)
  end
  in
  Test.with_sample_exn generator ?config ~f:(fun sequence ->
    let generated_values = Sequence.to_list sequence in
    let distinct_generated_values = Set.of_list (module Value) generated_values in
    let distinct_known_values = Set.of_list (module Value) Value.examples in
    let failed_to_generate =
      Set.diff distinct_known_values distinct_generated_values
    in
    let message =
      if Set.equal distinct_generated_values distinct_known_values
      then [%message "exhaustive"]
      else (
        let description =
          Printf.sprintf
            !"generated %{Int#hum} distinct values in %{Int#hum} iterations"
            (Set.length distinct_generated_values)
            (List.length generated_values)
        in
        let error_message =
          if Set.is_empty failed_to_generate
          then None
          else
            Some
              [%message
                "did not generate these values"
                  ~_:(Set.to_list failed_to_generate : Value.t list)]
        in
        [%message description ~_:(error_message : (Sexp.t option[@sexp.option]))])
    in
    print_s [%message "generator" ~_:(message : Sexp.t)];
    match mode with
    | `exhaustive ->
      require
        [%here]
        ?cr
        (Set.is_empty failed_to_generate)
        ~if_false_then_print_s:(lazy [%message "failed to generate all known values"])
    | `inexhaustive ->
      require
        [%here]
        ?cr
        (not (Set.is_empty failed_to_generate))
        ~if_false_then_print_s:
          (lazy
            [%message "generated all known values even though we did not expect to"]))
;;

let test_observer (type a) ?config ?(mode = `transparent) ?cr observer m =
  let (module Value : With_examples with type t = a) = m in
  let number_of_examples = List.length Value.examples in
  (* Not passing [?cr] here, this is not a soft test on observer properties, this is a
     performance bound based on the current implementation that all callsites should
     observe. *)
  require
    [%here]
    (number_of_examples <= 20)
    ~if_false_then_print_s:
      (lazy
        [%message
          "too many values; observer test will take too long"
            (number_of_examples : int)
            ~examples:(Value.examples : Value.t list)]);
  let module Value = struct
    include Value
    include Comparator.Make (Value)
  end
  in
  let module Partitioning = Partitioning (Value) in
  let hash_fn_gen = Generator.fn observer arbitrary_int_gen in
  Test.with_sample_exn hash_fn_gen ?config ~f:(fun sample ->
    let partitionings =
      Sequence.to_list (Sequence.map sample ~f:Partitioning.create)
      |> List.dedup_and_sort ~compare:Partitioning.compare
    in
    let partitions = Partitioning.union partitionings in
    let message =
      if Partitioning.is_complete partitions
      then [%message "transparent"]
      else if Partitioning.is_singleton partitions
      then [%message "opaque"]
      else [%message (partitions : Partitioning.t)]
    in
    print_s [%message "observer" ~_:(message : Sexp.t)];
    match mode with
    | `transparent ->
      require
        [%here]
        ?cr
        (List.exists partitionings ~f:Partitioning.is_complete)
        ~if_false_then_print_s:
          (lazy
            [%message
              "did not generate any single function that distinguishes all values"])
    | `opaque ->
      require
        [%here]
        ?cr
        (Partitioning.is_singleton partitions)
        ~if_false_then_print_s:
          (lazy [%message "generated functions did not treat input as opaque"]))
;;

let test_shrinker (type a) ?config:_ ?(mode = `compound) ?cr shrinker m =
  let (module Value : With_examples with type t = a) = m in
  let alist =
    List.map Value.examples ~f:(fun value ->
      value, Sequence.to_list (Shrinker.shrink shrinker value))
    |> List.filter ~f:(fun (_, list) -> not (List.is_empty list))
  in
  let message =
    if List.is_empty alist
    then [%message "atomic"]
    else
      List.concat_map alist ~f:(fun (large, small_list) ->
        List.map small_list ~f:(fun small ->
          [%sexp (large : Value.t), "=>", (small : Value.t)]))
      |> [%sexp_of: Sexp.t list]
  in
  print_s [%message "shrinker" ~_:(message : Sexp.t)];
  match mode with
  | `atomic ->
    require
      [%here]
      ?cr
      (List.is_empty alist)
      ~if_false_then_print_s:(lazy [%message "atomic shrinker should not shrink values"])
  | `compound ->
    require
      [%here]
      ?cr
      (not (List.is_empty alist))
      ~if_false_then_print_s:(lazy [%message "compound shrinker should shrink values"])
;;

let percent ~count ~total =
  Core_kernel.Percent.of_mult (Float.of_int count /. Float.of_int total)
;;

let show_distribution (type a) ?config ?(show = 20) generator m =
  let (module Value : Value with type t = a) = m in
  let module Value = struct
    include Value
    include Comparator.Make (Value)
  end
  in
  Test.with_sample_exn ?config generator ~f:(fun sample ->
    let sample = Sequence.to_list sample in
    let total = List.length sample in
    let value_by_count =
      sample
      |> List.map ~f:(fun value -> value, value)
      |> Map.of_alist_multi (module Value)
      |> Map.map ~f:List.length
      |> Map.to_alist
      |> List.map ~f:(fun (value, count) -> count, value)
      |> List.sort ~compare:[%compare: int * Value.t]
      |> List.map ~f:(fun (count, value) -> percent ~count ~total, value)
      |> List.rev
      |> fun list -> List.take list show
    in
    print_s [%sexp (value_by_count : (Core_kernel.Percent.t * Value.t) list)])
;;

module type Exhaustive = sig
  type t [@@deriving enumerate]

  include Value with type t := t
end

let exhaustive (type a) (module Value : Exhaustive with type t = a) =
  (module struct
    include Value

    let examples = all
  end : With_examples
    with type t = a)
;;

let m_unit = exhaustive (module Unit)
let m_bool = exhaustive (module Bool)

let m_char =
  (module struct
    include Char

    (* min and max of each: uppercase, lowercase, digit, punctuation, whitespace, other *)
    let examples = String.to_list "AZaz09!~ \t\000\255"
  end : With_examples
    with type t = char)
;;

let m_biject (type a b) (module A : With_examples with type t = a) ~f ~f_inverse =
  (module struct
    type t = b

    let compare = Comparable.lift A.compare ~f:f_inverse
    let sexp_of_t t = A.sexp_of_t (f_inverse t)
    let examples = List.map A.examples ~f |> List.dedup_and_sort ~compare
  end : With_examples
    with type t = b)
;;

let m_option (type value) (module Value : With_examples with type t = value) =
  (module struct
    type t = Value.t option [@@deriving compare, sexp_of]

    let examples = [ None ] @ List.map Value.examples ~f:Option.return
  end : With_examples
    with type t = value option)
;;

let m_list (type elt) (module Elt : With_examples with type t = elt) =
  (module struct
    type t = Elt.t list [@@deriving sexp_of]

    let compare =
      Comparable.lift [%compare: int * Elt.t list] ~f:(fun t -> List.length t, t)
    ;;

    let examples =
      [ [] ]
      @ List.map Elt.examples ~f:(fun x -> [ x ])
      @ List.map2_exn Elt.examples (List.rev Elt.examples) ~f:(fun x y -> [ x; y ])
    ;;
  end : With_examples
    with type t = elt list)
;;

let m_arrow
      (type a b)
      (module A : With_examples with type t = a)
      (module B : With_examples with type t = b)
  =
  (module struct
    type t = A.t -> B.t

    let to_alist f = List.map A.examples ~f:(fun a -> a, f a)
    let compare = Comparable.lift [%compare: (A.t * B.t) list] ~f:to_alist
    let sexp_of_t t = [%sexp (to_alist t : (A.t * B.t) list)]

    let examples =
      if List.length A.examples <= 4 && List.length B.examples <= 4
      then
        List.fold_right A.examples ~init:[ [] ] ~f:(fun a alists ->
          List.concat_map B.examples ~f:(fun b ->
            let pair = a, b in
            List.map alists ~f:(fun alist -> pair :: alist)))
        |> List.map ~f:(fun alist a ->
          List.Assoc.find alist a ~equal:[%compare.equal: A.t]
          |> Option.value ~default:(List.hd_exn B.examples))
      else List.map B.examples ~f:Fn.const
    ;;
  end : With_examples
    with type t = a -> b)
;;

let m_arrow_named
      (type a b)
      (module A : With_examples with type t = a)
      (module B : With_examples with type t = b)
  : (module With_examples with type t = x:a -> b)
  =
  m_biject
    (m_arrow (module A) (module B))
    ~f:(fun f ~x -> f x)
    ~f_inverse:(fun f x -> f ~x)
;;

let m_arrow_optional
      (type a b)
      (module A : With_examples with type t = a)
      (module B : With_examples with type t = b)
  : (module With_examples with type t = ?x:a -> unit -> b)
  =
  m_biject
    (m_arrow (m_option (module A)) (module B))
    ~f:(fun f ?x () -> f x)
    ~f_inverse:(fun f x -> f ?x ())
;;

let m_either
      (type a b)
      (module A : With_examples with type t = a)
      (module B : With_examples with type t = b)
  =
  (module struct
    type t = (A.t, B.t) Either.t [@@deriving compare, sexp_of]

    let examples =
      List.map A.examples ~f:Either.first @ List.map B.examples ~f:Either.second
    ;;
  end : With_examples
    with type t = (a, b) Either.t)
;;

let m_result
      (type a b)
      (module A : With_examples with type t = a)
      (module B : With_examples with type t = b)
  =
  (module struct
    type t = (A.t, B.t) Result.t [@@deriving compare, sexp_of]

    let examples =
      List.map A.examples ~f:(fun x -> Ok x) @ List.map B.examples ~f:(fun x -> Error x)
    ;;
  end : With_examples
    with type t = (a, b) Result.t)
;;

let m_pair
      (type a b)
      (module A : With_examples with type t = a)
      (module B : With_examples with type t = b)
  =
  (module struct
    type t = A.t * B.t [@@deriving compare, sexp_of]

    let examples = List.cartesian_product A.examples B.examples
  end : With_examples
    with type t = a * b)
;;

let m_string =
  (module struct
    type t = string [@@deriving sexp_of]

    let compare =
      Comparable.lift [%compare: int * string] ~f:(fun string ->
        String.length string, string)
    ;;

    let examples =
      (* one of each: uppercase, lowercase, digit, punctuation, whitespace, other *)
      let chars = String.to_list "Az0_ \000" in
      [ "" ]
      @ List.map chars ~f:String.of_char
      @ List.map chars ~f:(fun char -> String.of_char char ^ String.of_char char)
    ;;
  end : With_examples
    with type t = string)
;;

let m_nat ~up_to =
  (module struct
    type t = int [@@deriving compare, sexp_of]

    let examples = List.range 0 up_to ~start:`inclusive ~stop:`inclusive
  end : With_examples
    with type t = int)
;;

let m_nat' (type i) ~up_to (module I : Int.S with type t = i) =
  (module struct
    type t = I.t [@@deriving compare, sexp_of]

    let examples =
      List.range 0 up_to ~start:`inclusive ~stop:`inclusive |> List.map ~f:I.of_int_exn
    ;;
  end : With_examples
    with type t = i)
;;

let m_int (type a) (module I : Int.S with type t = a) =
  (module struct
    type t = I.t [@@deriving compare, sexp_of]

    let examples = [ I.min_value; I.minus_one; I.zero; I.one; I.max_value ]
  end : With_examples
    with type t = a)
;;

let m_float =
  (module struct
    type t = float [@@deriving compare, sexp_of]

    let examples =
      [ Float.zero
      ; Float.min_positive_subnormal_value
      ; Float.min_positive_normal_value |> Float.one_ulp `Down
      ; Float.min_positive_normal_value
      ; Float.max_finite_value
      ; Float.infinity
      ; Float.nan
      ]
      |> List.concat_map ~f:(fun x -> [ Float.neg x; x ])
      |> List.dedup_and_sort ~compare:Float.compare
    ;;
  end : With_examples
    with type t = float)
;;

let m_sexp =
  (module struct
    type t = Sexp.t [@@deriving compare, sexp_of]

    let examples =
      let atoms = List.map ~f:(fun string -> Sexp.Atom string) [ "a"; "bc"; "def" ] in
      let lists_of_atoms =
        List.map atoms ~f:(fun atom -> Sexp.List [ atom ]) @ [ Sexp.List atoms ]
      in
      atoms @ lists_of_atoms @ [ Sexp.List (atoms @ lists_of_atoms) ]
    ;;
  end : With_examples
    with type t = Sexp.t)
;;

let m_set
      (type elt cmp)
      (module Cmp : Comparator.S with type t = elt and type comparator_witness = cmp)
      (module Elt : With_examples with type t = elt)
  : (module With_examples with type t = (elt, cmp) Set.t)
  =
  m_biject (m_list (module Elt)) ~f:(Set.of_list (module Cmp)) ~f_inverse:Set.to_list
;;

let m_map
      (type key data cmp)
      (module Cmp : Comparator.S with type t = key and type comparator_witness = cmp)
      (module Key : With_examples with type t = key)
      (module Data : With_examples with type t = data)
  =
  (module struct
    type t = (key, data, cmp) Map.t

    let compare = Map.compare_m__t (module Key) Data.compare
    let sexp_of_t = Map.sexp_of_m__t (module Key) Data.sexp_of_t

    let examples =
      [ Map.empty (module Cmp) ]
      @ List.map Data.examples ~f:(fun data ->
        Map.of_alist_exn
          (module Cmp)
          (List.map Key.examples ~f:(fun key -> key, data)))
    ;;
  end : With_examples
    with type t = (key, data, cmp) Map.t)
;;
