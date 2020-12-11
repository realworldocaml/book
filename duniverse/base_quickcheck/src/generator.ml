open! Base

module T : sig
  type +'a t

  val create : (size:int -> random:Splittable_random.State.t -> 'a) -> 'a t
  val generate : 'a t -> size:int -> random:Splittable_random.State.t -> 'a
end = struct
  type 'a t = (size:int -> random:Splittable_random.State.t -> 'a) Staged.t

  let create f : _ t = Staged.stage f

  let generate (t : _ t) ~size ~random =
    if size < 0
    then raise_s [%message "Base_quickcheck.Generator.generate: size < 0" (size : int)]
    else Staged.unstage t ~size ~random
  ;;
end

include T

let size = create (fun ~size ~random:_ -> size)

let fn dom rng =
  create (fun ~size ~random ->
    let random = Splittable_random.State.split random in
    fun x ->
      let hash = Observer0.observe dom x ~size ~hash:(Hash.alloc ()) in
      let random = Splittable_random.State.copy random in
      Splittable_random.State.perturb random (Hash.get_hash_value hash);
      generate rng ~size ~random)
;;

let with_size t ~size = create (fun ~size:_ ~random -> generate t ~size ~random)

let perturb t salt =
  create (fun ~size ~random ->
    Splittable_random.State.perturb random salt;
    generate t ~size ~random)
;;

let filter_map t ~f =
  let rec loop ~size ~random =
    let x = generate t ~size ~random in
    match f x with
    | Some y -> y
    | None -> loop ~size:(size + 1) ~random
  in
  create loop
;;

let filter t ~f = filter_map t ~f:(fun x -> if f x then Some x else None)
let return x = create (fun ~size:_ ~random:_ -> x)
let map t ~f = create (fun ~size ~random -> f (generate t ~size ~random))

let apply tf tx =
  create (fun ~size ~random ->
    let f = generate tf ~size ~random in
    let x = generate tx ~size ~random in
    f x)
;;

let bind t ~f =
  create (fun ~size ~random ->
    let x = generate t ~size ~random in
    generate (f x) ~size ~random)
;;

let all list = create (fun ~size ~random -> List.map list ~f:(generate ~size ~random))

let all_unit list =
  create (fun ~size ~random -> List.iter list ~f:(generate ~size ~random))
;;

module For_applicative = Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let apply = apply
    let map = `Custom map
  end)

let both = For_applicative.both
let map2 = For_applicative.map2
let map3 = For_applicative.map3

module Applicative_infix = For_applicative.Applicative_infix
include Applicative_infix

module For_monad = Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

let ignore_m = For_monad.ignore_m
let join = For_monad.join

module Monad_infix = For_monad.Monad_infix
include Monad_infix
module Let_syntax = For_monad.Let_syntax
open Let_syntax

let of_list list =
  if List.is_empty list
  then Error.raise_s [%message "Base_quickcheck.Generator.of_list: empty list"];
  let array = Array.of_list list in
  let lo = 0 in
  let hi = Array.length array - 1 in
  create (fun ~size:_ ~random ->
    let index = Splittable_random.int random ~lo ~hi in
    array.(index))
;;

let union list = join (of_list list)

let of_weighted_list alist =
  if List.is_empty alist
  then Error.raise_s [%message "Base_quickcheck.Generator.of_weighted_list: empty list"];
  let weights, values = List.unzip alist in
  let value_array = Array.of_list values in
  let total_weight, cumulative_weight_array =
    let array = Array.init (Array.length value_array) ~f:(fun _ -> 0.) in
    let sum =
      List.foldi weights ~init:0. ~f:(fun index acc weight ->
        if not (Float.is_finite weight)
        then
          Error.raise_s
            [%message
              "Base_quickcheck.Generator.of_weighted_list: weight is not finite"
                (weight : float)];
        if Float.( < ) weight 0.
        then
          Error.raise_s
            [%message
              "Base_quickcheck.Generator.of_weighted_list: weight is negative"
                (weight : float)];
        let cumulative = acc +. weight in
        array.(index) <- cumulative;
        cumulative)
    in
    if Float.( <= ) sum 0.
    then
      Error.raise_s
        [%message "Base_quickcheck.Generator.of_weighted_list: total weight is zero"];
    sum, array
  in
  create (fun ~size:_ ~random ->
    let choice = Splittable_random.float random ~lo:0. ~hi:total_weight in
    match
      Array.binary_search
        cumulative_weight_array
        ~compare:Float.compare
        `First_greater_than_or_equal_to
        choice
    with
    | Some index -> value_array.(index)
    | None -> assert false)
;;

let weighted_union alist = join (of_weighted_list alist)
let of_lazy lazy_t = create (fun ~size ~random -> generate (force lazy_t) ~size ~random)

let fixed_point of_generator =
  let rec lazy_t = lazy (of_generator (of_lazy lazy_t)) in
  force lazy_t
;;

let weighted_recursive_union nonrec_list ~f =
  fixed_point (fun self ->
    let rec_list =
      List.map (f self) ~f:(fun (w, t) ->
        ( w
        , let%bind n = size in
          with_size ~size:(n - 1) t ))
    in
    if List.is_empty nonrec_list || List.is_empty rec_list
    then
      raise_s
        [%message
          "Base_quickcheck.Generator.weighted_recursive_union: lists must be non-empty"];
    let nonrec_gen = weighted_union nonrec_list in
    let rec_gen = weighted_union (nonrec_list @ rec_list) in
    match%bind size with
    | 0 -> nonrec_gen
    | _ -> rec_gen)
;;

let recursive_union nonrec_list ~f =
  let weighted list = List.map list ~f:(fun t -> 1., t) in
  weighted_recursive_union (weighted nonrec_list) ~f:(fun self -> weighted (f self))
;;

let sizes ?(min_length = 0) ?(max_length = Int.max_value) () =
  create (fun ~size ~random ->
    assert (min_length <= max_length);
    let upper_bound = min_length + size in
    let max_length =
      if upper_bound >= min_length (* guard against overflow *)
      then min max_length upper_bound
      else max_length
    in
    (* pick a length, weighted low so that most of the size is spent on elements *)
    let len = Splittable_random.Log_uniform.int random ~lo:min_length ~hi:max_length in
    (* if there are no elements return an empty array, otherwise return a non-empty array
       with the size distributed among the elements *)
    if len = 0
    then []
    else (
      let sizes = Array.init len ~f:(fun _ -> 0) in
      let remaining = size - (len - min_length) in
      let max_index = len - 1 in
      for _ = 1 to remaining do
        (* pick an index, weighted low so that we see unbalanced distributions often *)
        let index = Splittable_random.Log_uniform.int random ~lo:0 ~hi:max_index in
        sizes.(index) <- sizes.(index) + 1
      done;
      (* permute the array so that no index is favored over another *)
      for i = 0 to max_index - 1 do
        let j = Splittable_random.int random ~lo:i ~hi:max_index in
        Array.swap sizes i j
      done;
      assert (Array.sum (module Int) sizes ~f:Fn.id + (len - min_length) = size);
      Array.to_list sizes))
;;

let unit = return ()
let bool = create (fun ~size:_ ~random -> Splittable_random.bool random)
let option value_t = union [ return None; map value_t ~f:Option.return ]
let either fst_t snd_t = union [ map fst_t ~f:Either.first; map snd_t ~f:Either.second ]

let result ok_t err_t =
  map (either ok_t err_t) ~f:(function
    | First ok -> Ok ok
    | Second err -> Error err)
;;

let list_generic ?min_length ?max_length elt_gen =
  let%bind sizes = sizes ?min_length ?max_length () in
  List.map sizes ~f:(fun size -> with_size ~size elt_gen) |> all
;;

let list elt_gen = list_generic elt_gen
let list_non_empty elt_gen = list_generic ~min_length:1 elt_gen

let list_with_length elt_gen ~length =
  list_generic ~min_length:length ~max_length:length elt_gen
;;

let list_filtered elts =
  let elts = Array.of_list elts in
  let length_of_input = Array.length elts in
  create (fun ~size:_ ~random ->
    let length_of_output = Splittable_random.int random ~lo:0 ~hi:length_of_input in
    let indices = Array.init length_of_input ~f:Fn.id in
    (* Choose [length_of_output] random values in the prefix of [indices]. *)
    for i = 0 to length_of_output - 1 do
      let j = Splittable_random.int random ~lo:i ~hi:(length_of_input - 1) in
      Array.swap indices i j
    done;
    (* Sort the chosen indices because we don't want to reorder them. *)
    Array.sort indices ~pos:0 ~len:length_of_output ~compare:Int.compare;
    (* Return the chosen elements. *)
    List.init length_of_output ~f:(fun i -> elts.(indices.(i))))
;;

let list_permutations list =
  create (fun ~size:_ ~random ->
    let array = Array.of_list list in
    for i = 1 to Array.length array - 1 do
      let j = Splittable_random.int random ~lo:0 ~hi:i in
      Array.swap array i j
    done;
    Array.to_list array)
;;

let char_uniform_inclusive lo hi =
  create (fun ~size:_ ~random ->
    Splittable_random.int random ~lo:(Char.to_int lo) ~hi:(Char.to_int hi)
    |> Char.unsafe_of_int)
;;

let char_uppercase = char_uniform_inclusive 'A' 'Z'
let char_lowercase = char_uniform_inclusive 'a' 'z'
let char_digit = char_uniform_inclusive '0' '9'
let char_print_uniform = char_uniform_inclusive ' ' '~'
let char_uniform = char_uniform_inclusive Char.min_value Char.max_value
let char_alpha = union [ char_lowercase; char_uppercase ]

let char_alphanum =
  weighted_union
    (* Most people probably expect this to be a uniform distribution, not weighted
       toward digits like we would get with [union] (since there are fewer digits than
       letters). *)
    [ 52., char_alpha; 10., char_digit ]
;;

let char_whitespace = of_list (List.filter Char.all ~f:Char.is_whitespace)
let char_print = weighted_union [ 10., char_alphanum; 1., char_print_uniform ]

let char =
  weighted_union
    [ 100., char_print
    ; 10., char_uniform
    ; 1., return Char.min_value
    ; 1., return Char.max_value
    ]
;;

(* Produces a number from 0 or 1 to size + 1, weighted high. We have found this
   distribution empirically useful for string lengths. *)
let small_int ~allow_zero =
  create (fun ~size ~random ->
    let lower_bound = if allow_zero then 0 else 1 in
    let upper_bound = size + 1 in
    let weighted_low =
      Splittable_random.Log_uniform.int random ~lo:0 ~hi:(upper_bound - lower_bound)
    in
    let weighted_high = upper_bound - weighted_low in
    weighted_high)
;;

let small_positive_or_zero_int = small_int ~allow_zero:true
let small_strictly_positive_int = small_int ~allow_zero:false

module type Int_with_random = sig
  include Int.S

  val uniform : Splittable_random.State.t -> lo:t -> hi:t -> t
  val log_uniform : Splittable_random.State.t -> lo:t -> hi:t -> t
end

module For_integer (Integer : Int_with_random) = struct
  let uniform_inclusive lo hi =
    create (fun ~size:_ ~random -> Integer.uniform random ~lo ~hi)
  ;;

  let log_uniform_inclusive lo hi =
    create (fun ~size:_ ~random -> Integer.log_uniform random ~lo ~hi)
  ;;

  let non_uniform f lo hi =
    weighted_union [ 0.05, return lo; 0.05, return hi; 0.9, f lo hi ]
  ;;

  let inclusive = non_uniform uniform_inclusive
  let log_inclusive = non_uniform log_uniform_inclusive
  let uniform_all = uniform_inclusive Integer.min_value Integer.max_value

  let all =
    [%map
      let negative = bool
      and magnitude = log_inclusive Integer.zero Integer.max_value in
      if negative then Integer.bit_not magnitude else magnitude]
  ;;
end

module For_int = For_integer (struct
    include Int

    let uniform = Splittable_random.int
    let log_uniform = Splittable_random.Log_uniform.int
  end)

let int = For_int.all
let int_uniform = For_int.uniform_all
let int_inclusive = For_int.inclusive
let int_uniform_inclusive = For_int.uniform_inclusive
let int_log_inclusive = For_int.log_inclusive
let int_log_uniform_inclusive = For_int.log_uniform_inclusive

module For_int32 = For_integer (struct
    include Int32

    let uniform = Splittable_random.int32
    let log_uniform = Splittable_random.Log_uniform.int32
  end)

let int32 = For_int32.all
let int32_uniform = For_int32.uniform_all
let int32_inclusive = For_int32.inclusive
let int32_uniform_inclusive = For_int32.uniform_inclusive
let int32_log_inclusive = For_int32.log_inclusive
let int32_log_uniform_inclusive = For_int32.log_uniform_inclusive

module For_int63 = For_integer (struct
    include Int63

    let uniform = Splittable_random.int63
    let log_uniform = Splittable_random.Log_uniform.int63
  end)

let int63 = For_int63.all
let int63_uniform = For_int63.uniform_all
let int63_inclusive = For_int63.inclusive
let int63_uniform_inclusive = For_int63.uniform_inclusive
let int63_log_inclusive = For_int63.log_inclusive
let int63_log_uniform_inclusive = For_int63.log_uniform_inclusive

module For_int64 = For_integer (struct
    include Int64

    let uniform = Splittable_random.int64
    let log_uniform = Splittable_random.Log_uniform.int64
  end)

let int64 = For_int64.all
let int64_uniform = For_int64.uniform_all
let int64_inclusive = For_int64.inclusive
let int64_uniform_inclusive = For_int64.uniform_inclusive
let int64_log_inclusive = For_int64.log_inclusive
let int64_log_uniform_inclusive = For_int64.log_uniform_inclusive

module For_nativeint = For_integer (struct
    include Nativeint

    let uniform = Splittable_random.nativeint
    let log_uniform = Splittable_random.Log_uniform.nativeint
  end)

let nativeint = For_nativeint.all
let nativeint_uniform = For_nativeint.uniform_all
let nativeint_inclusive = For_nativeint.inclusive
let nativeint_uniform_inclusive = For_nativeint.uniform_inclusive
let nativeint_log_inclusive = For_nativeint.log_inclusive
let nativeint_log_uniform_inclusive = For_nativeint.log_uniform_inclusive
let float_zero_exponent = Float.ieee_exponent 0.
let float_zero_mantissa = Float.ieee_mantissa 0.

let float_max_positive_subnormal_value =
  Float.one_ulp `Down Float.min_positive_normal_value
;;

let float_subnormal_exponent = Float.ieee_exponent Float.min_positive_subnormal_value
let float_min_subnormal_mantissa = Float.ieee_mantissa Float.min_positive_subnormal_value
let float_max_subnormal_mantissa = Float.ieee_mantissa float_max_positive_subnormal_value
let float_max_positive_normal_value = Float.max_finite_value
let float_min_normal_exponent = Float.ieee_exponent Float.min_positive_normal_value
let float_max_normal_exponent = Float.ieee_exponent float_max_positive_normal_value
let float_max_normal_mantissa = Float.ieee_mantissa float_max_positive_normal_value
let float_inf_exponent = Float.ieee_exponent Float.infinity
let float_inf_mantissa = Float.ieee_mantissa Float.infinity
let float_nan_exponent = Float.ieee_exponent Float.nan
let float_min_nan_mantissa = Int63.succ float_inf_mantissa
let float_max_nan_mantissa = float_max_normal_mantissa
let float_num_mantissa_bits = 52

(* We weight mantissas so that "integer-like" values, and values with only a few digits
   past the decimal, are reasonably common. *)
let float_normal_mantissa =
  let%bind num_bits = For_int.uniform_inclusive 0 float_num_mantissa_bits in
  let%map bits =
    For_int63.inclusive Int63.zero (Int63.pred (Int63.shift_left Int63.one num_bits))
  in
  Int63.shift_left bits (Int.( - ) float_num_mantissa_bits num_bits)
;;

let float_exponent_weighted_low lower_bound upper_bound =
  let%map offset = For_int.log_inclusive 0 (Int.( - ) upper_bound lower_bound) in
  Int.( + ) lower_bound offset
;;

let float_exponent_weighted_high lower_bound upper_bound =
  let%map offset = For_int.log_inclusive 0 (Int.( - ) upper_bound lower_bound) in
  Int.( - ) upper_bound offset
;;

(* We weight exponents such that values near 1 are more likely. *)
let float_exponent =
  let midpoint = Float.ieee_exponent 1. in
  union
    [ float_exponent_weighted_high float_min_normal_exponent midpoint
    ; float_exponent_weighted_low midpoint float_max_normal_exponent
    ]
;;

let float_zero =
  let%map negative = bool in
  Float.create_ieee_exn
    ~negative
    ~exponent:float_zero_exponent
    ~mantissa:float_zero_mantissa
;;

let float_subnormal =
  let%map negative = bool
  and exponent = return float_subnormal_exponent
  and mantissa =
    For_int63.log_inclusive float_min_subnormal_mantissa float_max_subnormal_mantissa
  in
  Float.create_ieee_exn ~negative ~exponent ~mantissa
;;

let float_normal =
  let%map negative = bool
  and exponent = float_exponent
  and mantissa = float_normal_mantissa in
  Float.create_ieee_exn ~negative ~exponent ~mantissa
;;

let float_infinite =
  let%map negative = bool in
  Float.create_ieee_exn
    ~negative
    ~exponent:float_inf_exponent
    ~mantissa:float_inf_mantissa
;;

let float_nan =
  let%map negative = bool
  and exponent = return float_nan_exponent
  and mantissa = For_int63.inclusive float_min_nan_mantissa float_max_nan_mantissa in
  Float.create_ieee_exn ~negative ~exponent ~mantissa
;;

let float_of_class c =
  match (c : Float.Class.t) with
  | Zero -> float_zero
  | Subnormal -> float_subnormal
  | Normal -> float_normal
  | Infinite -> float_infinite
  | Nan -> float_nan
;;

let float_weight_of_class c =
  match (c : Float.Class.t) with
  | Zero -> 1.
  | Subnormal -> 10.
  | Normal -> 100.
  | Infinite -> 1.
  | Nan -> 1.
;;

let float_matching_classes filter =
  List.filter_map Float.Class.all ~f:(fun c ->
    if filter c then Some (float_weight_of_class c, float_of_class c) else None)
  |> weighted_union
;;

let float_finite =
  float_matching_classes (function
    | Zero | Subnormal | Normal -> true
    | Infinite | Nan -> false)
;;

let float_without_nan =
  float_matching_classes (function
    | Zero | Subnormal | Normal | Infinite -> true
    | Nan -> false)
;;

let float = float_matching_classes (fun _ -> true)

let float_finite_non_zero =
  float_matching_classes (function
    | Subnormal | Normal -> true
    | Zero | Infinite | Nan -> false)
;;

let float_strictly_positive =
  let%map t = float_finite_non_zero in
  Float.abs t
;;

let float_strictly_negative =
  let%map t = float_finite_non_zero in
  ~-.(Float.abs t)
;;

let float_positive_or_zero =
  let%map t = float_finite in
  Float.abs t
;;

let float_negative_or_zero =
  let%map t = float_finite in
  ~-.(Float.abs t)
;;

let float_uniform_exclusive lower_bound upper_bound =
  let open Float.O in
  if (not (Float.is_finite lower_bound)) || not (Float.is_finite upper_bound)
  then
    raise_s
      [%message
        "Float.uniform_exclusive: bounds are not finite"
          (lower_bound : float)
          (upper_bound : float)];
  let lower_inclusive = Float.one_ulp `Up lower_bound in
  let upper_inclusive = Float.one_ulp `Down upper_bound in
  if lower_inclusive > upper_inclusive
  then
    raise_s
      [%message
        "Float.uniform_exclusive: requested range is empty"
          (lower_bound : float)
          (upper_bound : float)];
  create (fun ~size:_ ~random ->
    Splittable_random.float random ~lo:lower_inclusive ~hi:upper_inclusive)
;;

let float_inclusive lower_bound upper_bound =
  weighted_union
    [ 0.05, return lower_bound
    ; 0.05, return upper_bound
    ; 0.9, float_uniform_exclusive lower_bound upper_bound
    ]
;;

let string_with_length_of char_gen ~length =
  list_with_length char_gen ~length |> map ~f:String.of_char_list
;;

let string_of char_gen =
  bind small_positive_or_zero_int ~f:(fun length ->
    string_with_length_of char_gen ~length)
;;

let string_non_empty_of char_gen =
  bind small_strictly_positive_int ~f:(fun length ->
    string_with_length_of char_gen ~length)
;;

let string = string_of char
let string_non_empty = string_non_empty_of char
let string_with_length ~length = string_with_length_of char ~length

let sexp_of atom =
  fixed_point (fun self ->
    let%bind size = size in
    (* choose a number weighted low so we have a decreasing, but not vanishing, chance
       to generate atoms as size grows *)
    match%bind For_int.log_uniform_inclusive 0 (size + 1) with
    (* generate an atom using the given size *)
    | 0 ->
      let%map atom = atom in
      Sexp.Atom atom
    (* relying on [List.gen] to distribute [size] over sub-sexps *)
    | _ ->
      let%map list = list self in
      Sexp.List list)
;;

let sexp = sexp_of string

let map_tree_using_comparator ~comparator key_gen data_gen =
  let%bind keys = list key_gen in
  let keys = List.dedup_and_sort keys ~compare:comparator.Comparator.compare in
  let%bind data = list_with_length data_gen ~length:(List.length keys) in
  return (Map.Using_comparator.Tree.of_alist_exn ~comparator (List.zip_exn keys data))
;;

let set_tree_using_comparator ~comparator elt_gen =
  map (list elt_gen) ~f:(Set.Using_comparator.Tree.of_list ~comparator)
;;

let comparator_of_m
      (type a c)
      (module M : Comparator.S with type t = a and type comparator_witness = c)
  =
  M.comparator
;;

let map_t_m m key_gen data_gen =
  let comparator = comparator_of_m m in
  map_tree_using_comparator ~comparator key_gen data_gen
  |> map ~f:(Map.Using_comparator.of_tree ~comparator)
;;

let set_t_m m elt_gen =
  let comparator = comparator_of_m m in
  set_tree_using_comparator ~comparator elt_gen
  |> map ~f:(Set.Using_comparator.of_tree ~comparator)
;;

let bigarray1 t kind layout =
  let%map elts = list t in
  let elts = Array.of_list elts in
  let dim = Array.length elts in
  let offset = Bigarray_helpers.Layout.offset layout in
  Bigarray_helpers.Array1.init kind layout dim ~f:(fun i -> elts.(i - offset))
;;

let bigstring = bigarray1 char Char C_layout
let float32_vec = bigarray1 float Float32 Fortran_layout
let float64_vec = bigarray1 float Float64 Fortran_layout

let bigarray2_dim =
  match%bind size with
  | 0 -> return (0, 0)
  | max_total_size ->
    let%bind a =
      (* choose a dimension up to [max_total_size], weighted low to give the other
         dimension a good chance of being comparatively high *)
      int_log_uniform_inclusive 1 max_total_size
    in
    let%bind b =
      (* choose a dimension up to [max_total_size / a], weighted high to reach close to
         [max_total_size] most of the time *)
      let max_b = max_total_size / a in
      let%map b_weighted_low = int_log_uniform_inclusive 0 max_b in
      max_b - b_weighted_low
    in
    (* avoid any skew of a vs b by randomly swapping *)
    if%map bool then a, b else b, a
;;

let bigarray2 t kind layout =
  let%bind dim1, dim2 = bigarray2_dim in
  let%map elts = list_with_length ~length:dim1 (list_with_length ~length:dim2 t) in
  let elts = Array.of_list_map ~f:Array.of_list elts in
  let offset = Bigarray_helpers.Layout.offset layout in
  Bigarray_helpers.Array2.init kind layout dim1 dim2 ~f:(fun i j ->
    elts.(i - offset).(j - offset))
;;

let float32_mat = bigarray2 float Float32 Fortran_layout
let float64_mat = bigarray2 float Float64 Fortran_layout
