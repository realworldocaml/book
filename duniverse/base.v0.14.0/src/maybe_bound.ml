open! Import

type 'a t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving_inline enumerate, sexp]

let all : 'a. 'a list -> 'a t list =
  fun _all_of_a ->
  Ppx_enumerate_lib.List.append
    (let rec map l acc =
       match l with
       | [] -> Ppx_enumerate_lib.List.rev acc
       | enumerate__001_ :: l -> map l (Incl enumerate__001_ :: acc)
     in
     map _all_of_a [])
    (Ppx_enumerate_lib.List.append
       (let rec map l acc =
          match l with
          | [] -> Ppx_enumerate_lib.List.rev acc
          | enumerate__002_ :: l -> map l (Excl enumerate__002_ :: acc)
        in
        map _all_of_a [])
       [ Unbounded ])
;;

let t_of_sexp
  : type a. (Ppx_sexp_conv_lib.Sexp.t -> a) -> Ppx_sexp_conv_lib.Sexp.t -> a t
  =
  let _tp_loc = "maybe_bound.ml.t" in
  fun _of_a -> function
    | Ppx_sexp_conv_lib.Sexp.List
        (Ppx_sexp_conv_lib.Sexp.Atom (("incl" | "Incl") as _tag) :: sexp_args) as _sexp
      ->
      (match sexp_args with
       | [ v0 ] ->
         let v0 = _of_a v0 in
         Incl v0
       | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Ppx_sexp_conv_lib.Sexp.List
        (Ppx_sexp_conv_lib.Sexp.Atom (("excl" | "Excl") as _tag) :: sexp_args) as _sexp
      ->
      (match sexp_args with
       | [ v0 ] ->
         let v0 = _of_a v0 in
         Excl v0
       | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Ppx_sexp_conv_lib.Sexp.Atom ("unbounded" | "Unbounded") -> Unbounded
    | Ppx_sexp_conv_lib.Sexp.Atom ("incl" | "Incl") as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.Atom ("excl" | "Excl") as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List
        (Ppx_sexp_conv_lib.Sexp.Atom ("unbounded" | "Unbounded") :: _) as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
      Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
      Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
    | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
;;

let sexp_of_t
  : type a. (a -> Ppx_sexp_conv_lib.Sexp.t) -> a t -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_a -> function
    | Incl v0 ->
      let v0 = _of_a v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Incl"; v0 ]
    | Excl v0 ->
      let v0 = _of_a v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Excl"; v0 ]
    | Unbounded -> Ppx_sexp_conv_lib.Sexp.Atom "Unbounded"
;;

[@@@end]

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving_inline sexp, compare, hash]

let interval_comparison_of_sexp =
  (let _tp_loc = "maybe_bound.ml.interval_comparison" in
   function
   | Ppx_sexp_conv_lib.Sexp.Atom ("below_lower_bound" | "Below_lower_bound") ->
     Below_lower_bound
   | Ppx_sexp_conv_lib.Sexp.Atom ("in_range" | "In_range") -> In_range
   | Ppx_sexp_conv_lib.Sexp.Atom ("above_upper_bound" | "Above_upper_bound") ->
     Above_upper_bound
   | Ppx_sexp_conv_lib.Sexp.List
       (Ppx_sexp_conv_lib.Sexp.Atom ("below_lower_bound" | "Below_lower_bound") :: _) as
     sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List
       (Ppx_sexp_conv_lib.Sexp.Atom ("in_range" | "In_range") :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List
       (Ppx_sexp_conv_lib.Sexp.Atom ("above_upper_bound" | "Above_upper_bound") :: _) as
     sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
     Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
   | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
             : Ppx_sexp_conv_lib.Sexp.t -> interval_comparison)
;;

let sexp_of_interval_comparison =
  (function
    | Below_lower_bound -> Ppx_sexp_conv_lib.Sexp.Atom "Below_lower_bound"
    | In_range -> Ppx_sexp_conv_lib.Sexp.Atom "In_range"
    | Above_upper_bound -> Ppx_sexp_conv_lib.Sexp.Atom "Above_upper_bound"
                           : interval_comparison -> Ppx_sexp_conv_lib.Sexp.t)
;;

let compare_interval_comparison =
  (Ppx_compare_lib.polymorphic_compare
   : interval_comparison -> interval_comparison -> int)
;;

let (hash_fold_interval_comparison :
       Ppx_hash_lib.Std.Hash.state -> interval_comparison -> Ppx_hash_lib.Std.Hash.state)
  =
  (fun hsv arg ->
     match arg with
     | Below_lower_bound -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
     | In_range -> Ppx_hash_lib.Std.Hash.fold_int hsv 1
     | Above_upper_bound -> Ppx_hash_lib.Std.Hash.fold_int hsv 2
                            : Ppx_hash_lib.Std.Hash.state -> interval_comparison -> Ppx_hash_lib.Std.Hash.state)
;;

let (hash_interval_comparison : interval_comparison -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create () in
       hash_fold_interval_comparison hsv arg)
  in
  fun x -> func x
;;

[@@@end]

let map t ~f =
  match t with
  | Incl incl -> Incl (f incl)
  | Excl excl -> Excl (f excl)
  | Unbounded -> Unbounded
;;

let is_lower_bound t ~of_:a ~compare =
  match t with
  | Incl incl -> compare incl a <= 0
  | Excl excl -> compare excl a < 0
  | Unbounded -> true
;;

let is_upper_bound t ~of_:a ~compare =
  match t with
  | Incl incl -> compare a incl <= 0
  | Excl excl -> compare a excl < 0
  | Unbounded -> true
;;

let bounds_crossed ~lower ~upper ~compare =
  match lower with
  | Unbounded -> false
  | Incl lower | Excl lower ->
    (match upper with
     | Unbounded -> false
     | Incl upper | Excl upper -> compare lower upper > 0)
;;

let check_interval_exn ~lower ~upper ~compare =
  if bounds_crossed ~lower ~upper ~compare
  then failwith "Maybe_bound.compare_to_interval_exn: lower bound > upper bound"
;;

let compare_to_interval_exn ~lower ~upper a ~compare =
  check_interval_exn ~lower ~upper ~compare;
  if not (is_lower_bound lower ~of_:a ~compare)
  then Below_lower_bound
  else if not (is_upper_bound upper ~of_:a ~compare)
  then Above_upper_bound
  else In_range
;;

let interval_contains_exn ~lower ~upper a ~compare =
  match compare_to_interval_exn ~lower ~upper a ~compare with
  | In_range -> true
  | Below_lower_bound | Above_upper_bound -> false
;;
