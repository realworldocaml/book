open! Import

type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving_inline compare, hash, sexp]

let compare :
  'f 's. ('f -> 'f -> int) -> ('s -> 's -> int) -> ('f, 's) t -> ('f, 's) t -> int
  =
  fun _cmp__f _cmp__s a__001_ b__002_ ->
  if Ppx_compare_lib.phys_equal a__001_ b__002_
  then 0
  else (
    match a__001_, b__002_ with
    | First _a__003_, First _b__004_ -> _cmp__f _a__003_ _b__004_
    | First _, _ -> -1
    | _, First _ -> 1
    | Second _a__005_, Second _b__006_ -> _cmp__s _a__005_ _b__006_)
;;

let hash_fold_t
  : type f s.
    (Ppx_hash_lib.Std.Hash.state -> f -> Ppx_hash_lib.Std.Hash.state)
    -> (Ppx_hash_lib.Std.Hash.state -> s -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> (f, s) t
    -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_f _hash_fold_s hsv arg ->
  match arg with
  | First _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in
    _hash_fold_f hsv _a0
  | Second _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in
    _hash_fold_s hsv _a0
;;

let t_of_sexp
  : type f s.
    (Ppx_sexp_conv_lib.Sexp.t -> f)
    -> (Ppx_sexp_conv_lib.Sexp.t -> s)
    -> Ppx_sexp_conv_lib.Sexp.t
    -> (f, s) t
  =
  let _tp_loc = "either0.ml.t" in
  fun _of_f _of_s -> function
    | Ppx_sexp_conv_lib.Sexp.List
        (Ppx_sexp_conv_lib.Sexp.Atom (("first" | "First") as _tag) :: sexp_args) as _sexp
      ->
      (match sexp_args with
       | [ v0 ] ->
         let v0 = _of_f v0 in
         First v0
       | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Ppx_sexp_conv_lib.Sexp.List
        (Ppx_sexp_conv_lib.Sexp.Atom (("second" | "Second") as _tag) :: sexp_args) as
      _sexp ->
      (match sexp_args with
       | [ v0 ] ->
         let v0 = _of_s v0 in
         Second v0
       | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Ppx_sexp_conv_lib.Sexp.Atom ("first" | "First") as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.Atom ("second" | "Second") as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
      Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
      Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
    | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
;;

let sexp_of_t
  : type f s.
    (f -> Ppx_sexp_conv_lib.Sexp.t)
    -> (s -> Ppx_sexp_conv_lib.Sexp.t)
    -> (f, s) t
    -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_f _of_s -> function
    | First v0 ->
      let v0 = _of_f v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "First"; v0 ]
    | Second v0 ->
      let v0 = _of_s v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Second"; v0 ]
;;

[@@@end]
