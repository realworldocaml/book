open! Import

type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving_inline compare, hash, sexp, sexp_grammar]

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

let t_of_sexp :
  'f 's.
  (Sexplib0.Sexp.t -> 'f) -> (Sexplib0.Sexp.t -> 's) -> Sexplib0.Sexp.t -> ('f, 's) t
  =
  fun (type f__023_ s__024_)
      :  ((Sexplib0.Sexp.t -> f__023_) -> (Sexplib0.Sexp.t -> s__024_) -> Sexplib0.Sexp.t
          -> (f__023_, s__024_) t) ->
    let error_source__011_ = "either0.ml.t" in
    fun _of_f__007_ _of_s__008_ -> function
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom (("first" | "First") as _tag__014_) :: sexp_args__015_) as
        _sexp__013_ ->
        (match sexp_args__015_ with
         | [ arg0__016_ ] ->
           let res0__017_ = _of_f__007_ arg0__016_ in
           First res0__017_
         | _ ->
           Sexplib0.Sexp_conv_error.stag_incorrect_n_args
             error_source__011_
             _tag__014_
             _sexp__013_)
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom (("second" | "Second") as _tag__019_) :: sexp_args__020_) as
        _sexp__018_ ->
        (match sexp_args__020_ with
         | [ arg0__021_ ] ->
           let res0__022_ = _of_s__008_ arg0__021_ in
           Second res0__022_
         | _ ->
           Sexplib0.Sexp_conv_error.stag_incorrect_n_args
             error_source__011_
             _tag__019_
             _sexp__018_)
      | Sexplib0.Sexp.Atom ("first" | "First") as sexp__012_ ->
        Sexplib0.Sexp_conv_error.stag_takes_args error_source__011_ sexp__012_
      | Sexplib0.Sexp.Atom ("second" | "Second") as sexp__012_ ->
        Sexplib0.Sexp_conv_error.stag_takes_args error_source__011_ sexp__012_
      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__010_ ->
        Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__011_ sexp__010_
      | Sexplib0.Sexp.List [] as sexp__010_ ->
        Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__011_ sexp__010_
      | sexp__010_ ->
        Sexplib0.Sexp_conv_error.unexpected_stag error_source__011_ sexp__010_
;;

let sexp_of_t :
  'f 's.
  ('f -> Sexplib0.Sexp.t) -> ('s -> Sexplib0.Sexp.t) -> ('f, 's) t -> Sexplib0.Sexp.t
  =
  fun (type f__031_ s__032_)
      :  ((f__031_ -> Sexplib0.Sexp.t) -> (s__032_ -> Sexplib0.Sexp.t)
          -> (f__031_, s__032_) t -> Sexplib0.Sexp.t) ->
    fun _of_f__025_ _of_s__026_ -> function
      | First arg0__027_ ->
        let res0__028_ = _of_f__025_ arg0__027_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "First"; res0__028_ ]
      | Second arg0__029_ ->
        let res0__030_ = _of_s__026_ arg0__029_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Second"; res0__030_ ]
;;

let (t_sexp_grammar :
       'f Sexplib0.Sexp_grammar.t
     -> 's Sexplib0.Sexp_grammar.t
     -> ('f, 's) t Sexplib0.Sexp_grammar.t)
  =
  fun _'f_sexp_grammar _'s_sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag
                { name = "First"
                ; clause_kind =
                    List_clause { args = Cons (_'f_sexp_grammar.untyped, Empty) }
                }
            ; No_tag
                { name = "Second"
                ; clause_kind =
                    List_clause { args = Cons (_'s_sexp_grammar.untyped, Empty) }
                }
            ]
        }
  }
;;

[@@@end]
