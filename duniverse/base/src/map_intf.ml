open! Import
open! T

module Or_duplicate = struct
  type 'a t =
    [ `Ok of 'a
    | `Duplicate
    ]
  [@@deriving_inline compare, equal, sexp_of]

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__001_ b__002_ ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else (
      match a__001_, b__002_ with
      | `Ok _left__003_, `Ok _right__004_ -> _cmp__a _left__003_ _right__004_
      | `Duplicate, `Duplicate -> 0
      | x, y -> Ppx_compare_lib.polymorphic_compare x y)
  ;;

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__005_ b__006_ ->
      if Ppx_compare_lib.phys_equal a__005_ b__006_
      then true
      else (
        match a__005_, b__006_ with
        | `Ok _left__007_, `Ok _right__008_ -> _cmp__a _left__007_ _right__008_
        | `Duplicate, `Duplicate -> true
        | x, y -> Ppx_compare_lib.polymorphic_equal x y)
  ;;

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__009_ -> function
      | `Ok v__010_ -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Ok"; _of_a__009_ v__010_ ]
      | `Duplicate -> Sexplib0.Sexp.Atom "Duplicate"
  ;;

  [@@@end]
end

module Without_comparator = struct
  type ('key, 'cmp, 'z) t = 'z
end

module With_comparator = struct
  type ('key, 'cmp, 'z) t = comparator:('key, 'cmp) Comparator.t -> 'z
end

module With_first_class_module = struct
  type ('key, 'cmp, 'z) t = ('key, 'cmp) Comparator.Module.t -> 'z
end

module Symmetric_diff_element = struct
  type ('k, 'v) t = 'k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]
  [@@deriving_inline compare, equal, sexp, sexp_grammar]

  let compare :
    'k 'v. ('k -> 'k -> int) -> ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int
    =
    fun _cmp__k _cmp__v a__011_ b__012_ ->
    let t__013_, t__014_ = a__011_ in
    let t__015_, t__016_ = b__012_ in
    match _cmp__k t__013_ t__015_ with
    | 0 ->
      if Ppx_compare_lib.phys_equal t__014_ t__016_
      then 0
      else (
        match t__014_, t__016_ with
        | `Left _left__017_, `Left _right__018_ -> _cmp__v _left__017_ _right__018_
        | `Right _left__019_, `Right _right__020_ -> _cmp__v _left__019_ _right__020_
        | `Unequal _left__021_, `Unequal _right__022_ ->
          let t__023_, t__024_ = _left__021_ in
          let t__025_, t__026_ = _right__022_ in
          (match _cmp__v t__023_ t__025_ with
           | 0 -> _cmp__v t__024_ t__026_
           | n -> n)
        | x, y -> Ppx_compare_lib.polymorphic_compare x y)
    | n -> n
  ;;

  let equal :
    'k 'v.
    ('k -> 'k -> bool) -> ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool
    =
    fun _cmp__k _cmp__v a__027_ b__028_ ->
      let t__029_, t__030_ = a__027_ in
      let t__031_, t__032_ = b__028_ in
      Ppx_compare_lib.( && )
        (_cmp__k t__029_ t__031_)
        (if Ppx_compare_lib.phys_equal t__030_ t__032_
         then true
         else (
           match t__030_, t__032_ with
           | `Left _left__033_, `Left _right__034_ -> _cmp__v _left__033_ _right__034_
           | `Right _left__035_, `Right _right__036_ -> _cmp__v _left__035_ _right__036_
           | `Unequal _left__037_, `Unequal _right__038_ ->
             let t__039_, t__040_ = _left__037_ in
             let t__041_, t__042_ = _right__038_ in
             Ppx_compare_lib.( && ) (_cmp__v t__039_ t__041_) (_cmp__v t__040_ t__042_)
           | x, y -> Ppx_compare_lib.polymorphic_equal x y))
  ;;

  let t_of_sexp :
    'k 'v.
    (Sexplib0.Sexp.t -> 'k)
    -> (Sexplib0.Sexp.t -> 'v)
    -> Sexplib0.Sexp.t
    -> ('k, 'v) t
    =
    let error_source__057_ = "map_intf.ml.Symmetric_diff_element.t" in
    fun _of_k__043_ _of_v__044_ -> function
      | Sexplib0.Sexp.List [ arg0__067_; arg1__068_ ] ->
        let res0__069_ = _of_k__043_ arg0__067_
        and res1__070_ =
          let sexp__066_ = arg1__068_ in
          try
            match sexp__066_ with
            | Sexplib0.Sexp.Atom atom__047_ as _sexp__049_ ->
              (match atom__047_ with
               | "Left" ->
                 Sexplib0.Sexp_conv_error.ptag_takes_args error_source__057_ _sexp__049_
               | "Right" ->
                 Sexplib0.Sexp_conv_error.ptag_takes_args error_source__057_ _sexp__049_
               | "Unequal" ->
                 Sexplib0.Sexp_conv_error.ptag_takes_args error_source__057_ _sexp__049_
               | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
            | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__047_ :: sexp_args__050_) as
              _sexp__049_ ->
              (match atom__047_ with
               | "Left" as _tag__063_ ->
                 (match sexp_args__050_ with
                  | [ arg0__064_ ] ->
                    let res0__065_ = _of_v__044_ arg0__064_ in
                    `Left res0__065_
                  | _ ->
                    Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                      error_source__057_
                      _tag__063_
                      _sexp__049_)
               | "Right" as _tag__060_ ->
                 (match sexp_args__050_ with
                  | [ arg0__061_ ] ->
                    let res0__062_ = _of_v__044_ arg0__061_ in
                    `Right res0__062_
                  | _ ->
                    Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                      error_source__057_
                      _tag__060_
                      _sexp__049_)
               | "Unequal" as _tag__051_ ->
                 (match sexp_args__050_ with
                  | [ arg0__058_ ] ->
                    let res0__059_ =
                      match arg0__058_ with
                      | Sexplib0.Sexp.List [ arg0__052_; arg1__053_ ] ->
                        let res0__054_ = _of_v__044_ arg0__052_
                        and res1__055_ = _of_v__044_ arg1__053_ in
                        res0__054_, res1__055_
                      | sexp__056_ ->
                        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                          error_source__057_
                          2
                          sexp__056_
                    in
                    `Unequal res0__059_
                  | _ ->
                    Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                      error_source__057_
                      _tag__051_
                      _sexp__049_)
               | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
            | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__048_ ->
              Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                error_source__057_
                sexp__048_
            | Sexplib0.Sexp.List [] as sexp__048_ ->
              Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                error_source__057_
                sexp__048_
          with
          | Sexplib0.Sexp_conv_error.No_variant_match ->
            Sexplib0.Sexp_conv_error.no_matching_variant_found
              error_source__057_
              sexp__066_
        in
        res0__069_, res1__070_
      | sexp__071_ ->
        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__057_ 2 sexp__071_
  ;;

  let sexp_of_t :
    'k 'v.
    ('k -> Sexplib0.Sexp.t)
    -> ('v -> Sexplib0.Sexp.t)
    -> ('k, 'v) t
    -> Sexplib0.Sexp.t
    =
    fun _of_k__072_ _of_v__073_ (arg0__081_, arg1__082_) ->
      let res0__083_ = _of_k__072_ arg0__081_
      and res1__084_ =
        match arg1__082_ with
        | `Left v__074_ ->
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Left"; _of_v__073_ v__074_ ]
        | `Right v__075_ ->
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Right"; _of_v__073_ v__075_ ]
        | `Unequal v__076_ ->
          Sexplib0.Sexp.List
            [ Sexplib0.Sexp.Atom "Unequal"
            ; (let arg0__077_, arg1__078_ = v__076_ in
               let res0__079_ = _of_v__073_ arg0__077_
               and res1__080_ = _of_v__073_ arg1__078_ in
               Sexplib0.Sexp.List [ res0__079_; res1__080_ ])
            ]
      in
      Sexplib0.Sexp.List [ res0__083_; res1__084_ ]
  ;;

  let (t_sexp_grammar :
         'k Sexplib0.Sexp_grammar.t
       -> 'v Sexplib0.Sexp_grammar.t
       -> ('k, 'v) t Sexplib0.Sexp_grammar.t)
    =
    fun _'k_sexp_grammar _'v_sexp_grammar ->
      { untyped =
          List
            (Cons
               ( _'k_sexp_grammar.untyped
               , Cons
                   ( Variant
                       { case_sensitivity = Case_sensitive
                       ; clauses =
                           [ No_tag
                               { name = "Left"
                               ; clause_kind =
                                   List_clause
                                     { args = Cons (_'v_sexp_grammar.untyped, Empty) }
                               }
                           ; No_tag
                               { name = "Right"
                               ; clause_kind =
                                   List_clause
                                     { args = Cons (_'v_sexp_grammar.untyped, Empty) }
                               }
                           ; No_tag
                               { name = "Unequal"
                               ; clause_kind =
                                   List_clause
                                     { args =
                                         Cons
                                           ( List
                                               (Cons
                                                  ( _'v_sexp_grammar.untyped
                                                  , Cons (_'v_sexp_grammar.untyped, Empty)
                                                  ))
                                           , Empty )
                                     }
                               }
                           ]
                       }
                   , Empty ) ))
      }
  ;;

  [@@@end]
end

module Merge_element = struct
  type ('left, 'right) t =
    [ `Left of 'left
    | `Right of 'right
    | `Both of 'left * 'right
    ]
  [@@deriving_inline compare, equal, sexp_of]

  let compare :
    'left 'right.
    ('left -> 'left -> int)
    -> ('right -> 'right -> int)
    -> ('left, 'right) t
    -> ('left, 'right) t
    -> int
    =
    fun _cmp__left _cmp__right a__085_ b__086_ ->
    if Ppx_compare_lib.phys_equal a__085_ b__086_
    then 0
    else (
      match a__085_, b__086_ with
      | `Left _left__087_, `Left _right__088_ -> _cmp__left _left__087_ _right__088_
      | `Right _left__089_, `Right _right__090_ -> _cmp__right _left__089_ _right__090_
      | `Both _left__091_, `Both _right__092_ ->
        let t__093_, t__094_ = _left__091_ in
        let t__095_, t__096_ = _right__092_ in
        (match _cmp__left t__093_ t__095_ with
         | 0 -> _cmp__right t__094_ t__096_
         | n -> n)
      | x, y -> Ppx_compare_lib.polymorphic_compare x y)
  ;;

  let equal :
    'left 'right.
    ('left -> 'left -> bool)
    -> ('right -> 'right -> bool)
    -> ('left, 'right) t
    -> ('left, 'right) t
    -> bool
    =
    fun _cmp__left _cmp__right a__097_ b__098_ ->
      if Ppx_compare_lib.phys_equal a__097_ b__098_
      then true
      else (
        match a__097_, b__098_ with
        | `Left _left__099_, `Left _right__100_ -> _cmp__left _left__099_ _right__100_
        | `Right _left__101_, `Right _right__102_ -> _cmp__right _left__101_ _right__102_
        | `Both _left__103_, `Both _right__104_ ->
          let t__105_, t__106_ = _left__103_ in
          let t__107_, t__108_ = _right__104_ in
          Ppx_compare_lib.( && ) (_cmp__left t__105_ t__107_) (_cmp__right t__106_ t__108_)
        | x, y -> Ppx_compare_lib.polymorphic_equal x y)
  ;;

  let sexp_of_t :
    'left 'right.
    ('left -> Sexplib0.Sexp.t)
    -> ('right -> Sexplib0.Sexp.t)
    -> ('left, 'right) t
    -> Sexplib0.Sexp.t
    =
    fun _of_left__109_ _of_right__110_ -> function
      | `Left v__111_ ->
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Left"; _of_left__109_ v__111_ ]
      | `Right v__112_ ->
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Right"; _of_right__110_ v__112_ ]
      | `Both v__113_ ->
        Sexplib0.Sexp.List
          [ Sexplib0.Sexp.Atom "Both"
          ; (let arg0__114_, arg1__115_ = v__113_ in
             let res0__116_ = _of_left__109_ arg0__114_
             and res1__117_ = _of_right__110_ arg1__115_ in
             Sexplib0.Sexp.List [ res0__116_; res1__117_ ])
          ]
  ;;

  [@@@end]
end

(** @canonical Base.Map.Continue_or_stop *)
module Continue_or_stop = struct
  type t =
    | Continue
    | Stop
  [@@deriving_inline compare, enumerate, equal, sexp_of]

  let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)
  let all = ([ Continue; Stop ] : t list)
  let equal = (Ppx_compare_lib.polymorphic_equal : t -> t -> bool)

  let sexp_of_t =
    (function
      | Continue -> Sexplib0.Sexp.Atom "Continue"
      | Stop -> Sexplib0.Sexp.Atom "Stop"
                : t -> Sexplib0.Sexp.t)
  ;;

  [@@@end]
end

(** @canonical Base.Map.Finished_or_unfinished *)
module Finished_or_unfinished = struct
  type t =
    | Finished
    | Unfinished
  [@@deriving_inline compare, enumerate, equal, sexp_of]

  let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)
  let all = ([ Finished; Unfinished ] : t list)
  let equal = (Ppx_compare_lib.polymorphic_equal : t -> t -> bool)

  let sexp_of_t =
    (function
      | Finished -> Sexplib0.Sexp.Atom "Finished"
      | Unfinished -> Sexplib0.Sexp.Atom "Unfinished"
                      : t -> Sexplib0.Sexp.t)
  ;;

  [@@@end]
end

module type Accessors_generic = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree
  type 'a key
  type 'cmp cmp
  type ('a, 'cmp, 'z) options

  val invariants : ('k, 'cmp, ('k, 'v, 'cmp) t -> bool) options
  val is_empty : (_, _, _) t -> bool
  val length : (_, _, _) t -> int

  val add
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t -> key:'k key -> data:'v -> ('k, 'v, 'cmp) t Or_duplicate.t )
        options

  val add_exn
    : ('k, 'cmp, ('k, 'v, 'cmp) t -> key:'k key -> data:'v -> ('k, 'v, 'cmp) t) options

  val set
    : ('k, 'cmp, ('k, 'v, 'cmp) t -> key:'k key -> data:'v -> ('k, 'v, 'cmp) t) options

  val add_multi
    : ( 'k
      , 'cmp
      , ('k, 'v list, 'cmp) t -> key:'k key -> data:'v -> ('k, 'v list, 'cmp) t )
        options

  val remove_multi
    : ('k, 'cmp, ('k, 'v list, 'cmp) t -> 'k key -> ('k, 'v list, 'cmp) t) options

  val find_multi : ('k, 'cmp, ('k, 'v list, 'cmp) t -> 'k key -> 'v list) options

  val change
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t -> 'k key -> f:('v option -> 'v option) -> ('k, 'v, 'cmp) t )
        options

  val update
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t -> 'k key -> f:('v option -> 'v) -> ('k, 'v, 'cmp) t )
        options

  val find : ('k, 'cmp, ('k, 'v, 'cmp) t -> 'k key -> 'v option) options
  val find_exn : ('k, 'cmp, ('k, 'v, 'cmp) t -> 'k key -> 'v) options
  val remove : ('k, 'cmp, ('k, 'v, 'cmp) t -> 'k key -> ('k, 'v, 'cmp) t) options
  val mem : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> bool) options
  val iter_keys : ('k, _, _) t -> f:('k key -> unit) -> unit
  val iter : (_, 'v, _) t -> f:('v -> unit) -> unit
  val iteri : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> unit) -> unit

  val iteri_until
    :  ('k, 'v, _) t
    -> f:(key:'k key -> data:'v -> Continue_or_stop.t)
    -> Finished_or_unfinished.t

  val iter2
    : ( 'k
      , 'cmp
      , ('k, 'v1, 'cmp) t
      -> ('k, 'v2, 'cmp) t
      -> f:(key:'k key -> data:('v1, 'v2) Merge_element.t -> unit)
      -> unit )
        options

  val map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'cmp) t
  val mapi : ('k, 'v1, 'cmp) t -> f:(key:'k key -> data:'v1 -> 'v2) -> ('k, 'v2, 'cmp) t
  val fold : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  val fold_until
    :  ('k, 'v, _) t
    -> init:'a
    -> f:(key:'k key -> data:'v -> 'a -> ('a, 'final) Container.Continue_or_stop.t)
    -> finish:('a -> 'final)
    -> 'final

  val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  val fold2
    : ( 'k
      , 'cmp
      , ('k, 'v1, 'cmp) t
      -> ('k, 'v2, 'cmp) t
      -> init:'a
      -> f:(key:'k key -> data:('v1, 'v2) Merge_element.t -> 'a -> 'a)
      -> 'a )
        options

  val filter_keys
    : ('k, 'cmp, ('k, 'v, 'cmp) t -> f:('k key -> bool) -> ('k, 'v, 'cmp) t) options

  val filter : ('k, 'cmp, ('k, 'v, 'cmp) t -> f:('v -> bool) -> ('k, 'v, 'cmp) t) options

  val filteri
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t -> f:(key:'k key -> data:'v -> bool) -> ('k, 'v, 'cmp) t )
        options

  val filter_map
    : ('k, 'cmp, ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2 option) -> ('k, 'v2, 'cmp) t) options

  val filter_mapi
    : ( 'k
      , 'cmp
      , ('k, 'v1, 'cmp) t -> f:(key:'k key -> data:'v1 -> 'v2 option) -> ('k, 'v2, 'cmp) t
      )
        options

  val partition_mapi
    : ( 'k
      , 'cmp
      , ('k, 'v1, 'cmp) t
      -> f:(key:'k key -> data:'v1 -> ('v2, 'v3) Either.t)
      -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t )
        options

  val partition_map
    : ( 'k
      , 'cmp
      , ('k, 'v1, 'cmp) t
      -> f:('v1 -> ('v2, 'v3) Either.t)
      -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t )
        options

  val partitioni_tf
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> f:(key:'k key -> data:'v -> bool)
      -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t )
        options

  val partition_tf
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t -> f:('v -> bool) -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t )
        options

  val combine_errors
    : ('k, 'cmp, ('k, 'v Or_error.t, 'cmp) t -> ('k, 'v, 'cmp) t Or_error.t) options

  val compare_direct
    : ('k, 'cmp, ('v -> 'v -> int) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> int) options

  val equal
    : ( 'k
      , 'cmp
      , ('v -> 'v -> bool) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool )
        options

  val keys : ('k, _, _) t -> 'k key list
  val data : (_, 'v, _) t -> 'v list

  val to_alist
    :  ?key_order:[ `Increasing | `Decreasing ]
    -> ('k, 'v, _) t
    -> ('k key * 'v) list

  val merge
    : ( 'k
      , 'cmp
      , ('k, 'v1, 'cmp) t
      -> ('k, 'v2, 'cmp) t
      -> f:(key:'k key -> ('v1, 'v2) Merge_element.t -> 'v3 option)
      -> ('k, 'v3, 'cmp) t )
        options

  val merge_skewed
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> ('k, 'v, 'cmp) t
      -> combine:(key:'k key -> 'v -> 'v -> 'v)
      -> ('k, 'v, 'cmp) t )
        options

  val symmetric_diff
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> ('k, 'v, 'cmp) t
      -> data_equal:('v -> 'v -> bool)
      -> ('k key, 'v) Symmetric_diff_element.t Sequence.t )
        options

  val fold_symmetric_diff
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> ('k, 'v, 'cmp) t
      -> data_equal:('v -> 'v -> bool)
      -> init:'a
      -> f:('a -> ('k key, 'v) Symmetric_diff_element.t -> 'a)
      -> 'a )
        options

  val min_elt : ('k, 'v, _) t -> ('k key * 'v) option
  val min_elt_exn : ('k, 'v, _) t -> 'k key * 'v
  val max_elt : ('k, 'v, _) t -> ('k key * 'v) option
  val max_elt_exn : ('k, 'v, _) t -> 'k key * 'v
  val for_all : ('k, 'v, _) t -> f:('v -> bool) -> bool
  val for_alli : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> bool) -> bool
  val exists : ('k, 'v, _) t -> f:('v -> bool) -> bool
  val existsi : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> bool) -> bool
  val count : ('k, 'v, _) t -> f:('v -> bool) -> int
  val counti : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> bool) -> int

  val split
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> 'k key
      -> ('k, 'v, 'cmp) t * ('k key * 'v) option * ('k, 'v, 'cmp) t )
        options

  val append
    : ( 'k
      , 'cmp
      , lower_part:('k, 'v, 'cmp) t
      -> upper_part:('k, 'v, 'cmp) t
      -> [ `Ok of ('k, 'v, 'cmp) t | `Overlapping_key_ranges ] )
        options

  val subrange
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> lower_bound:'k key Maybe_bound.t
      -> upper_bound:'k key Maybe_bound.t
      -> ('k, 'v, 'cmp) t )
        options

  val fold_range_inclusive
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> min:'k key
      -> max:'k key
      -> init:'a
      -> f:(key:'k key -> data:'v -> 'a -> 'a)
      -> 'a )
        options

  val range_to_alist
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t -> min:'k key -> max:'k key -> ('k key * 'v) list )
        options

  val closest_key
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
      -> 'k key
      -> ('k key * 'v) option )
        options

  val nth : ('k, 'v, 'cmp) t -> int -> ('k key * 'v) option
  val nth_exn : ('k, 'v, 'cmp) t -> int -> 'k key * 'v
  val rank : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> int option) options
  val to_tree : ('k, 'v, 'cmp) t -> ('k key, 'v, 'cmp) tree

  val to_sequence
    : ( 'k
      , 'cmp
      , ?order:[ `Increasing_key | `Decreasing_key ]
      -> ?keys_greater_or_equal_to:'k key
      -> ?keys_less_or_equal_to:'k key
      -> ('k, 'v, 'cmp) t
      -> ('k key * 'v) Sequence.t )
        options

  val binary_search
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> compare:(key:'k key -> data:'v -> 'key -> int)
      -> Binary_searchable.Which_target_by_key.t
      -> 'key
      -> ('k key * 'v) option )
        options

  val binary_search_segmented
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> segment_of:(key:'k key -> data:'v -> [ `Left | `Right ])
      -> Binary_searchable.Which_target_by_segment.t
      -> ('k key * 'v) option )
        options

  val binary_search_subrange
    : ( 'k
      , 'cmp
      , ('k, 'v, 'cmp) t
      -> compare:(key:'k key -> data:'v -> 'bound -> int)
      -> lower_bound:'bound Maybe_bound.t
      -> upper_bound:'bound Maybe_bound.t
      -> ('k, 'v, 'cmp) t )
        options
end

module type Accessors1 = sig
  type 'a t
  type 'a tree
  type key
  type comparator_witness

  val invariants : _ t -> bool
  val is_empty : _ t -> bool
  val length : _ t -> int
  val add : 'a t -> key:key -> data:'a -> 'a t Or_duplicate.t
  val add_exn : 'a t -> key:key -> data:'a -> 'a t
  val set : 'a t -> key:key -> data:'a -> 'a t
  val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
  val remove_multi : 'a list t -> key -> 'a list t
  val find_multi : 'a list t -> key -> 'a list
  val change : 'a t -> key -> f:('a option -> 'a option) -> 'a t
  val update : 'a t -> key -> f:('a option -> 'a) -> 'a t
  val find : 'a t -> key -> 'a option
  val find_exn : 'a t -> key -> 'a
  val remove : 'a t -> key -> 'a t
  val mem : _ t -> key -> bool
  val iter_keys : _ t -> f:(key -> unit) -> unit
  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(key:key -> data:'a -> unit) -> unit

  val iteri_until
    :  'a t
    -> f:(key:key -> data:'a -> Continue_or_stop.t)
    -> Finished_or_unfinished.t

  val iter2 : 'a t -> 'b t -> f:(key:key -> data:('a, 'b) Merge_element.t -> unit) -> unit
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(key:key -> data:'a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b

  val fold_until
    :  'a t
    -> init:'acc
    -> f:(key:key -> data:'a -> 'acc -> ('acc, 'final) Container.Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final

  val fold_right : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b

  val fold2
    :  'a t
    -> 'b t
    -> init:'c
    -> f:(key:key -> data:('a, 'b) Merge_element.t -> 'c -> 'c)
    -> 'c

  val filter_keys : 'a t -> f:(key -> bool) -> 'a t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filteri : 'a t -> f:(key:key -> data:'a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val filter_mapi : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t
  val partition_mapi : 'a t -> f:(key:key -> data:'a -> ('b, 'c) Either.t) -> 'b t * 'c t
  val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t
  val partitioni_tf : 'a t -> f:(key:key -> data:'a -> bool) -> 'a t * 'a t
  val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
  val combine_errors : 'a Or_error.t t -> 'a t Or_error.t
  val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val keys : _ t -> key list
  val data : 'a t -> 'a list
  val to_alist : ?key_order:[ `Increasing | `Decreasing ] -> 'a t -> (key * 'a) list
  val merge : 'a t -> 'b t -> f:(key:key -> ('a, 'b) Merge_element.t -> 'c option) -> 'c t
  val merge_skewed : 'v t -> 'v t -> combine:(key:key -> 'v -> 'v -> 'v) -> 'v t

  val symmetric_diff
    :  'a t
    -> 'a t
    -> data_equal:('a -> 'a -> bool)
    -> (key, 'a) Symmetric_diff_element.t Sequence.t

  val fold_symmetric_diff
    :  'a t
    -> 'a t
    -> data_equal:('a -> 'a -> bool)
    -> init:'c
    -> f:('c -> (key, 'a) Symmetric_diff_element.t -> 'c)
    -> 'c

  val min_elt : 'a t -> (key * 'a) option
  val min_elt_exn : 'a t -> key * 'a
  val max_elt : 'a t -> (key * 'a) option
  val max_elt_exn : 'a t -> key * 'a
  val for_all : 'a t -> f:('a -> bool) -> bool
  val for_alli : 'a t -> f:(key:key -> data:'a -> bool) -> bool
  val exists : 'a t -> f:('a -> bool) -> bool
  val existsi : 'a t -> f:(key:key -> data:'a -> bool) -> bool
  val count : 'a t -> f:('a -> bool) -> int
  val counti : 'a t -> f:(key:key -> data:'a -> bool) -> int
  val split : 'a t -> key -> 'a t * (key * 'a) option * 'a t

  val append
    :  lower_part:'a t
    -> upper_part:'a t
    -> [ `Ok of 'a t | `Overlapping_key_ranges ]

  val subrange
    :  'a t
    -> lower_bound:key Maybe_bound.t
    -> upper_bound:key Maybe_bound.t
    -> 'a t

  val fold_range_inclusive
    :  'a t
    -> min:key
    -> max:key
    -> init:'b
    -> f:(key:key -> data:'a -> 'b -> 'b)
    -> 'b

  val range_to_alist : 'a t -> min:key -> max:key -> (key * 'a) list

  val closest_key
    :  'a t
    -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
    -> key
    -> (key * 'a) option

  val nth : 'a t -> int -> (key * 'a) option
  val nth_exn : 'a t -> int -> key * 'a
  val rank : _ t -> key -> int option
  val to_tree : 'a t -> 'a tree

  val to_sequence
    :  ?order:[ `Increasing_key | `Decreasing_key ]
    -> ?keys_greater_or_equal_to:key
    -> ?keys_less_or_equal_to:key
    -> 'a t
    -> (key * 'a) Sequence.t

  val binary_search
    :  'a t
    -> compare:(key:key -> data:'a -> 'key -> int)
    -> Binary_searchable.Which_target_by_key.t
    -> 'key
    -> (key * 'a) option

  val binary_search_segmented
    :  'a t
    -> segment_of:(key:key -> data:'a -> [ `Left | `Right ])
    -> Binary_searchable.Which_target_by_segment.t
    -> (key * 'a) option

  val binary_search_subrange
    :  'a t
    -> compare:(key:key -> data:'a -> 'bound -> int)
    -> lower_bound:'bound Maybe_bound.t
    -> upper_bound:'bound Maybe_bound.t
    -> 'a t
end

module type Accessors2 = sig
  type ('a, 'b) t
  type ('a, 'b) tree
  type comparator_witness

  val invariants : (_, _) t -> bool
  val is_empty : (_, _) t -> bool
  val length : (_, _) t -> int
  val add : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t Or_duplicate.t
  val add_exn : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t
  val set : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t
  val add_multi : ('a, 'b list) t -> key:'a -> data:'b -> ('a, 'b list) t
  val remove_multi : ('a, 'b list) t -> 'a -> ('a, 'b list) t
  val find_multi : ('a, 'b list) t -> 'a -> 'b list
  val change : ('a, 'b) t -> 'a -> f:('b option -> 'b option) -> ('a, 'b) t
  val update : ('a, 'b) t -> 'a -> f:('b option -> 'b) -> ('a, 'b) t
  val find : ('a, 'b) t -> 'a -> 'b option
  val find_exn : ('a, 'b) t -> 'a -> 'b
  val remove : ('a, 'b) t -> 'a -> ('a, 'b) t
  val mem : ('a, 'b) t -> 'a -> bool
  val iter_keys : ('a, _) t -> f:('a -> unit) -> unit
  val iter : (_, 'b) t -> f:('b -> unit) -> unit
  val iteri : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit

  val iteri_until
    :  ('a, 'b) t
    -> f:(key:'a -> data:'b -> Continue_or_stop.t)
    -> Finished_or_unfinished.t

  val iter2
    :  ('a, 'b) t
    -> ('a, 'c) t
    -> f:(key:'a -> data:('b, 'c) Merge_element.t -> unit)
    -> unit

  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  val mapi : ('a, 'b) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c) t
  val fold : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val fold_until
    :  ('k, 'v) t
    -> init:'a
    -> f:(key:'k -> data:'v -> 'a -> ('a, 'final) Container.Continue_or_stop.t)
    -> finish:('a -> 'final)
    -> 'final

  val fold_right : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val fold2
    :  ('a, 'b) t
    -> ('a, 'c) t
    -> init:'d
    -> f:(key:'a -> data:('b, 'c) Merge_element.t -> 'd -> 'd)
    -> 'd

  val filter_keys : ('a, 'b) t -> f:('a -> bool) -> ('a, 'b) t
  val filter : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t
  val filteri : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b) t
  val filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  val filter_mapi : ('a, 'b) t -> f:(key:'a -> data:'b -> 'c option) -> ('a, 'c) t

  val partition_mapi
    :  ('a, 'b) t
    -> f:(key:'a -> data:'b -> ('c, 'd) Either.t)
    -> ('a, 'c) t * ('a, 'd) t

  val partition_map : ('a, 'b) t -> f:('b -> ('c, 'd) Either.t) -> ('a, 'c) t * ('a, 'd) t

  val partitioni_tf
    :  ('a, 'b) t
    -> f:(key:'a -> data:'b -> bool)
    -> ('a, 'b) t * ('a, 'b) t

  val partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t
  val combine_errors : ('a, 'b Or_error.t) t -> ('a, 'b) t Or_error.t
  val compare_direct : ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  val keys : ('a, _) t -> 'a list
  val data : (_, 'b) t -> 'b list
  val to_alist : ?key_order:[ `Increasing | `Decreasing ] -> ('a, 'b) t -> ('a * 'b) list

  val merge
    :  ('a, 'b) t
    -> ('a, 'c) t
    -> f:(key:'a -> ('b, 'c) Merge_element.t -> 'd option)
    -> ('a, 'd) t

  val merge_skewed
    :  ('k, 'v) t
    -> ('k, 'v) t
    -> combine:(key:'k -> 'v -> 'v -> 'v)
    -> ('k, 'v) t

  val symmetric_diff
    :  ('a, 'b) t
    -> ('a, 'b) t
    -> data_equal:('b -> 'b -> bool)
    -> ('a, 'b) Symmetric_diff_element.t Sequence.t

  val fold_symmetric_diff
    :  ('a, 'b) t
    -> ('a, 'b) t
    -> data_equal:('b -> 'b -> bool)
    -> init:'c
    -> f:('c -> ('a, 'b) Symmetric_diff_element.t -> 'c)
    -> 'c

  val min_elt : ('a, 'b) t -> ('a * 'b) option
  val min_elt_exn : ('a, 'b) t -> 'a * 'b
  val max_elt : ('a, 'b) t -> ('a * 'b) option
  val max_elt_exn : ('a, 'b) t -> 'a * 'b
  val for_all : (_, 'b) t -> f:('b -> bool) -> bool
  val for_alli : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> bool
  val exists : (_, 'b) t -> f:('b -> bool) -> bool
  val existsi : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> bool
  val count : (_, 'b) t -> f:('b -> bool) -> int
  val counti : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> int
  val split : ('a, 'b) t -> 'a -> ('a, 'b) t * ('a * 'b) option * ('a, 'b) t

  val append
    :  lower_part:('a, 'b) t
    -> upper_part:('a, 'b) t
    -> [ `Ok of ('a, 'b) t | `Overlapping_key_ranges ]

  val subrange
    :  ('a, 'b) t
    -> lower_bound:'a Maybe_bound.t
    -> upper_bound:'a Maybe_bound.t
    -> ('a, 'b) t

  val fold_range_inclusive
    :  ('a, 'b) t
    -> min:'a
    -> max:'a
    -> init:'c
    -> f:(key:'a -> data:'b -> 'c -> 'c)
    -> 'c

  val range_to_alist : ('a, 'b) t -> min:'a -> max:'a -> ('a * 'b) list

  val closest_key
    :  ('a, 'b) t
    -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
    -> 'a
    -> ('a * 'b) option

  val nth : ('a, 'b) t -> int -> ('a * 'b) option
  val nth_exn : ('a, 'b) t -> int -> 'a * 'b
  val rank : ('a, _) t -> 'a -> int option
  val to_tree : ('a, 'b) t -> ('a, 'b) tree

  val to_sequence
    :  ?order:[ `Increasing_key | `Decreasing_key ]
    -> ?keys_greater_or_equal_to:'a
    -> ?keys_less_or_equal_to:'a
    -> ('a, 'b) t
    -> ('a * 'b) Sequence.t

  val binary_search
    :  ('k, 'v) t
    -> compare:(key:'k -> data:'v -> 'key -> int)
    -> Binary_searchable.Which_target_by_key.t
    -> 'key
    -> ('k * 'v) option

  val binary_search_segmented
    :  ('k, 'v) t
    -> segment_of:(key:'k -> data:'v -> [ `Left | `Right ])
    -> Binary_searchable.Which_target_by_segment.t
    -> ('k * 'v) option

  val binary_search_subrange
    :  ('k, 'v) t
    -> compare:(key:'k -> data:'v -> 'bound -> int)
    -> lower_bound:'bound Maybe_bound.t
    -> upper_bound:'bound Maybe_bound.t
    -> ('k, 'v) t
end

module type Accessors3 = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree

  val invariants : (_, _, _) t -> bool
  val is_empty : (_, _, _) t -> bool
  val length : (_, _, _) t -> int
  val add : ('a, 'b, 'cmp) t -> key:'a -> data:'b -> ('a, 'b, 'cmp) t Or_duplicate.t
  val add_exn : ('a, 'b, 'cmp) t -> key:'a -> data:'b -> ('a, 'b, 'cmp) t
  val set : ('a, 'b, 'cmp) t -> key:'a -> data:'b -> ('a, 'b, 'cmp) t
  val add_multi : ('a, 'b list, 'cmp) t -> key:'a -> data:'b -> ('a, 'b list, 'cmp) t
  val remove_multi : ('a, 'b list, 'cmp) t -> 'a -> ('a, 'b list, 'cmp) t
  val find_multi : ('a, 'b list, 'cmp) t -> 'a -> 'b list
  val change : ('a, 'b, 'cmp) t -> 'a -> f:('b option -> 'b option) -> ('a, 'b, 'cmp) t
  val update : ('a, 'b, 'cmp) t -> 'a -> f:('b option -> 'b) -> ('a, 'b, 'cmp) t
  val find : ('a, 'b, 'cmp) t -> 'a -> 'b option
  val find_exn : ('a, 'b, 'cmp) t -> 'a -> 'b
  val remove : ('a, 'b, 'cmp) t -> 'a -> ('a, 'b, 'cmp) t
  val mem : ('a, 'b, 'cmp) t -> 'a -> bool
  val iter_keys : ('a, _, 'cmp) t -> f:('a -> unit) -> unit
  val iter : (_, 'b, 'cmp) t -> f:('b -> unit) -> unit
  val iteri : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> unit) -> unit

  val iteri_until
    :  ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> Continue_or_stop.t)
    -> Finished_or_unfinished.t

  val iter2
    :  ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> data:('b, 'c) Merge_element.t -> unit)
    -> unit

  val map : ('a, 'b, 'cmp) t -> f:('b -> 'c) -> ('a, 'c, 'cmp) t
  val mapi : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c, 'cmp) t
  val fold : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val fold_until
    :  ('k, 'v, _) t
    -> init:'a
    -> f:(key:'k -> data:'v -> 'a -> ('a, 'final) Container.Continue_or_stop.t)
    -> finish:('a -> 'final)
    -> 'final

  val fold_right : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val fold2
    :  ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> init:'d
    -> f:(key:'a -> data:('b, 'c) Merge_element.t -> 'd -> 'd)
    -> 'd

  val filter_keys : ('a, 'b, 'cmp) t -> f:('a -> bool) -> ('a, 'b, 'cmp) t
  val filter : ('a, 'b, 'cmp) t -> f:('b -> bool) -> ('a, 'b, 'cmp) t
  val filteri : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b, 'cmp) t
  val filter_map : ('a, 'b, 'cmp) t -> f:('b -> 'c option) -> ('a, 'c, 'cmp) t

  val filter_mapi
    :  ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> 'c option)
    -> ('a, 'c, 'cmp) t

  val partition_mapi
    :  ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> ('c, 'd) Either.t)
    -> ('a, 'c, 'cmp) t * ('a, 'd, 'cmp) t

  val partition_map
    :  ('a, 'b, 'cmp) t
    -> f:('b -> ('c, 'd) Either.t)
    -> ('a, 'c, 'cmp) t * ('a, 'd, 'cmp) t

  val partitioni_tf
    :  ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> bool)
    -> ('a, 'b, 'cmp) t * ('a, 'b, 'cmp) t

  val partition_tf
    :  ('a, 'b, 'cmp) t
    -> f:('b -> bool)
    -> ('a, 'b, 'cmp) t * ('a, 'b, 'cmp) t

  val combine_errors : ('a, 'b Or_error.t, 'cmp) t -> ('a, 'b, 'cmp) t Or_error.t
  val compare_direct : ('b -> 'b -> int) -> ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) t -> int
  val equal : ('b -> 'b -> bool) -> ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) t -> bool
  val keys : ('a, _, _) t -> 'a list
  val data : (_, 'b, _) t -> 'b list

  val to_alist
    :  ?key_order:[ `Increasing | `Decreasing ]
    -> ('a, 'b, _) t
    -> ('a * 'b) list

  val merge
    :  ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> ('b, 'c) Merge_element.t -> 'd option)
    -> ('a, 'd, 'cmp) t

  val merge_skewed
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> combine:(key:'k -> 'v -> 'v -> 'v)
    -> ('k, 'v, 'cmp) t

  val symmetric_diff
    :  ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> data_equal:('b -> 'b -> bool)
    -> ('a, 'b) Symmetric_diff_element.t Sequence.t

  val fold_symmetric_diff
    :  ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> data_equal:('b -> 'b -> bool)
    -> init:'c
    -> f:('c -> ('a, 'b) Symmetric_diff_element.t -> 'c)
    -> 'c

  val min_elt : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val min_elt_exn : ('a, 'b, 'cmp) t -> 'a * 'b
  val max_elt : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val max_elt_exn : ('a, 'b, 'cmp) t -> 'a * 'b
  val for_all : (_, 'b, _) t -> f:('b -> bool) -> bool
  val for_alli : ('a, 'b, _) t -> f:(key:'a -> data:'b -> bool) -> bool
  val exists : (_, 'b, _) t -> f:('b -> bool) -> bool
  val existsi : ('a, 'b, _) t -> f:(key:'a -> data:'b -> bool) -> bool
  val count : (_, 'b, _) t -> f:('b -> bool) -> int
  val counti : ('a, 'b, _) t -> f:(key:'a -> data:'b -> bool) -> int

  val split
    :  ('k, 'v, 'cmp) t
    -> 'k
    -> ('k, 'v, 'cmp) t * ('k * 'v) option * ('k, 'v, 'cmp) t

  val append
    :  lower_part:('k, 'v, 'cmp) t
    -> upper_part:('k, 'v, 'cmp) t
    -> [ `Ok of ('k, 'v, 'cmp) t | `Overlapping_key_ranges ]

  val subrange
    :  ('k, 'v, 'cmp) t
    -> lower_bound:'k Maybe_bound.t
    -> upper_bound:'k Maybe_bound.t
    -> ('k, 'v, 'cmp) t

  val fold_range_inclusive
    :  ('a, 'b, _) t
    -> min:'a
    -> max:'a
    -> init:'c
    -> f:(key:'a -> data:'b -> 'c -> 'c)
    -> 'c

  val range_to_alist : ('a, 'b, _) t -> min:'a -> max:'a -> ('a * 'b) list

  val closest_key
    :  ('a, 'b, _) t
    -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
    -> 'a
    -> ('a * 'b) option

  val nth : ('a, 'b, _) t -> int -> ('a * 'b) option
  val nth_exn : ('a, 'b, _) t -> int -> 'a * 'b
  val rank : ('a, _, _) t -> 'a -> int option
  val to_tree : ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) tree

  val to_sequence
    :  ?order:[ `Increasing_key | `Decreasing_key ]
    -> ?keys_greater_or_equal_to:'a
    -> ?keys_less_or_equal_to:'a
    -> ('a, 'b, _) t
    -> ('a * 'b) Sequence.t

  val binary_search
    :  ('k, 'v, _) t
    -> compare:(key:'k -> data:'v -> 'key -> int)
    -> Binary_searchable.Which_target_by_key.t
    -> 'key
    -> ('k * 'v) option

  val binary_search_segmented
    :  ('k, 'v, _) t
    -> segment_of:(key:'k -> data:'v -> [ `Left | `Right ])
    -> Binary_searchable.Which_target_by_segment.t
    -> ('k * 'v) option

  val binary_search_subrange
    :  ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'bound -> int)
    -> lower_bound:'bound Maybe_bound.t
    -> upper_bound:'bound Maybe_bound.t
    -> ('k, 'v, 'cmp) t
end

module type Accessors3_with_comparator = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree

  val invariants : comparator:('a, 'cmp) Comparator.t -> ('a, 'b, 'cmp) t -> bool
  val is_empty : ('a, 'b, 'cmp) t -> bool
  val length : ('a, 'b, 'cmp) t -> int

  val add
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> key:'a
    -> data:'b
    -> ('a, 'b, 'cmp) t Or_duplicate.t

  val add_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> key:'a
    -> data:'b
    -> ('a, 'b, 'cmp) t

  val set
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> key:'a
    -> data:'b
    -> ('a, 'b, 'cmp) t

  val add_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b list, 'cmp) t
    -> key:'a
    -> data:'b
    -> ('a, 'b list, 'cmp) t

  val remove_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b list, 'cmp) t
    -> 'a
    -> ('a, 'b list, 'cmp) t

  val find_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b list, 'cmp) t
    -> 'a
    -> 'b list

  val change
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> 'a
    -> f:('b option -> 'b option)
    -> ('a, 'b, 'cmp) t

  val update
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> 'a
    -> f:('b option -> 'b)
    -> ('a, 'b, 'cmp) t

  val find : comparator:('a, 'cmp) Comparator.t -> ('a, 'b, 'cmp) t -> 'a -> 'b option
  val find_exn : comparator:('a, 'cmp) Comparator.t -> ('a, 'b, 'cmp) t -> 'a -> 'b

  val remove
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> 'a
    -> ('a, 'b, 'cmp) t

  val mem : comparator:('a, 'cmp) Comparator.t -> ('a, 'b, 'cmp) t -> 'a -> bool
  val iter_keys : ('a, _, 'cmp) t -> f:('a -> unit) -> unit
  val iter : (_, 'b, 'cmp) t -> f:('b -> unit) -> unit
  val iteri : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> unit) -> unit

  val iteri_until
    :  ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> Continue_or_stop.t)
    -> Finished_or_unfinished.t

  val iter2
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> data:('b, 'c) Merge_element.t -> unit)
    -> unit

  val map : ('a, 'b, 'cmp) t -> f:('b -> 'c) -> ('a, 'c, 'cmp) t
  val mapi : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c, 'cmp) t
  val fold : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val fold_until
    :  ('k, 'v, _) t
    -> init:'a
    -> f:(key:'k -> data:'v -> 'a -> ('a, 'final) Container.Continue_or_stop.t)
    -> finish:('a -> 'final)
    -> 'final

  val fold_right : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val fold2
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> init:'d
    -> f:(key:'a -> data:('b, 'c) Merge_element.t -> 'd -> 'd)
    -> 'd

  val filter_keys
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:('a -> bool)
    -> ('a, 'b, 'cmp) t

  val filter
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:('b -> bool)
    -> ('a, 'b, 'cmp) t

  val filteri
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> bool)
    -> ('a, 'b, 'cmp) t

  val filter_map
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:('b -> 'c option)
    -> ('a, 'c, 'cmp) t

  val filter_mapi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> 'c option)
    -> ('a, 'c, 'cmp) t

  val partition_mapi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> ('c, 'd) Either.t)
    -> ('a, 'c, 'cmp) t * ('a, 'd, 'cmp) t

  val partition_map
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:('b -> ('c, 'd) Either.t)
    -> ('a, 'c, 'cmp) t * ('a, 'd, 'cmp) t

  val partitioni_tf
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:(key:'a -> data:'b -> bool)
    -> ('a, 'b, 'cmp) t * ('a, 'b, 'cmp) t

  val partition_tf
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> f:('b -> bool)
    -> ('a, 'b, 'cmp) t * ('a, 'b, 'cmp) t

  val combine_errors
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b Or_error.t, 'cmp) t
    -> ('a, 'b, 'cmp) t Or_error.t

  val compare_direct
    :  comparator:('a, 'cmp) Comparator.t
    -> ('b -> 'b -> int)
    -> ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> int

  val equal
    :  comparator:('a, 'cmp) Comparator.t
    -> ('b -> 'b -> bool)
    -> ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> bool

  val keys : ('a, _, _) t -> 'a list
  val data : (_, 'b, _) t -> 'b list

  val to_alist
    :  ?key_order:[ `Increasing | `Decreasing ]
    -> ('a, 'b, _) t
    -> ('a * 'b) list

  val merge
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> ('b, 'c) Merge_element.t -> 'd option)
    -> ('a, 'd, 'cmp) t

  val merge_skewed
    :  comparator:('k, 'cmp) Comparator.t
    -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> combine:(key:'k -> 'v -> 'v -> 'v)
    -> ('k, 'v, 'cmp) t

  val symmetric_diff
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> data_equal:('b -> 'b -> bool)
    -> ('a, 'b) Symmetric_diff_element.t Sequence.t

  val fold_symmetric_diff
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> data_equal:('b -> 'b -> bool)
    -> init:'c
    -> f:('c -> ('a, 'b) Symmetric_diff_element.t -> 'c)
    -> 'c

  val min_elt : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val min_elt_exn : ('a, 'b, 'cmp) t -> 'a * 'b
  val max_elt : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val max_elt_exn : ('a, 'b, 'cmp) t -> 'a * 'b
  val for_all : ('a, 'b, 'cmp) t -> f:('b -> bool) -> bool
  val for_alli : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> bool) -> bool
  val exists : ('a, 'b, 'cmp) t -> f:('b -> bool) -> bool
  val existsi : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> bool) -> bool
  val count : ('a, 'b, 'cmp) t -> f:('b -> bool) -> int
  val counti : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> bool) -> int

  val split
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> 'a
    -> ('a, 'b, 'cmp) t * ('a * 'b) option * ('a, 'b, 'cmp) t

  val append
    :  comparator:('a, 'cmp) Comparator.t
    -> lower_part:('a, 'b, 'cmp) t
    -> upper_part:('a, 'b, 'cmp) t
    -> [ `Ok of ('a, 'b, 'cmp) t | `Overlapping_key_ranges ]

  val subrange
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> lower_bound:'a Maybe_bound.t
    -> upper_bound:'a Maybe_bound.t
    -> ('a, 'b, 'cmp) t

  val fold_range_inclusive
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> min:'a
    -> max:'a
    -> init:'c
    -> f:(key:'a -> data:'b -> 'c -> 'c)
    -> 'c

  val range_to_alist
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> min:'a
    -> max:'a
    -> ('a * 'b) list

  val closest_key
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
    -> 'a
    -> ('a * 'b) option

  val nth : ('a, 'b, 'cmp) t -> int -> ('a * 'b) option
  val nth_exn : ('a, 'b, 'cmp) t -> int -> 'a * 'b
  val rank : comparator:('a, 'cmp) Comparator.t -> ('a, 'b, 'cmp) t -> 'a -> int option
  val to_tree : ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) tree

  val to_sequence
    :  comparator:('a, 'cmp) Comparator.t
    -> ?order:[ `Increasing_key | `Decreasing_key ]
    -> ?keys_greater_or_equal_to:'a
    -> ?keys_less_or_equal_to:'a
    -> ('a, 'b, 'cmp) t
    -> ('a * 'b) Sequence.t

  val binary_search
    :  comparator:('k, 'cmp) Comparator.t
    -> ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'key -> int)
    -> Binary_searchable.Which_target_by_key.t
    -> 'key
    -> ('k * 'v) option

  val binary_search_segmented
    :  comparator:('k, 'cmp) Comparator.t
    -> ('k, 'v, 'cmp) t
    -> segment_of:(key:'k -> data:'v -> [ `Left | `Right ])
    -> Binary_searchable.Which_target_by_segment.t
    -> ('k * 'v) option

  val binary_search_subrange
    :  comparator:('k, 'cmp) Comparator.t
    -> ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'bound -> int)
    -> lower_bound:'bound Maybe_bound.t
    -> upper_bound:'bound Maybe_bound.t
    -> ('k, 'v, 'cmp) t
end

(** Consistency checks (same as in [Container]). *)
module Check_accessors
    (T : T3)
    (Tree : T3)
    (Key : T1)
    (Cmp : T1)
    (Options : T3)
    (_ : Accessors_generic
     with type ('a, 'b, 'c) options := ('a, 'b, 'c) Options.t
     with type ('a, 'b, 'c) t := ('a, 'b, 'c) T.t
     with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
     with type 'a key := 'a Key.t
     with type 'cmp cmp := 'cmp Cmp.t) =
struct end

module Check_accessors1 (M : Accessors1) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = 'b M.t
    end)
    (struct
      type ('a, 'b, 'c) t = 'b M.tree
    end)
    (struct
      type 'a t = M.key
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_accessors2 (M : Accessors2) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_accessors3 (M : Accessors3) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = 'a
    end)
    (Without_comparator)
    (M)

module Check_accessors3_with_comparator (M : Accessors3_with_comparator) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = 'a
    end)
    (With_comparator)
    (M)

module type Creators_generic = sig
  type ('k, 'v, 'cmp) t
  type ('k, 'v, 'cmp) tree
  type 'k key
  type ('a, 'cmp, 'z) options
  type 'cmp cmp

  val empty : ('k, 'cmp, ('k, _, 'cmp) t) options
  val singleton : ('k, 'cmp, 'k key -> 'v -> ('k, 'v, 'cmp) t) options

  val map_keys
    : ( 'k2
      , 'cmp2
      , ('k1, 'v, 'cmp1) t
      -> f:('k1 key -> 'k2 key)
      -> [ `Ok of ('k2, 'v, 'cmp2) t | `Duplicate_key of 'k2 key ] )
        options

  val map_keys_exn
    : ( 'k2
      , 'cmp2
      , ('k1, 'v, 'cmp1) t -> f:('k1 key -> 'k2 key) -> ('k2, 'v, 'cmp2) t )
        options

  val of_sorted_array
    : ('k, 'cmp, ('k key * 'v) array -> ('k, 'v, 'cmp) t Or_error.t) options

  val of_sorted_array_unchecked
    : ('k, 'cmp, ('k key * 'v) array -> ('k, 'v, 'cmp) t) options

  val of_increasing_iterator_unchecked
    : ('k, 'cmp, len:int -> f:(int -> 'k key * 'v) -> ('k, 'v, 'cmp) t) options

  val of_alist
    : ( 'k
      , 'cmp
      , ('k key * 'v) list -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k key ] )
        options

  val of_alist_or_error
    : ('k, 'cmp, ('k key * 'v) list -> ('k, 'v, 'cmp) t Or_error.t) options

  val of_alist_exn : ('k, 'cmp, ('k key * 'v) list -> ('k, 'v, 'cmp) t) options
  val of_alist_multi : ('k, 'cmp, ('k key * 'v) list -> ('k, 'v list, 'cmp) t) options

  val of_alist_fold
    : ( 'k
      , 'cmp
      , ('k key * 'v1) list -> init:'v2 -> f:('v2 -> 'v1 -> 'v2) -> ('k, 'v2, 'cmp) t )
        options

  val of_alist_reduce
    : ('k, 'cmp, ('k key * 'v) list -> f:('v -> 'v -> 'v) -> ('k, 'v, 'cmp) t) options

  val of_increasing_sequence
    : ('k, 'cmp, ('k key * 'v) Sequence.t -> ('k, 'v, 'cmp) t Or_error.t) options

  val of_sequence
    : ( 'k
      , 'cmp
      , ('k key * 'v) Sequence.t -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k key ]
      )
        options

  val of_sequence_or_error
    : ('k, 'cmp, ('k key * 'v) Sequence.t -> ('k, 'v, 'cmp) t Or_error.t) options

  val of_sequence_exn : ('k, 'cmp, ('k key * 'v) Sequence.t -> ('k, 'v, 'cmp) t) options

  val of_sequence_multi
    : ('k, 'cmp, ('k key * 'v) Sequence.t -> ('k, 'v list, 'cmp) t) options

  val of_sequence_fold
    : ( 'k
      , 'cmp
      , ('k key * 'v1) Sequence.t
      -> init:'v2
      -> f:('v2 -> 'v1 -> 'v2)
      -> ('k, 'v2, 'cmp) t )
        options

  val of_sequence_reduce
    : ( 'k
      , 'cmp
      , ('k key * 'v) Sequence.t -> f:('v -> 'v -> 'v) -> ('k, 'v, 'cmp) t )
        options

  val of_iteri
    : ( 'k
      , 'cmp
      , iteri:(f:(key:'k key -> data:'v -> unit) -> unit)
      -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k key ] )
        options

  val of_iteri_exn
    : ( 'k
      , 'cmp
      , iteri:(f:(key:'k key -> data:'v -> unit) -> unit) -> ('k, 'v, 'cmp) t )
        options

  val of_tree : ('k, 'cmp, ('k key, 'v, 'cmp) tree -> ('k, 'v, 'cmp) t) options
end

module type Creators1 = sig
  type 'a t
  type 'a tree
  type key
  type comparator_witness

  val empty : _ t
  val singleton : key -> 'a -> 'a t
  val map_keys : 'v t -> f:(key -> key) -> [ `Ok of 'v t | `Duplicate_key of key ]
  val map_keys_exn : 'v t -> f:(key -> key) -> 'v t
  val of_alist : (key * 'a) list -> [ `Ok of 'a t | `Duplicate_key of key ]
  val of_alist_or_error : (key * 'a) list -> 'a t Or_error.t
  val of_alist_exn : (key * 'a) list -> 'a t
  val of_alist_multi : (key * 'a) list -> 'a list t
  val of_alist_fold : (key * 'a) list -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
  val of_alist_reduce : (key * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
  val of_sorted_array : (key * 'a) array -> 'a t Or_error.t
  val of_sorted_array_unchecked : (key * 'a) array -> 'a t
  val of_increasing_iterator_unchecked : len:int -> f:(int -> key * 'a) -> 'a t
  val of_increasing_sequence : (key * 'a) Sequence.t -> 'a t Or_error.t
  val of_sequence : (key * 'a) Sequence.t -> [ `Ok of 'a t | `Duplicate_key of key ]
  val of_sequence_or_error : (key * 'a) Sequence.t -> 'a t Or_error.t
  val of_sequence_exn : (key * 'a) Sequence.t -> 'a t
  val of_sequence_multi : (key * 'a) Sequence.t -> 'a list t
  val of_sequence_fold : (key * 'a) Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
  val of_sequence_reduce : (key * 'a) Sequence.t -> f:('a -> 'a -> 'a) -> 'a t

  val of_iteri
    :  iteri:(f:(key:key -> data:'v -> unit) -> unit)
    -> [ `Ok of 'v t | `Duplicate_key of key ]

  val of_iteri_exn : iteri:(f:(key:key -> data:'v -> unit) -> unit) -> 'v t
  val of_tree : 'a tree -> 'a t
end

module type Creators2 = sig
  type ('a, 'b) t
  type ('a, 'b) tree
  type comparator_witness

  val empty : (_, _) t
  val singleton : 'a -> 'b -> ('a, 'b) t

  val map_keys
    :  ('k1, 'v) t
    -> f:('k1 -> 'k2)
    -> [ `Ok of ('k2, 'v) t | `Duplicate_key of 'k2 ]

  val map_keys_exn : ('k1, 'v) t -> f:('k1 -> 'k2) -> ('k2, 'v) t
  val of_alist : ('a * 'b) list -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]
  val of_alist_or_error : ('a * 'b) list -> ('a, 'b) t Or_error.t
  val of_alist_exn : ('a * 'b) list -> ('a, 'b) t
  val of_alist_multi : ('a * 'b) list -> ('a, 'b list) t
  val of_alist_fold : ('a * 'b) list -> init:'c -> f:('c -> 'b -> 'c) -> ('a, 'c) t
  val of_alist_reduce : ('a * 'b) list -> f:('b -> 'b -> 'b) -> ('a, 'b) t
  val of_sorted_array : ('a * 'b) array -> ('a, 'b) t Or_error.t
  val of_sorted_array_unchecked : ('a * 'b) array -> ('a, 'b) t
  val of_increasing_iterator_unchecked : len:int -> f:(int -> 'a * 'b) -> ('a, 'b) t
  val of_increasing_sequence : ('a * 'b) Sequence.t -> ('a, 'b) t Or_error.t
  val of_sequence : ('a * 'b) Sequence.t -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]
  val of_sequence_or_error : ('a * 'b) Sequence.t -> ('a, 'b) t Or_error.t
  val of_sequence_exn : ('a * 'b) Sequence.t -> ('a, 'b) t
  val of_sequence_multi : ('a * 'b) Sequence.t -> ('a, 'b list) t

  val of_sequence_fold
    :  ('a * 'b) Sequence.t
    -> init:'c
    -> f:('c -> 'b -> 'c)
    -> ('a, 'c) t

  val of_sequence_reduce : ('a * 'b) Sequence.t -> f:('b -> 'b -> 'b) -> ('a, 'b) t

  val of_iteri
    :  iteri:(f:(key:'a -> data:'b -> unit) -> unit)
    -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]

  val of_iteri_exn : iteri:(f:(key:'a -> data:'b -> unit) -> unit) -> ('a, 'b) t
  val of_tree : ('a, 'b) tree -> ('a, 'b) t
end

module type Creators3_with_comparator = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree

  val empty : comparator:('a, 'cmp) Comparator.t -> ('a, _, 'cmp) t
  val singleton : comparator:('a, 'cmp) Comparator.t -> 'a -> 'b -> ('a, 'b, 'cmp) t

  val map_keys
    :  comparator:('k2, 'cmp2) Comparator.t
    -> ('k1, 'v, 'cmp1) t
    -> f:('k1 -> 'k2)
    -> [ `Ok of ('k2, 'v, 'cmp2) t | `Duplicate_key of 'k2 ]

  val map_keys_exn
    :  comparator:('k2, 'cmp2) Comparator.t
    -> ('k1, 'v, 'cmp1) t
    -> f:('k1 -> 'k2)
    -> ('k2, 'v, 'cmp2) t

  val of_alist
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  val of_alist_or_error
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list
    -> ('a, 'b, 'cmp) t Or_error.t

  val of_alist_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list
    -> ('a, 'b, 'cmp) t

  val of_alist_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list
    -> ('a, 'b list, 'cmp) t

  val of_alist_fold
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list
    -> init:'c
    -> f:('c -> 'b -> 'c)
    -> ('a, 'c, 'cmp) t

  val of_alist_reduce
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list
    -> f:('b -> 'b -> 'b)
    -> ('a, 'b, 'cmp) t

  val of_sorted_array
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) array
    -> ('a, 'b, 'cmp) t Or_error.t

  val of_sorted_array_unchecked
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) array
    -> ('a, 'b, 'cmp) t

  val of_increasing_iterator_unchecked
    :  comparator:('a, 'cmp) Comparator.t
    -> len:int
    -> f:(int -> 'a * 'b)
    -> ('a, 'b, 'cmp) t

  val of_increasing_sequence
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t Or_error.t

  val of_sequence
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  val of_sequence_or_error
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t Or_error.t

  val of_sequence_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t

  val of_sequence_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b list, 'cmp) t

  val of_sequence_fold
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> init:'c
    -> f:('c -> 'b -> 'c)
    -> ('a, 'c, 'cmp) t

  val of_sequence_reduce
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) Sequence.t
    -> f:('b -> 'b -> 'b)
    -> ('a, 'b, 'cmp) t

  val of_iteri
    :  comparator:('a, 'cmp) Comparator.t
    -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  val of_iteri_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
    -> ('a, 'b, 'cmp) t

  val of_tree
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) tree
    -> ('a, 'b, 'cmp) t
end

module Check_creators
    (T : T3)
    (Tree : T3)
    (Key : T1)
    (Cmp : T1)
    (Options : T3)
    (_ : Creators_generic
     with type ('a, 'b, 'c) options := ('a, 'b, 'c) Options.t
     with type ('a, 'b, 'c) t := ('a, 'b, 'c) T.t
     with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
     with type 'a key := 'a Key.t
     with type 'a cmp := 'a Cmp.t) =
struct end

module Check_creators1 (M : Creators1) =
  Check_creators
    (struct
      type ('a, 'b, 'c) t = 'b M.t
    end)
    (struct
      type ('a, 'b, 'c) t = 'b M.tree
    end)
    (struct
      type 'a t = M.key
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_creators2 (M : Creators2) =
  Check_creators
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_creators3_with_comparator (M : Creators3_with_comparator) =
  Check_creators
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = 'a
    end)
    (With_comparator)
    (M)

module type Creators_and_accessors_generic = sig
  include Creators_generic

  include
    Accessors_generic
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key
    with type 'a cmp := 'a cmp
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end

module type Creators_and_accessors1 = sig
  include Creators1

  include
    Accessors1
    with type 'a t := 'a t
    with type 'a tree := 'a tree
    with type key := key
    with type comparator_witness := comparator_witness
end

module type Creators_and_accessors2 = sig
  include Creators2

  include
    Accessors2
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type comparator_witness := comparator_witness
end

module type Creators_and_accessors3_with_comparator = sig
  include Creators3_with_comparator

  include
    Accessors3_with_comparator
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
end

module type S_poly = Creators_and_accessors2

module type For_deriving = sig
  type ('a, 'b, 'c) t

  module type Sexp_of_m = sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]
  end

  module type M_of_sexp = sig
    type t [@@deriving_inline of_sexp]

    val t_of_sexp : Sexplib0.Sexp.t -> t

    [@@@end]

    include Comparator.S with type t := t
  end

  module type M_sexp_grammar = sig
    type t [@@deriving_inline sexp_grammar]

    val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

    [@@@end]
  end

  module type Compare_m = sig end
  module type Equal_m = sig end
  module type Hash_fold_m = Hasher.S

  val sexp_of_m__t
    :  (module Sexp_of_m with type t = 'k)
    -> ('v -> Sexp.t)
    -> ('k, 'v, 'cmp) t
    -> Sexp.t

  val m__t_of_sexp
    :  (module M_of_sexp with type t = 'k and type comparator_witness = 'cmp)
    -> (Sexp.t -> 'v)
    -> Sexp.t
    -> ('k, 'v, 'cmp) t

  val m__t_sexp_grammar
    :  (module M_sexp_grammar with type t = 'k)
    -> 'v Sexplib0.Sexp_grammar.t
    -> ('k, 'v, 'cmp) t Sexplib0.Sexp_grammar.t

  val compare_m__t
    :  (module Compare_m)
    -> ('v -> 'v -> int)
    -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> int

  val equal_m__t
    :  (module Equal_m)
    -> ('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> bool

  val hash_fold_m__t
    :  (module Hash_fold_m with type t = 'k)
    -> (Hash.state -> 'v -> Hash.state)
    -> Hash.state
    -> ('k, 'v, _) t
    -> Hash.state
end

module type Map = sig
  (** [Map] is a functional data structure (balanced binary tree) implementing finite maps
      over a totally-ordered domain, called a "key". *)

  type ('key, +'value, 'cmp) t

  module Or_duplicate = Or_duplicate
  module Continue_or_stop = Continue_or_stop

  module Finished_or_unfinished : sig
    type t = Finished_or_unfinished.t =
      | Finished
      | Unfinished
    [@@deriving_inline compare, enumerate, equal, sexp_of]

    include Ppx_compare_lib.Comparable.S with type t := t
    include Ppx_enumerate_lib.Enumerable.S with type t := t
    include Ppx_compare_lib.Equal.S with type t := t

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]

    (** Maps [Continue] to [Finished] and [Stop] to [Unfinished]. *)
    val of_continue_or_stop : Continue_or_stop.t -> t

    (** Maps [Finished] to [Continue] and [Unfinished] to [Stop]. *)
    val to_continue_or_stop : t -> Continue_or_stop.t
  end

  module Merge_element : sig
    type ('left, 'right) t =
      [ `Left of 'left
      | `Right of 'right
      | `Both of 'left * 'right
      ]
    [@@deriving_inline compare, equal, sexp_of]

    val compare
      :  ('left -> 'left -> int)
      -> ('right -> 'right -> int)
      -> ('left, 'right) t
      -> ('left, 'right) t
      -> int

    val equal
      :  ('left -> 'left -> bool)
      -> ('right -> 'right -> bool)
      -> ('left, 'right) t
      -> ('left, 'right) t
      -> bool

    val sexp_of_t
      :  ('left -> Sexplib0.Sexp.t)
      -> ('right -> Sexplib0.Sexp.t)
      -> ('left, 'right) t
      -> Sexplib0.Sexp.t

    [@@@end]

    val left : ('left, _) t -> 'left option
    val right : (_, 'right) t -> 'right option
    val left_value : ('left, _) t -> default:'left -> 'left
    val right_value : (_, 'right) t -> default:'right -> 'right

    val values
      :  ('left, 'right) t
      -> left_default:'left
      -> right_default:'right
      -> 'left * 'right
  end

  type ('k, 'cmp) comparator = ('k, 'cmp) Comparator.Module.t
  [@@deprecated "[since 2021-12] use [Comparator.Module.t] instead"]

  (** Test if the invariants of the internal AVL search tree hold. *)
  val invariants : (_, _, _) t -> bool

  (** Returns a first-class module that can be used to build other map/set/etc.
      with the same notion of comparison. *)
  val comparator_s : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.Module.t

  val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t

  (** The empty map. *)
  val empty : ('a, 'cmp) Comparator.Module.t -> ('a, 'b, 'cmp) t

  (** A map with one (key, data) pair. *)
  val singleton : ('a, 'cmp) Comparator.Module.t -> 'a -> 'b -> ('a, 'b, 'cmp) t

  (** Creates a map from an association list with unique keys. *)
  val of_alist
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  (** Creates a map from an association list with unique keys, returning an error if
      duplicate ['a] keys are found. *)
  val of_alist_or_error
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> ('a, 'b, 'cmp) t Or_error.t

  (** Creates a map from an association list with unique keys, raising an exception if
      duplicate ['a] keys are found. *)
  val of_alist_exn : ('a, 'cmp) Comparator.Module.t -> ('a * 'b) list -> ('a, 'b, 'cmp) t

  (** Creates a map from an association list with possibly repeated keys. The values in
      the map for a given key appear in the same order as they did in the association
      list. *)
  val of_alist_multi
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> ('a, 'b list, 'cmp) t

  (** Combines an association list into a map, folding together bound values with common
      keys. The accumulator is per-key.

      Example:

      {[
        # let map = String.Map.of_alist_fold
                      [ "a", 1; "a", 10; "b", 2; "b", 20; "b", 200 ]
                      ~init:Int.Set.empty
                      ~f:Set.add
          in
          print_s [%sexp (map : Int.Set.t String.Map.t)];;
        ((a (1 10)) (b (2 20 200)))
        - : unit = ()
      ]}
  *)
  val of_alist_fold
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> init:'c
    -> f:('c -> 'b -> 'c)
    -> ('a, 'c, 'cmp) t

  (** Combines an association list into a map, reducing together bound values with common
      keys. *)
  val of_alist_reduce
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> f:('b -> 'b -> 'b)
    -> ('a, 'b, 'cmp) t

  (** [of_iteri ~iteri] behaves like [of_alist], except that instead of taking a concrete
      data structure, it takes an iteration function.  For instance, to convert a string table
      into a map: [of_iteri (module String) ~f:(Hashtbl.iteri table)].  It is faster than
      adding the elements one by one. *)
  val of_iteri
    :  ('a, 'cmp) Comparator.Module.t
    -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  (** Like [of_iteri] except that it raises an exception if duplicate ['a] keys are found. *)
  val of_iteri_exn
    :  ('a, 'cmp) Comparator.Module.t
    -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
    -> ('a, 'b, 'cmp) t

  (** Creates a map from a sorted array of key-data pairs. The input array must be sorted
      (either in ascending or descending order), as given by the relevant comparator, and
      must not contain duplicate keys. If either of these conditions does not hold,
      an error is returned.  *)
  val of_sorted_array
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) array
    -> ('a, 'b, 'cmp) t Or_error.t

  (** Like [of_sorted_array] except that it returns a map with broken invariants when an
      [Error] would have been returned. *)
  val of_sorted_array_unchecked
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) array
    -> ('a, 'b, 'cmp) t

  (** [of_increasing_iterator_unchecked c ~len ~f] behaves like [of_sorted_array_unchecked c
      (Array.init len ~f)], with the additional restriction that a decreasing order is not
      supported.  The advantage is not requiring you to allocate an intermediate array.  [f]
      will be called with 0, 1, ... [len - 1], in order. *)
  val of_increasing_iterator_unchecked
    :  ('a, 'cmp) Comparator.Module.t
    -> len:int
    -> f:(int -> 'a * 'b)
    -> ('a, 'b, 'cmp) t

  (** [of_increasing_sequence c seq] behaves like [of_sorted_array c (Sequence.to_array
      seq)], but does not allocate the intermediate array.

      The sequence will be folded over once, and the additional time complexity is {e O(n)}.
  *)
  val of_increasing_sequence
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k * 'v) Sequence.t
    -> ('k, 'v, 'cmp) t Or_error.t

  (** Creates a map from an association sequence with unique keys.

      [of_sequence c seq] behaves like [of_alist c (Sequence.to_list seq)] but
      does not allocate the intermediate list.

      If your sequence is increasing, use [of_increasing_sequence].
  *)
  val of_sequence
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k * 'v) Sequence.t
    -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k ]

  (** Creates a map from an association sequence with unique keys, returning an error if
      duplicate ['a] keys are found.

      [of_sequence_or_error c seq] behaves like [of_alist_or_error c (Sequence.to_list seq)]
      but does not allocate the intermediate list.
  *)
  val of_sequence_or_error
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t Or_error.t

  (** Creates a map from an association sequence with unique keys, raising an exception if
      duplicate ['a] keys are found.

      [of_sequence_exn c seq] behaves like [of_alist_exn c (Sequence.to_list seq)] but
      does not allocate the intermediate list.
  *)
  val of_sequence_exn
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t

  (** Creates a map from an association sequence with possibly repeated keys. The values in
      the map for a given key appear in the same order as they did in the association
      list.

      [of_sequence_multi c seq] behaves like [of_alist_exn c (Sequence.to_list seq)] but
      does not allocate the intermediate list.
  *)
  val of_sequence_multi
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b list, 'cmp) t

  (** Combines an association sequence into a map, folding together bound values with common
      keys.

      [of_sequence_fold c seq ~init ~f] behaves like [of_alist_fold c (Sequence.to_list seq) ~init ~f]
      but does not allocate the intermediate list.
  *)
  val of_sequence_fold
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> init:'c
    -> f:('c -> 'b -> 'c)
    -> ('a, 'c, 'cmp) t

  (** Combines an association sequence into a map, reducing together bound values with common
      keys.

      [of_sequence_reduce c seq ~f] behaves like [of_alist_reduce c (Sequence.to_list seq) ~f]
      but does not allocate the intermediate list.  *)
  val of_sequence_reduce
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> f:('b -> 'b -> 'b)
    -> ('a, 'b, 'cmp) t

  (** Tests whether a map is empty. *)
  val is_empty : (_, _, _) t -> bool

  (** [length map] returns the number of elements in [map].  O(1), but [Tree.length] is
      O(n). *)
  val length : (_, _, _) t -> int

  (** Returns a new map with the specified new binding; if the key was already bound, its
      previous binding disappears. *)
  val set : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

  (** [add t ~key ~data] adds a new entry to [t] mapping [key] to [data] and returns [`Ok]
      with the new map, or if [key] is already present in [t], returns [`Duplicate]. *)
  val add : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t Or_duplicate.t

  val add_exn : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

  (** If [key] is not present then add a singleton list, otherwise, cons data onto the
      head of the existing list. *)
  val add_multi : ('k, 'v list, 'cmp) t -> key:'k -> data:'v -> ('k, 'v list, 'cmp) t

  (** If the key is present, then remove its head element; if the result is empty, remove
      the key. *)
  val remove_multi : ('k, 'v list, 'cmp) t -> 'k -> ('k, 'v list, 'cmp) t

  (** Returns the value bound to the given key, or the empty list if there is none. *)
  val find_multi : ('k, 'v list, 'cmp) t -> 'k -> 'v list

  (** [change t key ~f] returns a new map [m] that is the same as [t] on all keys except
      for [key], and whose value for [key] is defined by [f], i.e., [find m key = f (find
      t key)]. *)
  val change : ('k, 'v, 'cmp) t -> 'k -> f:('v option -> 'v option) -> ('k, 'v, 'cmp) t

  (** [update t key ~f] is [change t key ~f:(fun o -> Some (f o))]. *)
  val update : ('k, 'v, 'cmp) t -> 'k -> f:('v option -> 'v) -> ('k, 'v, 'cmp) t


  (** Returns [Some value] bound to the given key, or [None] if none exists. *)
  val find : ('k, 'v, 'cmp) t -> 'k -> 'v option

  (** Returns the value bound to the given key, raising [Caml.Not_found] or [Not_found_s]
      if none exists. *)
  val find_exn : ('k, 'v, 'cmp) t -> 'k -> 'v

  (** Returns a new map with any binding for the key in question removed. *)
  val remove : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t

  (** [mem map key] tests whether [map] contains a binding for [key]. *)
  val mem : ('k, _, 'cmp) t -> 'k -> bool

  val iter_keys : ('k, _, _) t -> f:('k -> unit) -> unit
  val iter : (_, 'v, _) t -> f:('v -> unit) -> unit
  val iteri : ('k, 'v, _) t -> f:(key:'k -> data:'v -> unit) -> unit

  (** Iterates until the first time [f] returns [Stop]. If [f] returns [Stop], the final
      result is [Unfinished]. Otherwise, the final result is [Finished]. *)
  val iteri_until
    :  ('k, 'v, _) t
    -> f:(key:'k -> data:'v -> Continue_or_stop.t)
    -> Finished_or_unfinished.t

  (** Iterates two maps side by side. The complexity of this function is O(M + N).  If two
      inputs are [[(0, a); (1, a)]] and [[(1, b); (2, b)]], [f] will be called with [[(0,
      `Left a); (1, `Both (a, b)); (2, `Right b)]]. *)
  val iter2
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> unit)
    -> unit

  (** Returns a new map with bound values replaced by [f] applied to the bound values.*)
  val map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'cmp) t

  (** Like [map], but the passed function takes both [key] and [data] as arguments. *)
  val mapi : ('k, 'v1, 'cmp) t -> f:(key:'k -> data:'v1 -> 'v2) -> ('k, 'v2, 'cmp) t

  (** Convert map with keys of type ['k2] to a map with keys of type ['k2] using [f]. *)
  val map_keys
    :  ('k2, 'cmp2) Comparator.Module.t
    -> ('k1, 'v, 'cmp1) t
    -> f:('k1 -> 'k2)
    -> [ `Ok of ('k2, 'v, 'cmp2) t | `Duplicate_key of 'k2 ]

  (** Like [map_keys], but raises on duplicate key. *)
  val map_keys_exn
    :  ('k2, 'cmp2) Comparator.Module.t
    -> ('k1, 'v, 'cmp1) t
    -> f:('k1 -> 'k2)
    -> ('k2, 'v, 'cmp2) t

  (** Folds over keys and data in the map in increasing order of [key]. *)
  val fold : ('k, 'v, _) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

  (** Folds over keys and data in the map in increasing order of [key], until the first
      time that [f] returns [Stop _]. If [f] returns [Stop final], this function returns
      immediately with the value [final]. If [f] never returns [Stop _], and the final
      call to [f] returns [Continue last], this function returns [finish last]. *)
  val fold_until
    :  ('k, 'v, _) t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> ('acc, 'final) Container.Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final

  (** Folds over keys and data in the map in decreasing order of [key]. *)
  val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

  (** Folds over two maps side by side, like [iter2]. *)
  val fold2
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> init:'a
    -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> 'a -> 'a)
    -> 'a

  (** [filter], [filteri], [filter_keys], [filter_map], and [filter_mapi] run in O(n * lg
      n) time; they simply accumulate each key & data pair retained by [f] into a new map
      using [add]. *)
  val filter_keys : ('k, 'v, 'cmp) t -> f:('k -> bool) -> ('k, 'v, 'cmp) t

  val filter : ('k, 'v, 'cmp) t -> f:('v -> bool) -> ('k, 'v, 'cmp) t
  val filteri : ('k, 'v, 'cmp) t -> f:(key:'k -> data:'v -> bool) -> ('k, 'v, 'cmp) t

  (** Returns a new map with bound values filtered by [f] applied to the bound values. *)
  val filter_map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2 option) -> ('k, 'v2, 'cmp) t

  (** Like [filter_map], but the passed function takes both [key] and [data] as
      arguments. *)
  val filter_mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> 'v2 option)
    -> ('k, 'v2, 'cmp) t

  (** [partition_mapi t ~f] returns two new [t]s, with each key in [t] appearing in
      exactly one of the resulting maps depending on its mapping in [f]. *)
  val partition_mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
    -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

  (** [partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)] *)
  val partition_map
    :  ('k, 'v1, 'cmp) t
    -> f:('v1 -> ('v2, 'v3) Either.t)
    -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

  (**
     {[
       partitioni_tf t ~f
       =
       partition_mapi t ~f:(fun ~key ~data ->
         if f ~key ~data
         then First data
         else Second data)
     ]} *)
  val partitioni_tf
    :  ('k, 'v, 'cmp) t
    -> f:(key:'k -> data:'v -> bool)
    -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** [partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)] *)
  val partition_tf
    :  ('k, 'v, 'cmp) t
    -> f:('v -> bool)
    -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** Produces [Ok] of a map including all keys if all data is [Ok], or an [Error]
      including all errors otherwise. *)
  val combine_errors : ('k, 'v Or_error.t, 'cmp) t -> ('k, 'v, 'cmp) t Or_error.t

  (** Returns a total ordering between maps. The first argument is a total ordering used
      to compare data associated with equal keys in the two maps. *)
  val compare_direct : ('v -> 'v -> int) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> int

  (** Hash function: a building block to use when hashing data structures containing maps in
      them. [hash_fold_direct hash_fold_key] is compatible with [compare_direct] iff
      [hash_fold_key] is compatible with [(comparator m).compare] of the map [m] being
      hashed. *)
  val hash_fold_direct : 'k Hash.folder -> 'v Hash.folder -> ('k, 'v, 'cmp) t Hash.folder

  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain
      the same keys and associate each key with the same value.  [cmp] is the equality
      predicate used to compare the values associated with the keys. *)
  val equal : ('v -> 'v -> bool) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool

  (** Returns a list of the keys in the given map. *)
  val keys : ('k, _, _) t -> 'k list

  (** Returns a list of the data in the given map. *)
  val data : (_, 'v, _) t -> 'v list

  (** Creates an association list from the given map. *)
  val to_alist
    :  ?key_order:[ `Increasing | `Decreasing ] (** default is [`Increasing] *)
    -> ('k, 'v, _) t
    -> ('k * 'v) list

  (** {2 Additional operations on maps} *)

  (** Merges two maps. The runtime is O(length(t1) + length(t2)). You shouldn't use this
      function to merge a list of maps; consider using [merge_skewed] instead. *)
  val merge
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> f:(key:'k -> ('v1, 'v2) Merge_element.t -> 'v3 option)
    -> ('k, 'v3, 'cmp) t

  (** A special case of [merge], [merge_skewed t1 t2] is a map containing all the
      bindings of [t1] and [t2]. Bindings that appear in both [t1] and [t2] are
      combined into a single value using the [combine] function. In a call
      [combine ~key v1 v2], the value [v1] comes from [t1] and [v2] from [t2].

      The runtime of [merge_skewed] is [O(min(l1, l2) * log(max(l1, l2)))], where [l1] is
      the length of [t1] and [l2] the length of [t2]. This is likely to be faster than
      [merge] when one of the maps is a lot smaller, or when you merge a list of maps. *)
  val merge_skewed
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> combine:(key:'k -> 'v -> 'v -> 'v)
    -> ('k, 'v, 'cmp) t

  module Symmetric_diff_element : sig
    type ('k, 'v) t = 'k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]
    [@@deriving_inline compare, equal, sexp, sexp_grammar]

    include Ppx_compare_lib.Comparable.S2 with type ('k, 'v) t := ('k, 'v) t
    include Ppx_compare_lib.Equal.S2 with type ('k, 'v) t := ('k, 'v) t
    include Sexplib0.Sexpable.S2 with type ('k, 'v) t := ('k, 'v) t

    val t_sexp_grammar
      :  'k Sexplib0.Sexp_grammar.t
      -> 'v Sexplib0.Sexp_grammar.t
      -> ('k, 'v) t Sexplib0.Sexp_grammar.t

    [@@@end]
  end

  (** [symmetric_diff t1 t2 ~data_equal] returns a list of changes between [t1] and [t2].
      It is intended to be efficient in the case where [t1] and [t2] share a large amount
      of structure. The keys in the output sequence will be in sorted order.

      It is assumed that [data_equal] is at least as equating as physical equality: that
      [phys_equal x y] implies [data_equal x y]. Otherwise, [symmetric_diff] may behave in
      unexpected ways. For example, with [~data_equal:(fun _ _ -> false)] it is NOT
      necessarily the case the resulting change sequence will contain an element
      [(k, `Unequal _)] for every key [k] shared by both maps.

      Warning: Float equality violates this property! [phys_equal Float.nan Float.nan] is
      true, but [Float.(=) Float.nan Float.nan] is false. *)
  val symmetric_diff
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> data_equal:('v -> 'v -> bool)
    -> ('k, 'v) Symmetric_diff_element.t Sequence.t

  (** [fold_symmetric_diff t1 t2 ~data_equal] folds across an implicit sequence of changes
      between [t1] and [t2], in sorted order by keys. Equivalent to
      [Sequence.fold (symmetric_diff t1 t2 ~data_equal)], and more efficient. *)
  val fold_symmetric_diff
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> data_equal:('v -> 'v -> bool)
    -> init:'a
    -> f:('a -> ('k, 'v) Symmetric_diff_element.t -> 'a)
    -> 'a

  (** [min_elt map] returns [Some (key, data)] pair corresponding to the minimum key in
      [map], or [None] if empty. *)
  val min_elt : ('k, 'v, _) t -> ('k * 'v) option

  val min_elt_exn : ('k, 'v, _) t -> 'k * 'v

  (** [max_elt map] returns [Some (key, data)] pair corresponding to the maximum key in
      [map], or [None] if [map] is empty. *)
  val max_elt : ('k, 'v, _) t -> ('k * 'v) option

  val max_elt_exn : ('k, 'v, _) t -> 'k * 'v

  (** These functions have the same semantics as similar functions in [List]. *)

  val for_all : ('k, 'v, _) t -> f:('v -> bool) -> bool
  val for_alli : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> bool
  val exists : ('k, 'v, _) t -> f:('v -> bool) -> bool
  val existsi : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> bool
  val count : ('k, 'v, _) t -> f:('v -> bool) -> int
  val counti : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> int


  (** [split t key] returns a map of keys strictly less than [key], the mapping of [key] if
      any, and a map of keys strictly greater than [key].

      Runtime is O(m + log n), where n is the size of the input map and m is the size of
      the smaller of the two output maps.  The O(m) term is due to the need to calculate
      the length of the output maps. *)
  val split
    :  ('k, 'v, 'cmp) t
    -> 'k
    -> ('k, 'v, 'cmp) t * ('k * 'v) option * ('k, 'v, 'cmp) t

  (** [append ~lower_part ~upper_part] returns [`Ok map] where [map] contains all the
      [(key, value)] pairs from the two input maps if all the keys from [lower_part] are
      less than all the keys from [upper_part].  Otherwise it returns
      [`Overlapping_key_ranges].

      Runtime is O(log n) where n is the size of the larger input map.  This can be
      significantly faster than [Map.merge] or repeated [Map.add].

      {[
        assert (match Map.append ~lower_part ~upper_part with
          | `Ok whole_map ->
            Map.to_alist whole_map
            = List.append (to_alist lower_part) (to_alist upper_part)
          | `Overlapping_key_ranges -> true);
      ]} *)
  val append
    :  lower_part:('k, 'v, 'cmp) t
    -> upper_part:('k, 'v, 'cmp) t
    -> [ `Ok of ('k, 'v, 'cmp) t | `Overlapping_key_ranges ]

  (** [subrange t ~lower_bound ~upper_bound] returns a map containing all the entries from
      [t] whose keys lie inside the interval indicated by [~lower_bound] and
      [~upper_bound].  If this interval is empty, an empty map is returned.

      Runtime is O(m + log n), where n is the size of the input map and m is the size of
      the output map.  The O(m) term is due to the need to calculate the length of the
      output map. *)
  val subrange
    :  ('k, 'v, 'cmp) t
    -> lower_bound:'k Maybe_bound.t
    -> upper_bound:'k Maybe_bound.t
    -> ('k, 'v, 'cmp) t

  (** [fold_range_inclusive t ~min ~max ~init ~f] folds [f] (with initial value [~init])
      over all keys (and their associated values) that are in the range [[min, max]]
      (inclusive).  *)
  val fold_range_inclusive
    :  ('k, 'v, 'cmp) t
    -> min:'k
    -> max:'k
    -> init:'a
    -> f:(key:'k -> data:'v -> 'a -> 'a)
    -> 'a

  (** [range_to_alist t ~min ~max] returns an associative list of the elements whose keys
      lie in [[min, max]] (inclusive), with the smallest key being at the head of the
      list. *)
  val range_to_alist : ('k, 'v, 'cmp) t -> min:'k -> max:'k -> ('k * 'v) list

  (** [closest_key t dir k] returns the [(key, value)] pair in [t] with [key] closest to
      [k] that satisfies the given inequality bound.

      For example, [closest_key t `Less_than k] would be the pair with the closest key to
      [k] where [key < k].

      [to_sequence] can be used to get the same results as [closest_key].  It is less
      efficient for individual lookups but more efficient for finding many elements starting
      at some value. *)
  val closest_key
    :  ('k, 'v, 'cmp) t
    -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
    -> 'k
    -> ('k * 'v) option

  (** [nth t n] finds the (key, value) pair of rank n (i.e., such that there are exactly n
      keys strictly less than the found key), if one exists.  O(log(length t) + n) time. *)
  val nth : ('k, 'v, _) t -> int -> ('k * 'v) option

  val nth_exn : ('k, 'v, _) t -> int -> 'k * 'v

  (** [rank t k] If [k] is in [t], returns the number of keys strictly less than [k] in
      [t], and [None] otherwise. *)
  val rank : ('k, 'v, 'cmp) t -> 'k -> int option



  (** [to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t]
      gives a sequence of key-value pairs between [keys_less_or_equal_to] and
      [keys_greater_or_equal_to] inclusive, presented in [order].  If
      [keys_greater_or_equal_to > keys_less_or_equal_to], the sequence is
      empty.

      When neither [keys_greater_or_equal_to] nor [keys_less_or_equal_to] are
      provided, the cost is O(log n) up front and amortized O(1) to produce
      each element. If either is provided (and is used by the order parameter
      provided), then the the cost is O(n) up front, and amortized O(1) to
      produce each element. *)
  val to_sequence
    :  ?order:[ `Increasing_key (** default *) | `Decreasing_key ]
    -> ?keys_greater_or_equal_to:'k
    -> ?keys_less_or_equal_to:'k
    -> ('k, 'v, 'cmp) t
    -> ('k * 'v) Sequence.t

  (** [binary_search t ~compare which elt] returns the [(key, value)] pair in [t]
      specified by [compare] and [which], if one exists.

      [t] must be sorted in increasing order according to [compare], where [compare] and
      [elt] divide [t] into three (possibly empty) segments:

      {v
        |  < elt  |  = elt  |  > elt  |
      v}

      [binary_search] returns an element on the boundary of segments as specified by
      [which].  See the diagram below next to the [which] variants.

      [binary_search] does not check that [compare] orders [t], and behavior is
      unspecified if [compare] doesn't order [t].  Behavior is also unspecified if
      [compare] mutates [t]. *)
  val binary_search
    :  ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'key -> int)
    -> [ `Last_strictly_less_than (**        {v | < elt X |                       v} *)
       | `Last_less_than_or_equal_to (**     {v |      <= elt       X |           v} *)
       | `Last_equal_to (**                  {v           |   = elt X |           v} *)
       | `First_equal_to (**                 {v           | X = elt   |           v} *)
       | `First_greater_than_or_equal_to (** {v           | X       >= elt      | v} *)
       | `First_strictly_greater_than (**    {v                       | X > elt | v} *)
       ]
    -> 'key
    -> ('k * 'v) option

  (** [binary_search_segmented t ~segment_of which] takes a [segment_of] function that
      divides [t] into two (possibly empty) segments:

      {v
        | segment_of elt = `Left | segment_of elt = `Right |
      v}

      [binary_search_segmented] returns the [(key, value)] pair on the boundary of the
      segments as specified by [which]: [`Last_on_left] yields the last element of the
      left segment, while [`First_on_right] yields the first element of the right segment.
      It returns [None] if the segment is empty.

      [binary_search_segmented] does not check that [segment_of] segments [t] as in the
      diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
      is also unspecified if [segment_of] mutates [t]. *)
  val binary_search_segmented
    :  ('k, 'v, 'cmp) t
    -> segment_of:(key:'k -> data:'v -> [ `Left | `Right ])
    -> [ `Last_on_left | `First_on_right ]
    -> ('k * 'v) option

  (** [binary_search_subrange] takes a [compare] function that divides [t] into three
      (possibly empty) segments with respect to [lower_bound] and [upper_bound]:

      {v
        | Below_lower_bound | In_range | Above_upper_bound |
      v}

      and returns a map of the [In_range] segment.

      Runtime is O(log m + n) where [m] is the length of the input map and [n] is the
      length of the output. The linear term in [n] is to compute the length of the output.

      Behavior is undefined if [compare] does not segment [t] as shown above, or if
      [compare] mutates its inputs. *)
  val binary_search_subrange
    :  ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'bound -> int)
    -> lower_bound:'bound Maybe_bound.t
    -> upper_bound:'bound Maybe_bound.t
    -> ('k, 'v, 'cmp) t

  (** [M] is meant to be used in combination with OCaml applicative functor types:

      {[
        type string_to_int_map = int Map.M(String).t
      ]}

      which stands for:

      {[
        type string_to_int_map = (String.t, int, String.comparator_witness) Map.t
      ]}

      The point is that [int Map.M(String).t] supports deriving, whereas the second syntax
      doesn't (because there is no such thing as, say, [String.sexp_of_comparator_witness]
      -- instead you would want to pass the comparator directly).

      In addition, when using [@@deriving], the requirements on the key module are only
      those needed to satisfy what you are trying to derive on the map itself. Say you
      write:

      {[
        type t = int Map.M(X).t [@@deriving hash]
      ]}

      then this will be well typed exactly if [X] contains at least:
      - a type [t] with no parameters
      - a comparator witness
      - a [hash_fold_t] function with the right type *)
  module M (K : sig
      type t
      type comparator_witness
    end) : sig
    type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
  end

  include For_deriving with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) t

  (** [Using_comparator] is a similar interface as the toplevel of [Map], except the
      functions take a [~comparator:('k, 'cmp) Comparator.t], whereas the functions at the
      toplevel of [Map] take a [('k, 'cmp) comparator]. *)
  module Using_comparator : sig
    type nonrec ('k, +'v, 'cmp) t = ('k, 'v, 'cmp) t [@@deriving_inline sexp_of]

    val sexp_of_t
      :  ('k -> Sexplib0.Sexp.t)
      -> ('v -> Sexplib0.Sexp.t)
      -> ('cmp -> Sexplib0.Sexp.t)
      -> ('k, 'v, 'cmp) t
      -> Sexplib0.Sexp.t

    [@@@end]

    val t_of_sexp_direct
      :  comparator:('k, 'cmp) Comparator.t
      -> (Sexp.t -> 'k)
      -> (Sexp.t -> 'v)
      -> Sexp.t
      -> ('k, 'v, 'cmp) t

    module Tree : sig
      type (+'k, +'v, 'cmp) t [@@deriving_inline sexp_of]

      val sexp_of_t
        :  ('k -> Sexplib0.Sexp.t)
        -> ('v -> Sexplib0.Sexp.t)
        -> ('cmp -> Sexplib0.Sexp.t)
        -> ('k, 'v, 'cmp) t
        -> Sexplib0.Sexp.t

      [@@@end]

      val t_of_sexp_direct
        :  comparator:('k, 'cmp) Comparator.t
        -> (Sexp.t -> 'k)
        -> (Sexp.t -> 'v)
        -> Sexp.t
        -> ('k, 'v, 'cmp) t

      include
        Creators_and_accessors3_with_comparator
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t

      val empty_without_value_restriction : (_, _, _) t

      (** [Build_increasing] can be used to construct a map incrementally from a
          sequence that is known to be increasing.

          The total time complexity of constructing a map this way is O(n), which is more
          efficient than using [Map.add] by a logarithmic factor.

          This interface can be thought of as a dual of [to_sequence], but we don't have
          an equally neat idiom for the duals of sequences ([of_sequence] is much less
          general because it does not allow the sequence to be produced asynchronously). *)
      module Build_increasing : sig
        type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
        type ('k, 'v, 'w) t

        val empty : ('k, 'v, 'w) t

        (** Time complexity of [add_exn] is amortized constant-time (if [t] is used
            linearly), with a worst-case O(log(n)) time. *)
        val add_exn
          :  ('k, 'v, 'w) t
          -> comparator:('k, 'w) Comparator.t
          -> key:'k
          -> data:'v
          -> ('k, 'v, 'w) t

        (** Time complexity is O(log(n)). *)
        val to_tree : ('k, 'v, 'w) t -> ('k, 'v, 'w) tree
      end
    end

    include
      Accessors3
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t

    include
      Creators3_with_comparator
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t

    val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t

    val hash_fold_direct
      :  'k Hash.folder
      -> 'v Hash.folder
      -> ('k, 'v, 'cmp) t Hash.folder

    (** To get around the value restriction, apply the functor and include it. You
        can see an example of this in the [Poly] submodule below. *)
    module Empty_without_value_restriction (K : Comparator.S1) : sig
      val empty : ('a K.t, 'v, K.comparator_witness) t
    end
  end

  (** A polymorphic Map. *)
  module Poly :
    S_poly
    with type ('key, +'value) t = ('key, 'value, Comparator.Poly.comparator_witness) t
     and type ('key, +'value) tree =
           ('key, 'value, Comparator.Poly.comparator_witness) Using_comparator.Tree.t
     and type comparator_witness = Comparator.Poly.comparator_witness

  (** Create a map from a tree using the given comparator. *)
  val of_tree
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k, 'v, 'cmp) Using_comparator.Tree.t
    -> ('k, 'v, 'cmp) t

  (** Extract a tree from a map. *)
  val to_tree : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Using_comparator.Tree.t


  (** {2 Modules and module types for extending [Map]}

      For use in extensions of Base, like [Core]. *)

  module With_comparator = With_comparator
  module With_first_class_module = With_first_class_module
  module Without_comparator = Without_comparator

  module type For_deriving = For_deriving
  module type S_poly = S_poly
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
end
