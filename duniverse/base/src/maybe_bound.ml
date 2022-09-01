open! Import

type 'a t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving_inline enumerate, sexp, sexp_grammar]

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

let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
  fun (type a__018_) : ((Sexplib0.Sexp.t -> a__018_) -> Sexplib0.Sexp.t -> a__018_ t) ->
  let error_source__006_ = "maybe_bound.ml.t" in
  fun _of_a__003_ -> function
    | Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom (("incl" | "Incl") as _tag__009_) :: sexp_args__010_) as
      _sexp__008_ ->
      (match sexp_args__010_ with
       | [ arg0__011_ ] ->
         let res0__012_ = _of_a__003_ arg0__011_ in
         Incl res0__012_
       | _ ->
         Sexplib0.Sexp_conv_error.stag_incorrect_n_args
           error_source__006_
           _tag__009_
           _sexp__008_)
    | Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom (("excl" | "Excl") as _tag__014_) :: sexp_args__015_) as
      _sexp__013_ ->
      (match sexp_args__015_ with
       | [ arg0__016_ ] ->
         let res0__017_ = _of_a__003_ arg0__016_ in
         Excl res0__017_
       | _ ->
         Sexplib0.Sexp_conv_error.stag_incorrect_n_args
           error_source__006_
           _tag__014_
           _sexp__013_)
    | Sexplib0.Sexp.Atom ("unbounded" | "Unbounded") -> Unbounded
    | Sexplib0.Sexp.Atom ("incl" | "Incl") as sexp__007_ ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__006_ sexp__007_
    | Sexplib0.Sexp.Atom ("excl" | "Excl") as sexp__007_ ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__006_ sexp__007_
    | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("unbounded" | "Unbounded") :: _) as
      sexp__007_ -> Sexplib0.Sexp_conv_error.stag_no_args error_source__006_ sexp__007_
    | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__005_ ->
      Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__006_ sexp__005_
    | Sexplib0.Sexp.List [] as sexp__005_ ->
      Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__006_ sexp__005_
    | sexp__005_ ->
      Sexplib0.Sexp_conv_error.unexpected_stag error_source__006_ sexp__005_
;;

let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun (type a__024_) : ((a__024_ -> Sexplib0.Sexp.t) -> a__024_ t -> Sexplib0.Sexp.t) ->
  fun _of_a__019_ -> function
    | Incl arg0__020_ ->
      let res0__021_ = _of_a__019_ arg0__020_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Incl"; res0__021_ ]
    | Excl arg0__022_ ->
      let res0__023_ = _of_a__019_ arg0__022_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Excl"; res0__023_ ]
    | Unbounded -> Sexplib0.Sexp.Atom "Unbounded"
;;

let (t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t) =
  fun _'a_sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag
                { name = "Incl"
                ; clause_kind =
                    List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                }
            ; No_tag
                { name = "Excl"
                ; clause_kind =
                    List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                }
            ; No_tag { name = "Unbounded"; clause_kind = Atom_clause }
            ]
        }
  }
;;

[@@@end]

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving_inline sexp, sexp_grammar, compare, hash]

let interval_comparison_of_sexp =
  (let error_source__027_ = "maybe_bound.ml.interval_comparison" in
   function
   | Sexplib0.Sexp.Atom ("below_lower_bound" | "Below_lower_bound") -> Below_lower_bound
   | Sexplib0.Sexp.Atom ("in_range" | "In_range") -> In_range
   | Sexplib0.Sexp.Atom ("above_upper_bound" | "Above_upper_bound") -> Above_upper_bound
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom ("below_lower_bound" | "Below_lower_bound") :: _) as sexp__028_
     -> Sexplib0.Sexp_conv_error.stag_no_args error_source__027_ sexp__028_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("in_range" | "In_range") :: _) as sexp__028_
     -> Sexplib0.Sexp_conv_error.stag_no_args error_source__027_ sexp__028_
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom ("above_upper_bound" | "Above_upper_bound") :: _) as sexp__028_
     -> Sexplib0.Sexp_conv_error.stag_no_args error_source__027_ sexp__028_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__026_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__027_ sexp__026_
   | Sexplib0.Sexp.List [] as sexp__026_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__027_ sexp__026_
   | sexp__026_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__027_ sexp__026_
                   : Sexplib0.Sexp.t -> interval_comparison)
;;

let sexp_of_interval_comparison =
  (function
    | Below_lower_bound -> Sexplib0.Sexp.Atom "Below_lower_bound"
    | In_range -> Sexplib0.Sexp.Atom "In_range"
    | Above_upper_bound -> Sexplib0.Sexp.Atom "Above_upper_bound"
                           : interval_comparison -> Sexplib0.Sexp.t)
;;

let (interval_comparison_sexp_grammar : interval_comparison Sexplib0.Sexp_grammar.t) =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag { name = "Below_lower_bound"; clause_kind = Atom_clause }
            ; No_tag { name = "In_range"; clause_kind = Atom_clause }
            ; No_tag { name = "Above_upper_bound"; clause_kind = Atom_clause }
            ]
        }
  }
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
