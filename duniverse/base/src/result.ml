open! Import
module Either = Either0

type ('a, 'b) t = ('a, 'b) Caml.result =
  | Ok of 'a
  | Error of 'b
[@@deriving_inline sexp, sexp_grammar, compare, equal, hash]

let t_of_sexp :
  'a 'b.
  (Sexplib0.Sexp.t -> 'a) -> (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t
  =
  fun (type a__017_ b__018_)
      :  ((Sexplib0.Sexp.t -> a__017_) -> (Sexplib0.Sexp.t -> b__018_) -> Sexplib0.Sexp.t
          -> (a__017_, b__018_) t) ->
    let error_source__005_ = "result.ml.t" in
    fun _of_a__001_ _of_b__002_ -> function
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom (("ok" | "Ok") as _tag__008_) :: sexp_args__009_) as
        _sexp__007_ ->
        (match sexp_args__009_ with
         | [ arg0__010_ ] ->
           let res0__011_ = _of_a__001_ arg0__010_ in
           Ok res0__011_
         | _ ->
           Sexplib0.Sexp_conv_error.stag_incorrect_n_args
             error_source__005_
             _tag__008_
             _sexp__007_)
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom (("error" | "Error") as _tag__013_) :: sexp_args__014_) as
        _sexp__012_ ->
        (match sexp_args__014_ with
         | [ arg0__015_ ] ->
           let res0__016_ = _of_b__002_ arg0__015_ in
           Error res0__016_
         | _ ->
           Sexplib0.Sexp_conv_error.stag_incorrect_n_args
             error_source__005_
             _tag__013_
             _sexp__012_)
      | Sexplib0.Sexp.Atom ("ok" | "Ok") as sexp__006_ ->
        Sexplib0.Sexp_conv_error.stag_takes_args error_source__005_ sexp__006_
      | Sexplib0.Sexp.Atom ("error" | "Error") as sexp__006_ ->
        Sexplib0.Sexp_conv_error.stag_takes_args error_source__005_ sexp__006_
      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__004_ ->
        Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__005_ sexp__004_
      | Sexplib0.Sexp.List [] as sexp__004_ ->
        Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__005_ sexp__004_
      | sexp__004_ ->
        Sexplib0.Sexp_conv_error.unexpected_stag error_source__005_ sexp__004_
;;

let sexp_of_t :
  'a 'b.
  ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
  =
  fun (type a__025_ b__026_)
      :  ((a__025_ -> Sexplib0.Sexp.t) -> (b__026_ -> Sexplib0.Sexp.t)
          -> (a__025_, b__026_) t -> Sexplib0.Sexp.t) ->
    fun _of_a__019_ _of_b__020_ -> function
      | Ok arg0__021_ ->
        let res0__022_ = _of_a__019_ arg0__021_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Ok"; res0__022_ ]
      | Error arg0__023_ ->
        let res0__024_ = _of_b__020_ arg0__023_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Error"; res0__024_ ]
;;

let (t_sexp_grammar :
       'a Sexplib0.Sexp_grammar.t
     -> 'b Sexplib0.Sexp_grammar.t
     -> ('a, 'b) t Sexplib0.Sexp_grammar.t)
  =
  fun _'a_sexp_grammar _'b_sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag
                { name = "Ok"
                ; clause_kind =
                    List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                }
            ; No_tag
                { name = "Error"
                ; clause_kind =
                    List_clause { args = Cons (_'b_sexp_grammar.untyped, Empty) }
                }
            ]
        }
  }
;;

let compare :
  'a 'b. ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  =
  fun _cmp__a _cmp__b a__027_ b__028_ ->
  if Ppx_compare_lib.phys_equal a__027_ b__028_
  then 0
  else (
    match a__027_, b__028_ with
    | Ok _a__029_, Ok _b__030_ -> _cmp__a _a__029_ _b__030_
    | Ok _, _ -> -1
    | _, Ok _ -> 1
    | Error _a__031_, Error _b__032_ -> _cmp__b _a__031_ _b__032_)
;;

let equal :
  'a 'b. ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  =
  fun _cmp__a _cmp__b a__033_ b__034_ ->
  if Ppx_compare_lib.phys_equal a__033_ b__034_
  then true
  else (
    match a__033_, b__034_ with
    | Ok _a__035_, Ok _b__036_ -> _cmp__a _a__035_ _b__036_
    | Ok _, _ -> false
    | _, Ok _ -> false
    | Error _a__037_, Error _b__038_ -> _cmp__b _a__037_ _b__038_)
;;

let hash_fold_t
  : type a b.
    (Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state)
    -> (Ppx_hash_lib.Std.Hash.state -> b -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> (a, b) t
    -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a _hash_fold_b hsv arg ->
  match arg with
  | Ok _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in
    _hash_fold_a hsv _a0
  | Error _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in
    _hash_fold_b hsv _a0
;;

[@@@end]

include Monad.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let bind x ~f =
      match x with
      | Error _ as x -> x
      | Ok x -> f x
    ;;

    let map x ~f =
      match x with
      | Error _ as x -> x
      | Ok x -> Ok (f x)
    ;;

    let map = `Custom map
    let return x = Ok x
  end)

let invariant check_ok check_error t =
  match t with
  | Ok ok -> check_ok ok
  | Error error -> check_error error
;;

let fail x = Error x
let failf format = Printf.ksprintf fail format

let map_error t ~f =
  match t with
  | Ok _ as x -> x
  | Error x -> Error (f x)
;;

module Error = Monad.Make2 (struct
    type nonrec ('a, 'b) t = ('b, 'a) t

    let bind x ~f =
      match x with
      | Ok _ as ok -> ok
      | Error e -> f e
    ;;

    let map = `Custom map_error
    let return e = Error e
  end)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false
;;

let is_error = function
  | Ok _ -> false
  | Error _ -> true
;;

let ok = function
  | Ok x -> Some x
  | Error _ -> None
;;

let error = function
  | Ok _ -> None
  | Error x -> Some x
;;

let of_option opt ~error =
  match opt with
  | Some x -> Ok x
  | None -> Error error
;;

let iter v ~f =
  match v with
  | Ok x -> f x
  | Error _ -> ()
;;

let iter_error v ~f =
  match v with
  | Ok _ -> ()
  | Error x -> f x
;;

let to_either : _ t -> _ Either.t = function
  | Ok x -> First x
  | Error x -> Second x
;;

let of_either : _ Either.t -> _ t = function
  | First x -> Ok x
  | Second x -> Error x
;;

let ok_if_true bool ~error = if bool then Ok () else Error error

let try_with f =
  try Ok (f ()) with
  | exn -> Error exn
;;

let ok_exn = function
  | Ok x -> x
  | Error exn -> raise exn
;;

let ok_or_failwith = function
  | Ok x -> x
  | Error str -> failwith str
;;

module Export = struct
  type ('ok, 'err) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  let is_error = is_error
  let is_ok = is_ok
end

let combine t1 t2 ~ok ~err =
  match t1, t2 with
  | Ok _, Error e | Error e, Ok _ -> Error e
  | Ok ok1, Ok ok2 -> Ok (ok ok1 ok2)
  | Error err1, Error err2 -> Error (err err1 err2)
;;

let combine_errors l =
  let ok, errs = List1.partition_map l ~f:to_either in
  match errs with
  | [] -> Ok ok
  | _ :: _ -> Error errs
;;

let combine_errors_unit l = map (combine_errors l) ~f:(fun (_ : unit list) -> ())

(* deprecated binding for export only *)
let ok_fst = to_either
