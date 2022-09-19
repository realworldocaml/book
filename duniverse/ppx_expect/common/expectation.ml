open! Base
open Import
open Ppx_compare_lib.Builtin
open Sexplib0.Sexp_conv

module Body = struct
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun (type a__006_) : ((a__006_ -> Sexplib0.Sexp.t) -> a__006_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__001_ -> function
      | Exact arg0__002_ ->
        let res0__003_ = sexp_of_string arg0__002_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Exact"; res0__003_ ]
      | Output -> Sexplib0.Sexp.Atom "Output"
      | Pretty arg0__004_ ->
        let res0__005_ = _of_a__001_ arg0__004_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Pretty"; res0__005_ ]
      | Unreachable -> Sexplib0.Sexp.Atom "Unreachable"
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__007_ b__008_ ->
    if Ppx_compare_lib.phys_equal a__007_ b__008_
    then 0
    else (
      match a__007_, b__008_ with
      | Exact _a__009_, Exact _b__010_ -> compare_string _a__009_ _b__010_
      | Exact _, _ -> -1
      | _, Exact _ -> 1
      | Output, Output -> 0
      | Output, _ -> -1
      | _, Output -> 1
      | Pretty _a__011_, Pretty _b__012_ -> _cmp__a _a__011_ _b__012_
      | Pretty _, _ -> -1
      | _, Pretty _ -> 1
      | Unreachable, Unreachable -> 0)
  ;;

  let _ = compare

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__013_ b__014_ ->
    if Ppx_compare_lib.phys_equal a__013_ b__014_
    then true
    else (
      match a__013_, b__014_ with
      | Exact _a__015_, Exact _b__016_ -> equal_string _a__015_ _b__016_
      | Exact _, _ -> false
      | _, Exact _ -> false
      | Output, Output -> true
      | Output, _ -> false
      | _, Output -> false
      | Pretty _a__017_, Pretty _b__018_ -> _cmp__a _a__017_ _b__018_
      | Pretty _, _ -> false
      | _, Pretty _ -> false
      | Unreachable, Unreachable -> true)
  ;;

  let _ = equal

  [@@@end]

  let map_pretty t ~f =
    match t with
    | (Exact _ | Output | Unreachable) as t -> t
    | Pretty x -> Pretty (f x)
  ;;
end

type 'a t =
  { tag : string option
  ; body : 'a Body.t
  ; extid_location : File.Location.t
  ; body_location : File.Location.t
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a t) -> ()

let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun _of_a__019_
    { tag = tag__021_
    ; body = body__023_
    ; extid_location = extid_location__025_
    ; body_location = body_location__027_
    } ->
    let bnds__020_ = [] in
    let bnds__020_ =
      let arg__028_ = File.Location.sexp_of_t body_location__027_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "body_location"; arg__028_ ] :: bnds__020_
    in
    let bnds__020_ =
      let arg__026_ = File.Location.sexp_of_t extid_location__025_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "extid_location"; arg__026_ ] :: bnds__020_
    in
    let bnds__020_ =
      let arg__024_ = Body.sexp_of_t _of_a__019_ body__023_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "body"; arg__024_ ] :: bnds__020_
    in
    let bnds__020_ =
      let arg__022_ = sexp_of_option sexp_of_string tag__021_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "tag"; arg__022_ ] :: bnds__020_
    in
    Sexplib0.Sexp.List bnds__020_
;;

let _ = sexp_of_t

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__029_ b__030_ ->
  if Ppx_compare_lib.phys_equal a__029_ b__030_
  then 0
  else (
    match compare_option compare_string a__029_.tag b__030_.tag with
    | 0 ->
      (match Body.compare _cmp__a a__029_.body b__030_.body with
       | 0 ->
         (match File.Location.compare a__029_.extid_location b__030_.extid_location with
          | 0 -> File.Location.compare a__029_.body_location b__030_.body_location
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__035_ b__036_ ->
  if Ppx_compare_lib.phys_equal a__035_ b__036_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_option equal_string a__035_.tag b__036_.tag)
      (Ppx_compare_lib.( && )
         (Body.equal _cmp__a a__035_.body b__036_.body)
         (Ppx_compare_lib.( && )
            (File.Location.equal a__035_.extid_location b__036_.extid_location)
            (File.Location.equal a__035_.body_location b__036_.body_location)))
;;

let _ = equal

[@@@end]

module Raw = struct
  type nonrec t = string t [@@deriving_inline sexp_of, compare]

  let _ = fun (_ : t) -> ()
  let sexp_of_t = (fun x__041_ -> sexp_of_t sexp_of_string x__041_ : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  let compare =
    (fun a__042_ b__043_ -> compare compare_string a__042_ b__043_ : t -> t -> int)
  ;;

  let _ = compare

  [@@@end]
end

let map_pretty t ~f = { t with body = Body.map_pretty t.body ~f }
