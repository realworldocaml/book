open! Base
open Import
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

module Body = struct
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t
    : type a. (a -> Ppx_sexp_conv_lib.Sexp.t) -> a t -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a -> function
      | Exact v0 ->
        let v0 = sexp_of_string v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Exact"; v0 ]
      | Output -> Ppx_sexp_conv_lib.Sexp.Atom "Output"
      | Pretty v0 ->
        let v0 = _of_a v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Pretty"; v0 ]
      | Unreachable -> Ppx_sexp_conv_lib.Sexp.Atom "Unreachable"
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__001_ b__002_ ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else (
      match a__001_, b__002_ with
      | Exact _a__003_, Exact _b__004_ -> compare_string _a__003_ _b__004_
      | Exact _, _ -> -1
      | _, Exact _ -> 1
      | Output, Output -> 0
      | Output, _ -> -1
      | _, Output -> 1
      | Pretty _a__005_, Pretty _b__006_ -> _cmp__a _a__005_ _b__006_
      | Pretty _, _ -> -1
      | _, Pretty _ -> 1
      | Unreachable, Unreachable -> 0)
  ;;

  let _ = compare

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__007_ b__008_ ->
    if Ppx_compare_lib.phys_equal a__007_ b__008_
    then true
    else (
      match a__007_, b__008_ with
      | Exact _a__009_, Exact _b__010_ -> equal_string _a__009_ _b__010_
      | Exact _, _ -> false
      | _, Exact _ -> false
      | Output, Output -> true
      | Output, _ -> false
      | _, Output -> false
      | Pretty _a__011_, Pretty _b__012_ -> _cmp__a _a__011_ _b__012_
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

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  fun _of_a -> function
    | { tag = v_tag
      ; body = v_body
      ; extid_location = v_extid_location
      ; body_location = v_body_location
      } ->
      let bnds = [] in
      let bnds =
        let arg = File.Location.sexp_of_t v_body_location in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "body_location"; arg ]
        :: bnds
      in
      let bnds =
        let arg = File.Location.sexp_of_t v_extid_location in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "extid_location"; arg ]
        :: bnds
      in
      let bnds =
        let arg = Body.sexp_of_t _of_a v_body in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "body"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_option sexp_of_string v_tag in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "tag"; arg ] :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
;;

let _ = sexp_of_t

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__013_ b__014_ ->
  if Ppx_compare_lib.phys_equal a__013_ b__014_
  then 0
  else (
    match compare_option compare_string a__013_.tag b__014_.tag with
    | 0 ->
      (match Body.compare _cmp__a a__013_.body b__014_.body with
       | 0 ->
         (match File.Location.compare a__013_.extid_location b__014_.extid_location with
          | 0 -> File.Location.compare a__013_.body_location b__014_.body_location
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__019_ b__020_ ->
  if Ppx_compare_lib.phys_equal a__019_ b__020_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_option equal_string a__019_.tag b__020_.tag)
      (Ppx_compare_lib.( && )
         (Body.equal _cmp__a a__019_.body b__020_.body)
         (Ppx_compare_lib.( && )
            (File.Location.equal a__019_.extid_location b__020_.extid_location)
            (File.Location.equal a__019_.body_location b__020_.body_location)))
;;

let _ = equal

[@@@end]

module Raw = struct
  type nonrec t = string t [@@deriving_inline sexp_of, compare]

  let _ = fun (_ : t) -> ()
  let sexp_of_t = (fun v -> sexp_of_t sexp_of_string v : t -> Ppx_sexp_conv_lib.Sexp.t)
  let _ = sexp_of_t

  let compare =
    (fun a__025_ b__026_ -> compare compare_string a__025_ b__026_ : t -> t -> int)
  ;;

  let _ = compare

  [@@@end]
end

let map_pretty t ~f = { t with body = Body.map_pretty t.body ~f }
