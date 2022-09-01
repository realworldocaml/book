open Core
open Ppx_hash_lib.Std
open Hash.Builtin

(* Tests for record-field attributes [@hash.ignore] *)

let check_hash_differently ~hash ~sexp_of_t a b =
  if hash a = hash b
  then failwithf !"fail: expect values to hash differently: %{sexp:t} -- %{sexp:t}" a b ()
;;

let check_hash_same ~hash ~sexp_of_t a b =
  if not (hash a = hash b)
  then failwithf !"fail: expect values to hash the same: %{sexp:t} -- %{sexp:t}" a b ()
;;

module No_string_field = struct
  type t = { i : int } [@@deriving hash, sexp_of]

  let v1 = { i = 42 }
end

module Immutable = struct
  type t =
    { s : string
    ; i : int
    }
  [@@deriving hash, sexp_of]

  let v1 = { s = "hey"; i = 42 }
  let v2 = { s = "ho"; i = 42 }

  let%test_unit _ = check_hash_differently ~hash ~sexp_of_t v1 v2
end

module Immutable_hash_dot_ignore = struct
  type t =
    { s : string [@hash.ignore]
    ; i : int
    }
  [@@deriving hash, sexp_of]

  let v1 = { s = "hey"; i = 42 }
  let v2 = { s = "ho"; i = 42 }

  let%test_unit _ = check_hash_same ~hash ~sexp_of_t v1 v2
  let%test_unit _ = [%test_eq: int] (hash v1) No_string_field.(hash v1)
end

module Mutable_hash_dot_ignore = struct
  type t =
    { mutable s : string [@hash.ignore]
    ; i : int
    }
  [@@deriving hash, sexp_of]

  let v1 = { s = "hey"; i = 42 }
  let v2 = { s = "ho"; i = 42 }

  let%test_unit _ = check_hash_same ~hash ~sexp_of_t v1 v2
  let%test_unit _ = [%test_eq: int] (hash v1) No_string_field.(hash v1)
end

module Immutable_compare_ignore = struct
  type t =
    { s : string [@compare.ignore]
    ; i : int
    }
  [@@deriving hash, sexp_of]

  let v1 = { s = "hey"; i = 42 }
  let v2 = { s = "ho"; i = 42 }

  let%test_unit _ = check_hash_same ~hash ~sexp_of_t v1 v2
  let%test_unit _ = [%test_eq: int] (hash v1) No_string_field.(hash v1)
end
