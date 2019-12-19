open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

module Body = struct
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving sexp_of, compare]

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
[@@deriving sexp_of, compare]

module Raw = struct
  type nonrec t = string t [@@deriving sexp_of, compare]
end

let map_pretty t ~f = { t with body = Body.map_pretty t.body ~f }
