open! Import
include Bin_prot.Std
include Hash.Builtin

include (
  Base :
  sig
    type nonrec 'a array = 'a array [@@deriving sexp]
    type nonrec bool = bool [@@deriving sexp]
    type nonrec char = char [@@deriving sexp]
    type nonrec exn = exn [@@deriving sexp_of]
    type nonrec float = float [@@deriving sexp]
    type nonrec int = int [@@deriving sexp]
    type nonrec int32 = int32 [@@deriving sexp]
    type nonrec int64 = int64 [@@deriving sexp]
    type nonrec 'a list = 'a list [@@deriving sexp]
    type nonrec nativeint = nativeint [@@deriving sexp]
    type nonrec 'a option = 'a option [@@deriving sexp]
    type nonrec 'a ref = 'a ref [@@deriving sexp]
    type nonrec string = string [@@deriving sexp]
    type nonrec bytes = bytes [@@deriving sexp]
    type nonrec unit = unit [@@deriving sexp]
  end
  with type 'a array := 'a array
  with type bool := bool
  with type char := char
  with type exn := exn
  with type float := float
  with type int := int
  with type int32 := int32
  with type int64 := int64
  with type 'a list := 'a list
  with type nativeint := nativeint
  with type 'a option := 'a option
  with type 'a ref := 'a ref
  with type string := string
  with type bytes := bytes
  with type unit := unit)

include (
struct
  type 'a sexp_option = ('a Std_internal.sexp_option[@ocaml.warning "-3"])
  [@@deriving bin_io, compare, hash]

  type 'a sexp_list = ('a Std_internal.sexp_list[@ocaml.warning "-3"])
  [@@deriving bin_io, compare, hash]
end :
sig
  type 'a sexp_option = ('a Std_internal.sexp_option[@ocaml.warning "-3"])
  [@@deriving bin_io, compare, hash]

  type 'a sexp_list = ('a Std_internal.sexp_list[@ocaml.warning "-3"])
  [@@deriving bin_io, compare, hash]
end
with type 'a sexp_option := ('a Std_internal.sexp_option[@ocaml.warning "-3"])
with type 'a sexp_list := ('a Std_internal.sexp_list[@ocaml.warning "-3"]))

type 'a sexp_option = ('a Std_internal.sexp_option[@ocaml.warning "-3"])
[@@deprecated "[since 2019-03] use [@sexp.option] instead"]

type 'a sexp_list = ('a Std_internal.sexp_list[@ocaml.warning "-3"])
[@@deprecated "[since 2019-03] use [@sexp.list] instead"]
