open! Import

let invalid_argf = Printf.invalid_argf

module T = struct
  type t = bool [@@deriving_inline compare, enumerate, hash, sexp]
  let compare = (compare_bool : t -> t -> int)
  let all = ([false; true] : t list)
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_bool
  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_bool in fun x -> func x
  let t_of_sexp = (bool_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_bool : t -> Ppx_sexp_conv_lib.Sexp.t)
  [@@@end]

  let of_string = function
    | "true" -> true
    | "false" -> false
    | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
  ;;

  let to_string = Caml.string_of_bool
end

include T
include Comparator.Make (T)
include Comparable.Validate (T)

include Pretty_printer.Register (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Bool"
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Bool_replace_polymorphic_compare

let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

let clamp_exn t ~min ~max =
  assert (min <= max);
  clamp_unchecked t ~min ~max
;;

let clamp t ~min ~max =
  if min > max
  then
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
  else Ok (clamp_unchecked t ~min ~max)
;;

let to_int x = bool_to_int x

module Non_short_circuiting = struct
  (* We don't expose this, since we don't want to break the invariant mentioned below of
     (to_int true = 1) and (to_int false = 0). *)
  let unsafe_of_int (x : int) : bool = Caml.Obj.magic x
  let ( || ) a b = unsafe_of_int (to_int a lor to_int b)
  let ( && ) a b = unsafe_of_int (to_int a land to_int b)
end

(* We do this as a direct assert on the theory that it's a cheap thing to test and a
   really core invariant that we never expect to break, and we should be happy for a
   program to fail immediately if this is violated. *)
let () = assert (Poly.( = ) (to_int true) 1 && Poly.( = ) (to_int false) 0)

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Bool_replace_polymorphic_compare
