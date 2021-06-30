(** [Ordering] is intended to make code that matches on the result of a comparison
    more concise and easier to read.

    For example, instead of writing:

    {[
      let r = compare x y in
      if r < 0 then
        ...
      else if r = 0 then
        ...
      else
        ...
    ]}

    you could simply write:

    {[
      match Ordering.of_int (compare x y) with
      | Less -> ...
      | Equal -> ...
      | Greater -> ...
    ]}

*)

open! Import

type t =
  | Less
  | Equal
  | Greater
[@@deriving_inline compare, enumerate, hash, sexp]

val compare : t -> t -> int
val all : t list
val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

include Ppx_sexp_conv_lib.Sexpable.S with type t := t

[@@@end]

include Equal.S with type t := t

(** [of_int n] is:

    {v
      Less     if n < 0
      Equal    if n = 0
      Greater  if n > 0
    v} *)
val of_int : int -> t

(** [to_int t] is:

    {v
      Less     -> -1
      Equal    -> 0
      Greater  -> 1
    v}

    It can be useful when writing a comparison function to allow one to return
    [Ordering.t] values and transform them to [int]s later. *)
val to_int : t -> int

module Export : sig
  type _ordering = t =
    | Less
    | Equal
    | Greater
end
