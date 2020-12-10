(** A module internal to [Core_bench]. Please look at {!Bench}.

    Some basic linear algebra code, so that basic operations can be done without
    introducing a dependency on Lacaml/LAPACK.  Currently only has the minimum needed to
    do ordinary least squares.

    Matrices are represented via [float array array], in row-major order.
*)

open! Core

(** Vectors *)
module Vec : sig
  type t = float array [@@deriving sexp]

  (** Copy a vector *)
  val copy : t -> t

  (** [create0 len] sreate a vector of 0s of length [len]. *)
  val create0 : int -> t

  (** The sum of squares of entries in a vector *)
  val sumsq : t -> float

  (** The Euclidean length of a vector *)
  val norm : t -> float
end

(** Matrices *)
module Mat : sig
  type t = float array array [@@deriving sexp]

  (** Copy a matrix *)
  val copy : t -> t

  (** Create a matrix of 0s *)
  val create0 : rows:int -> cols:int -> t
  val create_per_row: rows:int -> cols:int -> f:(int -> float) -> t

  (** Extract a column.  Data is copied.  Indices start at 0. *)
  val get_column : t -> int -> Vec.t

end

(** [qr A] returns the QR-decomposition of [A] as a pair (Q,R). [A] must have
    at least as many rows as columns and have full rank.

    If [in_place] (default: [false]) is [true], then [A] is overwritten with [Q].
*)
val qr : ?in_place:bool -> Mat.t -> Mat.t * Mat.t

(** [triu_solve R b] solves R x = b where [R] is an m x m upper-triangular matrix
    and [b] is an m x 1 column vector.  *)
val triu_solve : Mat.t -> Vec.t -> Vec.t Or_error.t

(** [mul_mv A x] computes the product [A * x] (where [M] is a matrix and [x] is
    a column vector). *)
val mul_mv : ?transa:bool -> Mat.t -> Vec.t -> Vec.t

(** [ols A b] computes the ordinary least-squares solution to A x = b.  [A] must have at
    least as many rows as columns and have full rank.

    This can be used to compute solutions to non-singular square systems, but is somewhat
    sub-optimal for that purpose.  The algorithm is to factor A = Q * R and solve R x = Q'
    b where Q' denotes the transpose of Q.

    If [in_place] (default: [false]) is [true], then [A] will be destroyed.
*)
val ols : ?in_place:bool -> Mat.t -> Vec.t -> Vec.t Or_error.t
