open! Base

module type S_bigarray = sig
  (** This helper module type exists separately just to [open Bigarray] in its scope. *)
  open Bigarray

  type 'a t

  val bigstring : (char, int8_unsigned_elt, c_layout) Array1.t t
  val float32_vec : (float, float32_elt, fortran_layout) Array1.t t
  val float64_vec : (float, float64_elt, fortran_layout) Array1.t t
  val float32_mat : (float, float32_elt, fortran_layout) Array2.t t
  val float64_mat : (float, float64_elt, fortran_layout) Array2.t t
end

module type S = sig
  type 'a t

  val unit : unit t
  val bool : bool t
  val char : char t
  val string : string t
  val int : int t
  val int32 : int32 t
  val int63 : Int63.t t
  val int64 : int64 t
  val nativeint : nativeint t
  val float : float t
  val sexp : Sexp.t t
  val option : 'a t -> 'a option t
  val list : 'a t -> 'a list t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  val result : 'a t -> 'b t -> ('a, 'b) Result.t t

  include S_bigarray with type 'a t := 'a t (** @inline *)
end
