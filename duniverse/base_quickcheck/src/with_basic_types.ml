open! Base

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
end
