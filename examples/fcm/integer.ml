open Core.Std

module type INTEGER = sig
  type t
  val to_string : t -> string
  val zero : t
  val one : t
  val ( = ) : t -> t -> bool
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val neg : t -> t
end

let double (type a) m x =
  let module I = (val m : INTEGER with type t = a) in
  I.(x + x)
;;

let int64 = (module Int64  : INTEGER with type t = Int64.t)

let x = double int64 3L
