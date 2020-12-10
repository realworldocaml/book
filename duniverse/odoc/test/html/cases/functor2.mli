(* test *)

module type S = sig type t end

module X : functor (Y:S) -> functor (Z:S) -> sig
   type y_t = Y.t
   type z_t = Z.t
   type x_t = y_t
end

module type XF = functor (Y:S) -> functor (Z:S) -> sig
   type y_t = Y.t
   type z_t = Z.t
   type x_t = y_t
end
