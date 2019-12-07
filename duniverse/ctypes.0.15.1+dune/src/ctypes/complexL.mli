type t
(** The type of long double complex values *)

val make : LDouble.t -> LDouble.t -> t
(** [make x y] creates the long double complex value [x + y * i] *)

val of_complex : Complex.t -> t
(** create a long double complex from a Complex.t *)

val to_complex : t -> Complex.t 
(** Convert a long double complex to a Complex.t.  The real and imaginary components
    are converted by calling [LDouble.to_float] which can produce unspecified results. *)

val zero : t
(** [0 + i0] *)

val one : t
(** [1 + i0] *)

val i : t
(** [0 + i] *)

val re : t -> LDouble.t 
(** return the real part of the long double complex *)

val im : t -> LDouble.t 
(** return the imaginary part of the long double complex *)

val neg : t -> t
(** Unary negation *)

val conj : t -> t
(** Conjugate: given the complex [x + i.y], returns [x - i.y]. *) 

val add : t -> t -> t
(** Addition *)

val sub : t -> t -> t
(** Subtraction *)

val mul : t -> t -> t
(** Multiplication *)

val div : t -> t -> t
(** Division *)

val inv : t -> t
(** Multiplicative inverse ([1/z]). *)

val sqrt : t -> t
(** Square root. *)

val norm2 : t -> LDouble.t
(** Norm squared: given [x + i.y], returns [x^2 + y^2]. *)

val norm : t -> LDouble.t
(** Norm: given [x + i.y], returns [sqrt(x^2 + y^2)]. *)

val polar : LDouble.t -> LDouble.t -> t
(** [polar norm arg] returns the complex having norm [norm] and argument [arg]. *)

val arg : t -> LDouble.t
(** Argument.  The argument of a complex number is the angle
    in the complex plane between the positive real axis and a line
    passing through zero and the number. *)

val exp : t -> t 
(** Exponentiation.  [exp z] returns [e] to the [z] power. *)

val log : t -> t 
(** Natural logarithm (in base [e]). *)

val pow : t -> t -> t
(** Power function.  [pow z1 z2] returns [z1] to the [z2] power. *)

