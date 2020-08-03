
(** This comment needs to be here before #235 is fixed. *)

(** {1 Module} *)

(** This is module X.

    Some additional comments. *)
module X : sig

  (** {1 Type} *)

  type t
  (** Some type. *)

  (** {1 Values} *)

  val x : t
  (** The value of x. *)
end


(** {1 Module type} *)

(** This is module type Y.

    Some additional comments. *)
module type Y = sig

  (** {1 Type} *)

  type t
  (** Some type. *)

  (** {1 Values} *)

  val y : t
  (** The value of y. *)
end


(** {1 Functor} *)

(** This is a functor F.

    Some additional comments. *)
module F
    (Arg1 : Y) (Arg2 : sig
    (** {1 Type} *)

    type t
    (** Some type. *)
  end) : sig
  (** {1 Type} *)

  type t = Arg1.t * Arg2.t
  (** Some type. *)
end


(** {1 Class} *)

(** This is class z.

    Some additional comments. *)
class virtual z : object

  val y : int
  (** Some value. *)

  val mutable virtual y' : int

  (** {1 Methods} *)

  method z : int
  (** Some method. *)

  method private virtual z' : int
end


class virtual inherits : object
  inherit z
end
