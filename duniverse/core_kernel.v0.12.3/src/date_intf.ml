module type Date = sig

  (**/**)

  type t = Date0.t

  (**/**)

  (** @inline *)
  include module type of Date0 with type t := t

  val of_time : Time_float.t -> zone:Time_float.Zone.t -> t
  val today : zone:Time_float.Zone.t -> t
end
