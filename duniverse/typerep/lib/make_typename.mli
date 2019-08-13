open Std_internal

(*
  Typerep.Named generation helpers
*)

module Make0 (X : Named_intf.S0) : sig
  val named         : X.t Typerep.Named.t
  val typename_of_t : X.t Typename.t
end

module Make1 (X : Named_intf.S1) : sig
  val named         : 'a Typerep.t  -> 'a X.t Typerep.Named.t
  val typename_of_t : 'a Typename.t -> 'a X.t Typename.t
end

module Make2 (X : Named_intf.S2) : sig
  val named         : 'a Typerep.t  -> 'b Typerep.t  -> ('a, 'b) X.t Typerep.Named.t
  val typename_of_t : 'a Typename.t -> 'b Typename.t -> ('a, 'b) X.t Typename.t
end

module Make3 (X : Named_intf.S3) : sig
  val named :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t
    -> ('a, 'b, 'c) X.t Typerep.Named.t
  val typename_of_t :
    'a Typename.t -> 'b Typename.t -> 'c Typename.t
    -> ('a, 'b, 'c) X.t Typename.t
end

module Make4 (X : Named_intf.S4) : sig
  val named :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t
    -> ('a, 'b, 'c, 'd) X.t Typerep.Named.t
  val typename_of_t :
    'a Typename.t -> 'b Typename.t -> 'c Typename.t -> 'd Typename.t
    -> ('a, 'b, 'c, 'd) X.t Typename.t
end

module Make5 (X : Named_intf.S5) : sig
  val named :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t -> 'e Typerep.t
    -> ('a, 'b, 'c, 'd, 'e) X.t Typerep.Named.t
  val typename_of_t :
    'a Typename.t -> 'b Typename.t -> 'c Typename.t -> 'd Typename.t -> 'e Typename.t
    -> ('a, 'b, 'c, 'd, 'e) X.t Typename.t
end
