open Std_internal

module type S = sig
  type t

  val typerep_of_t  : t Typerep.t
  val typename_of_t : t Typename.t
end

module type S1 = sig
  type 'a t

  val typerep_of_t  : 'a Typerep.t  -> 'a t Typerep.t
  val typename_of_t : 'a Typename.t -> 'a t Typename.t
end

module type S2 = sig
  type ('a, 'b) t

  val typerep_of_t  : 'a Typerep.t  -> 'b Typerep.t  -> ('a, 'b) t Typerep.t
  val typename_of_t : 'a Typename.t -> 'b Typename.t -> ('a, 'b) t Typename.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val typerep_of_t :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t
    -> ('a, 'b, 'c) t Typerep.t
  val typename_of_t :
    'a Typename.t -> 'b Typename.t -> 'c Typename.t
    -> ('a, 'b, 'c) t Typename.t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val typerep_of_t :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t
    -> ('a, 'b, 'c, 'd) t Typerep.t
  val typename_of_t :
    'a Typename.t -> 'b Typename.t -> 'c Typename.t -> 'd Typename.t
    -> ('a, 'b, 'c, 'd) t Typename.t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val typerep_of_t :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t -> 'e Typerep.t
    -> ('a, 'b, 'c, 'd, 'e) t Typerep.t
  val typename_of_t :
    'a Typename.t -> 'b Typename.t -> 'c Typename.t -> 'd Typename.t -> 'e Typename.t
    -> ('a, 'b, 'c, 'd, 'e) t Typename.t
end
