(** Foo. *)

val foo : unit
(** The module needs at least one signature item, otherwise a bug causes the
    compiler to drop the module comment (above). See
    {{:https://caml.inria.fr/mantis/view.php?id=7701}}. *)

module type S =
sig
  type t
  type u
  type 'a v
  type ('a, 'b) w
  module M : sig end
end

module type S1

module type S2 = S

module type S3 = S with type t = int and type u = string

module type S4 = S with type t := int

module type S5 = S with type 'a v := 'a list

type ('a, 'b) result

module type S6 = S with type ('a, 'b) w := ('a, 'b) result

module M' : sig end

module type S7 = S with module M = M'

module type S8 = S with module M := M'

module type S9 = module type of M'

open M'

open! M'

module rec Mutually : sig end
and Recursive : sig end
