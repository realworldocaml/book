open! Import
open Test_container
include (Test_S1 (Array) : sig end)
include (Test_S1 (List) : sig end)
include (Test_S1 (Queue) : sig end)
open Container
open T


(* The following functors exist as a consistency check among all the various [S?]
   interfaces.  They ensure that each particular [S?] is an instance of a more generic
   signature. *)
module Check
    (T : T1)
    (Elt : T1)
    (_ : Generic with type 'a t := 'a T.t with type 'a elt := 'a Elt.t) =
struct end

module _ (M : S0) =
  Check
    (struct
      type 'a t = M.t
    end)
    (struct
      type 'a t = M.elt
    end)
    (M)

module _ (M : S0_phantom) =
  Check
    (struct
      type 'a t = 'a M.t
    end)
    (struct
      type 'a t = M.elt
    end)
    (M)

module _ (M : S1) =
  Check
    (struct
      type 'a t = 'a M.t
    end)
    (struct
      type 'a t = 'a
    end)
    (M)

type phantom

module _ (M : S1_phantom) =
  Check
    (struct
      type 'a t = ('a, phantom) M.t
    end)
    (struct
      type 'a t = 'a
    end)
    (M)

module _ (M : S1_phantom_invariant) =
  Check
    (struct
      type 'a t = ('a, phantom) M.t
    end)
    (struct
      type 'a t = 'a
    end)
    (M)
