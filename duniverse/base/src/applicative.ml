open! Import
include Applicative_intf
module List = List0

(** This module serves mostly as a partial check that [S2] and [S] are in sync, but
    actually calling it is occasionally useful. *)
module S_to_S2 (X : S) : S2 with type ('a, 'e) t = 'a X.t = struct
  include X

  type ('a, 'e) t = 'a X.t
end

module S2_to_S (X : S2) : S with type 'a t = ('a, unit) X.t = struct
  include X

  type 'a t = ('a, unit) X.t
end

module S2_to_S3 (X : S2) : S3 with type ('a, 'd, 'e) t = ('a, 'd) X.t = struct
  include X

  type ('a, 'd, 'e) t = ('a, 'd) X.t
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'd) t = ('a, 'd, unit) X.t = struct
  include X

  type ('a, 'd) t = ('a, 'd, unit) X.t
end

module Make3 (X : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t = struct
  include X

  let ( <*> ) = apply
  let derived_map t ~f = return f <*> t

  let map =
    match X.map with
    | `Define_using_apply -> derived_map
    | `Custom x -> x
  ;;

  let ( >>| ) t f = map t ~f
  let map2 ta tb ~f = map ~f ta <*> tb
  let map3 ta tb tc ~f = map ~f ta <*> tb <*> tc
  let all ts = List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
  let both ta tb = map2 ta tb ~f:(fun a b -> a, b)
  let ( *> ) u v = return (fun () y -> y) <*> u <*> v
  let ( <* ) u v = return (fun x () -> x) <*> u <*> v
  let all_unit ts = List.fold ts ~init:(return ()) ~f:( *> )

  module Applicative_infix = struct
    let ( <*> ) = ( <*> )
    let ( *> ) = ( *> )
    let ( <* ) = ( <* )
    let ( >>| ) = ( >>| )
  end
end

module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t = Make3 (struct
    include X

    type ('a, 'd, 'e) t = ('a, 'd) X.t
  end)

module Make (X : Basic) : S with type 'a t := 'a X.t = Make2 (struct
    include X

    type ('a, 'e) t = 'a X.t
  end)

module Make_let_syntax3
    (X : For_let_syntax3) (Intf : sig
                             module type S
                           end)
    (Impl : Intf.S) =
struct
  module Let_syntax = struct
    include X

    module Let_syntax = struct
      include X
      module Open_on_rhs = Impl
    end
  end
end

module Make_let_syntax2
    (X : For_let_syntax2) (Intf : sig
                             module type S
                           end)
    (Impl : Intf.S) =
  Make_let_syntax3
    (struct
      include X

      type ('a, 'd, _) t = ('a, 'd) X.t
    end)
    (Intf)
    (Impl)

module Make_let_syntax
    (X : For_let_syntax) (Intf : sig
                            module type S
                          end)
    (Impl : Intf.S) =
  Make_let_syntax2
    (struct
      include X

      type ('a, _) t = 'a X.t
    end)
    (Intf)
    (Impl)

(** This functor closely resembles [Make3], and indeed it could be implemented
    much shorter in terms of [Make3]. However, we implement it by hand so that
    the resulting functions are more efficient, e.g. using [map2] directly instead of
    defining [apply] in terms of it and then [map2] in terms of that. For most
    applicatives this does not matter, but for some (such as Bonsai.Value.t), it has a
    larger impact. *)
module Make3_using_map2 (X : Basic3_using_map2) :
  S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t = struct
  include X

  let apply tf ta = map2 tf ta ~f:(fun f a -> f a)
  let ( <*> ) = apply
  let derived_map t ~f = return f <*> t

  let map =
    match X.map with
    | `Define_using_map2 -> derived_map
    | `Custom x -> x
  ;;

  let ( >>| ) t f = map t ~f
  let both ta tb = map2 ta tb ~f:(fun a b -> a, b)
  let map3 ta tb tc ~f = map2 (map2 ta tb ~f) tc ~f:(fun fab c -> fab c)
  let all ts = List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
  let ( *> ) u v = map2 u v ~f:(fun () y -> y)
  let ( <* ) u v = map2 u v ~f:(fun x () -> x)
  let all_unit ts = List.fold ts ~init:(return ()) ~f:( *> )

  module Applicative_infix = struct
    let ( <*> ) = ( <*> )
    let ( *> ) = ( *> )
    let ( <* ) = ( <* )
    let ( >>| ) = ( >>| )
  end
end

module Make2_using_map2 (X : Basic2_using_map2) :
  S2 with type ('a, 'e) t := ('a, 'e) X.t = Make3_using_map2 (struct
    include X

    type ('a, 'd, 'e) t = ('a, 'd) X.t
  end)

module Make_using_map2 (X : Basic_using_map2) : S with type 'a t := 'a X.t =
  Make2_using_map2 (struct
    include X

    type ('a, 'e) t = 'a X.t
  end)

module Of_monad2 (M : Monad.S2) : S2 with type ('a, 'e) t := ('a, 'e) M.t = Make2 (struct
    type ('a, 'e) t = ('a, 'e) M.t

    let return = M.return
    let apply mf mx = M.bind mf ~f:(fun f -> M.map mx ~f)
    let map = `Custom M.map
  end)

module Of_monad (M : Monad.S) : S with type 'a t := 'a M.t = Of_monad2 (struct
    include M

    type ('a, _) t = 'a M.t
  end)

module Compose (F : S) (G : S) : S with type 'a t = 'a F.t G.t = struct
  type 'a t = 'a F.t G.t

  include Make (struct
      type nonrec 'a t = 'a t

      let return a = G.return (F.return a)
      let apply tf tx = G.apply (G.map ~f:F.apply tf) tx
      let custom_map t ~f = G.map ~f:(F.map ~f) t
      let map = `Custom custom_map
    end)
end

module Pair (F : S) (G : S) : S with type 'a t = 'a F.t * 'a G.t = struct
  type 'a t = 'a F.t * 'a G.t

  include Make (struct
      type nonrec 'a t = 'a t

      let return a = F.return a, G.return a
      let apply tf tx = F.apply (fst tf) (fst tx), G.apply (snd tf) (snd tx)
      let custom_map t ~f = F.map ~f (fst t), G.map ~f (snd t)
      let map = `Custom custom_map
    end)
end
