open! Import

module List = List0

include Monad_intf

module type Basic_general = sig
  type ('a, 'i, 'j, 'd, 'e) t

  val bind
    :  ('a, 'i, 'j, 'd, 'e) t
    -> f:('a -> ('b, 'j, 'k, 'd, 'e) t)
    -> ('b, 'i, 'k, 'd, 'e) t

  val map
    : [ `Define_using_bind
      | `Custom of (('a, 'i, 'j, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'i, 'j, 'd, 'e) t)
      ]

  val return : 'a -> ('a, 'i, 'i, 'd, 'e) t
end

module Make_general (M : Basic_general) = struct

  let bind   = M.bind
  let return = M.return

  let map_via_bind ma ~f = M.bind ma ~f:(fun a -> M.return (f a))

  let map =
    match M.map with
    | `Define_using_bind -> map_via_bind
    | `Custom x -> x

  module Monad_infix = struct
    let (>>=) t f = bind t ~f
    let (>>|) t f = map  t ~f
  end
  include Monad_infix

  module Let_syntax = struct

    let return = return
    include Monad_infix

    module Let_syntax = struct
      let return = return
      let bind   = bind
      let map    = map
      let both a b = a >>= fun a -> b >>| fun b -> (a, b)
      module Open_on_rhs  = struct end
    end
  end

  let join t = t >>= fun t' -> t'

  let ignore_m t = map t ~f:(fun _ -> ())

  let all =
    let rec loop vs = function
      | [] -> return (List.rev vs)
      | t :: ts -> t >>= fun v -> loop (v :: vs) ts
    in
    fun ts -> loop [] ts

  let rec all_unit = function
    | [] -> return ()
    | t :: ts -> t >>= fun () -> all_unit ts

  let all_ignore = all_unit

end

module Make_indexed (M : Basic_indexed)
  : S_indexed with type ('a, 'i, 'j) t := ('a, 'i, 'j) M.t =
  Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = ('a, 'i, 'j) M.t
    include (M : Basic_indexed with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t)
  end)

module Make3 (M : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) M.t =
  Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = ('a, 'd, 'e) M.t
    include (M : Basic3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t)
  end)

module Make2 (M : Basic2) : S2 with type ('a, 'd) t := ('a, 'd) M.t =
  Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = ('a, 'd) M.t
    include (M : Basic2 with type ('a, 'b) t := ('a, 'b) M.t)
  end)

module Make (M : Basic) : S with type 'a t := 'a M.t =
  Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = 'a M.t
    include (M : Basic with type 'a t := 'a M.t)
  end)

module Ident = struct
  type 'a t = 'a
  include Make (struct
      type nonrec 'a t = 'a t
      let bind a ~f = f a
      let return a = a
      let map = `Custom (fun a ~f -> f a)
    end)
end
