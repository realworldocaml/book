open Ppx_hash_lib.Std
open Hash.Builtin

module Barbins_example = struct
  module X = struct
    type t = int [@@deriving hash]
  end

  module Y = struct
    type y = int [@@deriving hash]
  end

  module Z = struct
    type t = int [@@deriving hash]
  end

  module Example = struct
    module T = struct
      type t =
        { x : X.t
        ; y : Y.y
        ; z : Z.t
        ; mutable cached : int option [@hash.ignore]
        }
      [@@deriving hash]
    end

    module T_type = struct
      type t = T.t =
        { x : X.t
        ; y : Y.y
        ; z : Z.t
        ; mutable cached : int option
        }
    end

    module Unmemoized = T

    module Memoized = struct
      include T_type

      let hash t =
        match t.cached with
        | Some x -> x
        | None ->
          let cached = T.hash t in
          (* dont use state *)
          t.cached <- Some cached;
          cached
      ;;

      let hash_fold_t (state : Hash.state) (t : t) = hash_fold_int state (hash t)

      (* use state *)
    end
  end

  module Client = struct
    (* ensure overrides are typed correctly *)
    type t1 = Example.Unmemoized.t [@@deriving hash]
    type t3 = Example.Memoized.t [@@deriving hash]
  end
end
