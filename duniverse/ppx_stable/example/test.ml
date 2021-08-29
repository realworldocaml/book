module Records = struct
  module V1 = struct
    type t =
      { x1 : float
      ; y1 : int
      ; z1 : int
      }
  end

  module V2 = struct
    type t =
      { x1 : int
      ; y2 : int
      ; z1 : int
      }
    [@@deriving
      stable_record ~version:V1.t ~add:[ y1 ] ~remove:[ y2 ] ~modify:[ x1 ] ~set:[ z1 ]]
  end

  let downgrade t = V2.to_V1_t t ~y1:t.y2 ~modify_x1:Base.Float.of_int ~z1:(t.x1 + t.y2)
end

module Variants = struct
  module V1 = struct
    type t =
      | I0
      | I1 of bool
      | I2 of bool * bool
      | X0
      | X1 of int
      | X2 of int * int
      | Z1 of float * float
      | Z2 of float
      | Z3
      | T of char option * bool option
    [@@deriving stable_variant]
  end

  module V2 = struct
    type t =
      | I0
      | I1 of bool
      | I2 of bool * bool
      | X0
      | X1 of int
      | X2 of int * int
      | Y1
      | Y2 of int
      | Y3 of int * int
      | Z1
      | Z2 of int
      | Z3 of int * int
      | T1 of char
      | T2 of bool
    [@@deriving
      stable_variant
        ~version:V1.t
        ~remove:[ Y1; Y2; Y3; T1; T2 ]
        ~modify:[ Z1; Z2; Z3 ]
        ~add:[ T ]]
  end

  let v1_of_t (v2 : V2.t) : V1.t =
    V2.to_V1_t
      ~remove_Y1:(fun () -> failwith "Can't convert Y1 to V1.t")
      ~remove_Y2:(fun y -> X1 y)
      ~remove_Y3:(fun y0 y1 -> X2 (y0, y1))
      ~modify_Z1:(fun () -> Z1 (0., 1.))
      ~modify_Z2:(fun z -> Z2 (Base.Float.of_int z))
      ~modify_Z3:(fun z1 z2 ->
        ignore (z1, z2);
        Z3)
      ~remove_T1:(fun c -> T (Some c, None))
      ~remove_T2:(fun b -> T (None, Some b))
      v2
  ;;
end

module X0 = struct
  type t = int

  let of_int x = x
end

module X1 = struct
  type t = int

  let of_int x = x
end

module X0a = struct
  type t = int

  let of_int x = x
  let of_x0b x = x
end

module X0b = struct
  type t = float

  let of_int x = Base.Float.of_int x
  let of_x0a (_ : X0a.t) = (assert false : t)
end

module X = struct
  module V1 = struct
    type t = X0 of X0a.t [@@deriving stable_variant ~version:t]
  end

  module V2 = struct
    type t = X0 of X0b.t [@@deriving stable_variant ~version:V1.t ~modify:[ X0 ]]
  end

  let convert_of_v1 (v1 : V1.t) : V2.t =
    V2.of_V1_t ~modify_X0:(fun v -> X0 (X0b.of_x0a v)) v1
  ;;
end

module Inline_record = struct
  module V1 = struct
    type t =
      | E
      | X of
          { v1 : float
          ; v0 : int
          }
      | Z of int * float
      | Y of { y : float }
      | I of { y : float }
    [@@deriving stable_variant]
  end

  module V2 = struct
    type t =
      | X of { v0 : int }
      | I of { y : float }
    [@@deriving stable_variant ~version:V1.t ~modify:[ X ] ~add:[ Y; Z; E ]]
  end
end

module Both_kinds = struct
  module V1 = struct
    type t =
      | X of int
      | Y of { y : float }
    [@@deriving stable_variant]
  end
end
