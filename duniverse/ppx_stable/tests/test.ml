module Basic_record = struct
  type a
  type b1
  type b2
  type c1
  type c2
  type d1
  type d2

  module V1 = struct
    type t =
      { a : a
      ; b1 : b1
      ; c : c1
      ; d : d1
      }
  end

  module V2 = struct
    type t =
      { a : a
      ; b2 : b2
      ; c : c2
      ; d : d2 * a
      }
    [@@deriving_inline
      stable_record ~version:V1.t ~add:[ b1 ] ~remove:[ b2 ] ~modify:[ c ] ~set:[ d ]]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) ~modify_c ~d ~b1 =
      let ({ a; b2 = _; c; d = _ } : t) = _t in
      ({ a; b1; c = modify_c c; d } : V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) ~modify_c ~d ~b2 =
      let ({ a; b1 = _; c; d = _ } : V1.t) = _t in
      ({ a; b2; c = modify_c c; d } : t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Types_not_named_t_and_nested = struct
  module V1 = struct
    module A = struct
      type a =
        { x : bool
        ; y : int
        }
    end
  end

  module V2 = struct
    module B = struct
      type b =
        { x : int
        ; y : int
        }
      [@@deriving_inline stable_record ~version:V1.A.a ~modify:[ x ]]

      let _ = fun (_ : b) -> ()

      let b_to_V1_A_a (_t : b) ~modify_x =
        let ({ x; y } : b) = _t in
        ({ x = modify_x x; y } : V1.A.a)
      ;;

      let _ = b_to_V1_A_a

      let b_of_V1_A_a (_t : V1.A.a) ~modify_x =
        let ({ x; y } : V1.A.a) = _t in
        ({ x = modify_x x; y } : b)
      ;;

      let _ = b_of_V1_A_a

      [@@@end]
    end
  end
end

module Nothing_to_convert = struct
  module V1 = struct
    type t = { x : bool }
  end

  module V2 = struct
    type t = { x : bool } [@@deriving_inline stable_record ~version:V1.t]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) =
      let ({ x } : t) = _t in
      ({ x } : V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) =
      let ({ x } : V1.t) = _t in
      ({ x } : t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Basic_variant = struct
  module V1 = struct
    type t = X [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~x:x_fun = function
            | X -> x_fun ()
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t = Y [@@deriving_inline stable_variant ~version:V1.t ~add:[ X ] ~remove:[ Y ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~y:y_fun = function
            | Y -> y_fun ()
          ;;

          let _ = map
        end
      end

      let to_V1_t ~remove_Y (v : t) : V1.t = Stable_variant.Helper.map v ~y:remove_Y
      let _ = to_V1_t
      let of_V1_t ~remove_X (v : V1.t) : t = V1.Stable_variant.Helper.map v ~x:remove_X
      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Basic_variant2 = struct
  type a
  type b
  type c
  type d
  type e
  type f
  type g
  type h
  type i
  type j
  type k
  type l

  module V1 = struct
    type t =
      | I0
      | I1 of a
      | I2 of b * c
      | X1
      | X2 of j
      | X3 of k * l
      | Z1 of d * e
      | Z2 of f
      | Z3
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
                ~i0:i0_fun
                ~i1:i1_fun
                ~i2:i2_fun
                ~x1:x1_fun
                ~x2:x2_fun
                ~x3:x3_fun
                ~z1:z1_fun
                ~z2:z2_fun
                ~z3:z3_fun
            = function
              | I0 -> i0_fun ()
              | I1 v0 -> i1_fun v0
              | I2 (v0, v1) -> i2_fun v0 v1
              | X1 -> x1_fun ()
              | X2 v0 -> x2_fun v0
              | X3 (v0, v1) -> x3_fun v0 v1
              | Z1 (v0, v1) -> z1_fun v0 v1
              | Z2 v0 -> z2_fun v0
              | Z3 -> z3_fun ()
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | I0
      | I1 of a
      | I2 of b * c
      | Y1
      | Y2 of g
      | Y3 of h * i
      | Z1
      | Z2 of f
      | Z3 of d * e
    [@@deriving_inline
      stable_variant
        ~version:V1.t
        ~remove:[ Y1; Y2; Y3 ]
        ~add:[ X1; X2; X3 ]
        ~modify:[ Z1; Z2; Z3 ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
                ~i0:i0_fun
                ~i1:i1_fun
                ~i2:i2_fun
                ~y1:y1_fun
                ~y2:y2_fun
                ~y3:y3_fun
                ~z1:z1_fun
                ~z2:z2_fun
                ~z3:z3_fun
            = function
              | I0 -> i0_fun ()
              | I1 v0 -> i1_fun v0
              | I2 (v0, v1) -> i2_fun v0 v1
              | Y1 -> y1_fun ()
              | Y2 v0 -> y2_fun v0
              | Y3 (v0, v1) -> y3_fun v0 v1
              | Z1 -> z1_fun ()
              | Z2 v0 -> z2_fun v0
              | Z3 (v0, v1) -> z3_fun v0 v1
          ;;

          let _ = map
        end
      end

      let to_V1_t
            ~modify_Z1
            ~modify_Z2
            ~modify_Z3
            ~remove_Y3
            ~remove_Y2
            ~remove_Y1
            (v : t)
        : V1.t
        =
        Stable_variant.Helper.map
          v
          ~i0:(fun () -> V1.I0)
          ~i1:(fun v0 -> V1.I1 v0)
          ~i2:(fun v0 v1 -> V1.I2 (v0, v1))
          ~y1:remove_Y1
          ~y2:remove_Y2
          ~y3:remove_Y3
          ~z1:modify_Z1
          ~z2:modify_Z2
          ~z3:modify_Z3
      ;;

      let _ = to_V1_t

      let of_V1_t
            ~modify_Z1
            ~modify_Z2
            ~modify_Z3
            ~remove_X3
            ~remove_X2
            ~remove_X1
            (v : V1.t)
        : t
        =
        V1.Stable_variant.Helper.map
          v
          ~i0:(fun () -> I0)
          ~i1:(fun v0 -> I1 v0)
          ~i2:(fun v0 v1 -> I2 (v0, v1))
          ~x1:remove_X1
          ~x2:remove_X2
          ~x3:remove_X3
          ~z1:modify_Z1
          ~z2:modify_Z2
          ~z3:modify_Z3
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Types_not_named_t_and_nested_variant = struct
  module V1 = struct
    module A = struct
      type a =
        | X of bool
        | Y
      [@@deriving_inline stable_variant]

      include struct
        [@@@ocaml.warning "-60"]

        let _ = fun (_ : a) -> ()

        module Stable_variant_of_a = struct
          module Helper = struct
            let map ~x:x_fun ~y:y_fun = function
              | X v0 -> x_fun v0
              | Y -> y_fun ()
            ;;

            let _ = map
          end
        end
      end [@@ocaml.doc "@inline"]

      [@@@end]
    end
  end

  module V2 = struct
    module B = struct
      type b =
        | X of int
        | Y
      [@@deriving_inline stable_variant ~version:V1.A.a ~modify:[ X ]]

      include struct
        [@@@ocaml.warning "-60"]

        let _ = fun (_ : b) -> ()

        module Stable_variant_of_b = struct
          module Helper = struct
            let map ~x:x_fun ~y:y_fun = function
              | X v0 -> x_fun v0
              | Y -> y_fun ()
            ;;

            let _ = map
          end
        end

        let b_to_V1_A_a ~modify_X (v : b) : V1.A.a =
          Stable_variant_of_b.Helper.map v ~x:modify_X ~y:(fun () -> V1.A.Y)
        ;;

        let _ = b_to_V1_A_a

        let b_of_V1_A_a ~modify_X (v : V1.A.a) : b =
          V1.A.Stable_variant_of_a.Helper.map v ~x:modify_X ~y:(fun () -> Y)
        ;;

        let _ = b_of_V1_A_a
      end [@@ocaml.doc "@inline"]

      [@@@end]
    end
  end
end

module Nothing_to_convert_variant = struct
  module V1 = struct
    type t = X [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~x:x_fun = function
            | X -> x_fun ()
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t = X [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~x:x_fun = function
            | X -> x_fun ()
          ;;

          let _ = map
        end
      end

      let to_V1_t (v : t) : V1.t = Stable_variant.Helper.map v ~x:(fun () -> V1.X)
      let _ = to_V1_t
      let of_V1_t (v : V1.t) : t = V1.Stable_variant.Helper.map v ~x:(fun () -> X)
      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Inline_record_generic = struct
  module V1 = struct
    type t =
      | I of
          { x : int
          ; y : float
          }
      | X of { x : int }
      | Y of
          { y : float
          ; y1 : bool
          }
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~i:i_fun ~x:x_fun ~y:y_fun = function
            | I { x = v0; y = v1 } -> i_fun ~x:v0 ~y:v1
            | X { x = v0 } -> x_fun ~x:v0
            | Y { y = v0; y1 = v1 } -> y_fun ~y:v0 ~y1:v1
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | I of
          { x : int
          ; y : float
          }
      | X of { x : float }
      | Z of
          { z : float * int
          ; z1 : float * bool
          }
    [@@deriving_inline
      stable_variant ~version:V1.t ~add:[ Y ] ~remove:[ Z ] ~modify:[ X ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~i:i_fun ~x:x_fun ~z:z_fun = function
            | I { x = v0; y = v1 } -> i_fun ~x:v0 ~y:v1
            | X { x = v0 } -> x_fun ~x:v0
            | Z { z = v0; z1 = v1 } -> z_fun ~z:v0 ~z1:v1
          ;;

          let _ = map
        end
      end

      let to_V1_t ~modify_X ~remove_Z (v : t) : V1.t =
        Stable_variant.Helper.map
          v
          ~i:(fun ~x:v0 ~y:v1 -> V1.I { x = v0; y = v1 })
          ~x:modify_X
          ~z:remove_Z
      ;;

      let _ = to_V1_t

      let of_V1_t ~modify_X ~remove_Y (v : V1.t) : t =
        V1.Stable_variant.Helper.map
          v
          ~i:(fun ~x:v0 ~y:v1 -> I { x = v0; y = v1 })
          ~x:modify_X
          ~y:remove_Y
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Inline_record_field_name_matches_constructor = struct
  module V1 = struct
    type t = X of { x : float } [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~x:x_fun = function
            | X { x = v0 } -> x_fun ~x:v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t = X of { x : int }
    [@@deriving_inline stable_variant ~version:V1.t ~modify:[ X ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~x:x_fun = function
            | X { x = v0 } -> x_fun ~x:v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~modify_X (v : t) : V1.t = Stable_variant.Helper.map v ~x:modify_X
      let _ = to_V1_t
      let of_V1_t ~modify_X (v : V1.t) : t = V1.Stable_variant.Helper.map v ~x:modify_X
      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Inline_record_and_tuple = struct
  module V1 = struct
    type t =
      | X of { x : float }
      | Y of int * int
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~x:x_fun ~y:y_fun = function
            | X { x = v0 } -> x_fun ~x:v0
            | Y (v0, v1) -> y_fun v0 v1
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Mutable_record_field = struct
  module V1 = struct
    type t = { x : float }
  end

  module V2 = struct
    type t = { mutable x : int }
    [@@deriving_inline stable_record ~version:V1.t ~modify:[ x ]]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) ~modify_x =
      let ({ x } : t) = _t in
      ({ x = modify_x x } : V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) ~modify_x =
      let ({ x } : V1.t) = _t in
      ({ x = modify_x x } : t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end
