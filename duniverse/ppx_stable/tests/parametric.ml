(******************************************************************************)
(*                                                                            *)
(* First we duplicate some of the examples from test.mlt, but changing their  *)
(* use of fresh abstract types for type parameters.                           *)
(*                                                                            *)
(******************************************************************************)

module Basic_record = struct
  module V1 = struct
    type ('a, 'b, 'c, 'd) t =
      { a : 'a
      ; b1 : 'b
      ; c : 'c
      ; d : 'd
      }
  end

  module V2 = struct
    type ('a, 'b, 'c, 'd) t =
      { a : 'a
      ; b2 : 'b
      ; c : 'c
      ; d : 'd * 'a
      }
    [@@deriving
      stable_record
        ~version:[%stable: ('a, 'b, 'c, 'd) V1.t]
        ~add:[ b1 ]
        ~remove:[ b2 ]
        ~modify:[ c ]
        ~set:[ d ]]
  end
end

module Basic_variant = struct
  module V1 = struct
    type ('a, 'b, 'c, 'd, 'e, 'f, 'j, 'k, 'l) t =
      | I0
      | I1 of 'a
      | I2 of 'b * 'c
      | X1
      | X2 of 'j
      | X3 of 'k * 'l
      | Z1 of 'd * 'e
      | Z2 of 'f
      | Z3
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : ('a, 'b, 'c, 'd, 'e, 'f, 'j, 'k, 'l) t) -> ()

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
    type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t =
      | I0
      | I1 of 'a
      | I2 of 'b * 'c
      | Y1
      | Y2 of 'g
      | Y3 of 'h * 'i
      | Z1
      | Z2 of 'f
      | Z3 of 'd * 'e
    [@@deriving
      stable_variant
        ~version:[%stable: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) V1.t]
        ~remove:[ Y1; Y2; Y3 ]
        ~add:[ X1; X2; X3 ]
        ~modify:[ Z1; Z2; Z3 ]]
  end
end

(******************************************************************************)
(*                                                                            *)
(* Now we add some more interesting examples                                  *)
(*                                                                            *)
(******************************************************************************)

(******************)
(* First: records *)
(******************)

module Add_type_parameter_record = struct
  module V1 = struct
    type t = { value : int }
  end

  module V2 = struct
    type 'a t =
      { value : int
      ; stuff : 'a
      }
    [@@deriving_inline stable_record ~version:V1.t ~remove:[ stuff ]]

    let _ = fun (_ : 'a t) -> ()

    let to_V1_t (_t : 'a t) =
      let ({ stuff = _; value } : 'a t) = _t in
      ({ value } : V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) ~stuff =
      let ({ value } : V1.t) = _t in
      ({ stuff; value } : 'a t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Change_type_parameter_record = struct
  module V1 = struct
    type 'a t = { lst : 'a list }
  end

  module V2 = struct
    type ('a, 'b) t = { pair_lst : ('a * 'b) list }
    [@@deriving
      stable_record ~version:[%stable: 'a V1.t] ~add:[ lst ] ~remove:[ pair_lst ]]
  end
end

(******************)
(* Then: variants *)
(******************)

module Add_type_parameter_variant = struct
  module V1 = struct
    type t = Int of int [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~int:int_fun = function
            | Int v0 -> int_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type 'a t =
      | Int of int
      | Otherwise of 'a
    [@@deriving_inline stable_variant ~version:V1.t ~remove:[ Otherwise ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : 'a t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~int:int_fun ~otherwise:otherwise_fun = function
            | Int v0 -> int_fun v0
            | Otherwise v0 -> otherwise_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~remove_Otherwise (v : 'a t) : V1.t =
        Stable_variant.Helper.map v ~int:(fun v0 -> V1.Int v0) ~otherwise:remove_Otherwise
      ;;

      let _ = to_V1_t

      let of_V1_t (v : V1.t) : 'a t =
        V1.Stable_variant.Helper.map v ~int:(fun v0 -> Int v0)
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Change_type_parameter_variant = struct
  module V1 = struct
    type 'a t = Foo of 'a [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : 'a t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~foo:foo_fun = function
            | Foo v0 -> foo_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type ('a, 'b) t =
      | Foo of 'a * int
      | Bar of 'b
    [@@deriving
      stable_variant ~version:[%stable: ('a * int) V1.t] ~modify:[ Foo ] ~remove:[ Bar ]]
  end
end

module Change_type_parameter_variations = struct
  module V1 = struct
    type 'a t = { foo : 'a }
  end

  module V2a = struct
    type 'b t = { foo : 'b }
    [@@deriving_inline stable_record ~version:[%stable: 'x V1.t] ~modify:[ foo ]]

    let _ = fun (_ : 'b t) -> ()

    let to_V1_t (_t : 'b t) ~modify_foo =
      let ({ foo } : 'b t) = _t in
      ({ foo = modify_foo foo } : 'x V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : 'x V1.t) ~modify_foo =
      let ({ foo } : 'x V1.t) = _t in
      ({ foo = modify_foo foo } : 'b t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end

  module V2b = struct
    type 'b t = { foo : 'b }
    [@@deriving_inline stable_record ~version:[%stable: 'b V1.t] ~modify:[ foo ]]

    let _ = fun (_ : 'b t) -> ()

    let to_V1_t (_t : 'b t) ~modify_foo =
      let ({ foo } : 'b t) = _t in
      ({ foo = modify_foo foo } : 'b V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : 'b V1.t) ~modify_foo =
      let ({ foo } : 'b V1.t) = _t in
      ({ foo = modify_foo foo } : 'b t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end

  module V2c = struct
    type 'b t = { foo : 'b }
    [@@deriving_inline stable_record ~version:[%stable: _ V1.t] ~modify:[ foo ]]

    let _ = fun (_ : 'b t) -> ()

    let to_V1_t (_t : 'b t) ~modify_foo =
      let ({ foo } : 'b t) = _t in
      ({ foo = modify_foo foo } : _ V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : _ V1.t) ~modify_foo =
      let ({ foo } : _ V1.t) = _t in
      ({ foo = modify_foo foo } : 'b t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end

  module V2d = struct
    type 'b t = { foo : 'b }
    [@@deriving_inline
      stable_record ~version:[%stable: (int * string) V1.t] ~modify:[ foo ]]

    let _ = fun (_ : 'b t) -> ()

    let to_V1_t (_t : 'b t) ~modify_foo =
      let ({ foo } : 'b t) = _t in
      ({ foo = modify_foo foo } : (int * string) V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : (int * string) V1.t) ~modify_foo =
      let ({ foo } : (int * string) V1.t) = _t in
      ({ foo = modify_foo foo } : 'b t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end
