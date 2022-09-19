(* Some helpers that subscribe to the Core.Stable_module_types  *)

module Stable_container1 = struct
  type 'a t = { value : 'a }

  let map t ~f = { value = f t.value }
end

module Stable_container2 = struct
  type ('a, 'b) t =
    { a : 'a
    ; b : 'b
    }

  let map t ~f1 ~f2 = { a = f1 t.a; b = f2 t.b }
end

module Stable_container_bad = struct
  type 'a t = { a : 'a }
end

module Variant_basic = struct
  module V1 = struct
    type t =
      | A
      | B of t
      | C of t * t
      | D of
          { a : t
          ; b : t
          }
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun ~c:c_fun ~d:d_fun = function
            | A -> a_fun ()
            | B v0 -> b_fun v0
            | C (v0, v1) -> c_fun v0 v1
            | D { a = v0; b = v1 } -> d_fun ~a:v0 ~b:v1
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | A
      | B of t
      | C of t * t
      | D of
          { a : t
          ; b : t
          }
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun ~c:c_fun ~d:d_fun = function
            | A -> a_fun ()
            | B v0 -> b_fun v0
            | C (v0, v1) -> c_fun v0 v1
            | D { a = v0; b = v1 } -> d_fun ~a:v0 ~b:v1
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~a:(fun () -> V1.A)
            ~b:(fun v0 -> V1.B (recurse v0))
            ~c:(fun v0 v1 -> V1.C (recurse v0, recurse v1))
            ~d:(fun ~a:v0 ~b:v1 -> V1.D { a = recurse v0; b = recurse v1 })
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~a:(fun () -> A)
            ~b:(fun v0 -> B (recurse v0))
            ~c:(fun v0 v1 -> C (recurse v0, recurse v1))
            ~d:(fun ~a:v0 ~b:v1 -> D { a = recurse v0; b = recurse v1 })
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Record_basic = struct
  module V1 = struct
    type t =
      { a : unit
      ; b : t
      }
  end

  module V2 = struct
    type t =
      { a : unit
      ; b : t
      }
    [@@deriving_inline stable_record ~version:V1.t]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) =
      let rec recurse ({ a; b } : t) : V1.t = { a; b = recurse b } in
      recurse _t
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) =
      let rec recurse ({ a; b } : V1.t) : t = { a; b = recurse b } in
      recurse _t
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Variant_nested = struct
  module V1 = struct
    type t =
      | A
      | B of unit * (unit * t)
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun = function
            | A -> a_fun ()
            | B (v0, v1) -> b_fun v0 v1
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | A
      | B of unit * (unit * t)
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~a:a_fun ~b:b_fun = function
            | A -> a_fun ()
            | B (v0, v1) -> b_fun v0 v1
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~a:(fun () -> V1.A)
            ~b:(fun v0 v1 ->
              V1.B
                ( v0
                , let v0, v1 = v1 in
                  v0, recurse v1 ))
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~a:(fun () -> A)
            ~b:(fun v0 v1 ->
              B
                ( v0
                , let v0, v1 = v1 in
                  v0, recurse v1 ))
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Variant_pervasives = struct
  module V1 = struct
    type t =
      | Option of t option
      | Ref of t ref
      | Lazy_t of t lazy_t
      | Array of t array
      | List of t list
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
                ~option:option_fun
                ~ref:ref_fun
                ~lazy_t:lazy_t_fun
                ~array:array_fun
                ~list:list_fun
            = function
              | Option v0 -> option_fun v0
              | Ref v0 -> ref_fun v0
              | Lazy_t v0 -> lazy_t_fun v0
              | Array v0 -> array_fun v0
              | List v0 -> list_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | Option of t option
      | Ref of t ref
      | Lazy_t of t lazy_t
      | Array of t array
      | List of t list
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map
                ~option:option_fun
                ~ref:ref_fun
                ~lazy_t:lazy_t_fun
                ~array:array_fun
                ~list:list_fun
            = function
              | Option v0 -> option_fun v0
              | Ref v0 -> ref_fun v0
              | Lazy_t v0 -> lazy_t_fun v0
              | Array v0 -> array_fun v0
              | List v0 -> list_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~array:(fun v0 -> V1.Array (Stdlib.Array.map (fun x -> recurse x) v0))
            ~lazy_t:(fun v0 -> V1.Lazy_t (lazy (recurse (Stdlib.Lazy.force v0))))
            ~list:(fun v0 -> V1.List (Stdlib.List.map (fun x -> recurse x) v0))
            ~option:(fun v0 -> V1.Option (Stdlib.Option.map (fun x -> recurse x) v0))
            ~ref:(fun v0 -> V1.Ref (ref (recurse !v0)))
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~array:(fun v0 -> Array (Stdlib.Array.map (fun x -> recurse x) v0))
            ~lazy_t:(fun v0 -> Lazy_t (lazy (recurse (Stdlib.Lazy.force v0))))
            ~list:(fun v0 -> List (Stdlib.List.map (fun x -> recurse x) v0))
            ~option:(fun v0 -> Option (Stdlib.Option.map (fun x -> recurse x) v0))
            ~ref:(fun v0 -> Ref (ref (recurse !v0)))
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Record_pervasives = struct
  module V1 = struct
    type t =
      { option : t option
      ; ref_ : t ref
      ; lazy_ : t lazy_t
      ; array : t array
      ; list : t list
      }
  end

  module V2 = struct
    type t =
      { option : t option
      ; ref_ : t ref
      ; lazy_ : t lazy_t
      ; array : t array
      ; list : t list
      }
    [@@deriving_inline stable_record ~version:V1.t]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) =
      let rec recurse ({ array; lazy_; list; option; ref_ } : t) : V1.t =
        { array = Stdlib.Array.map (fun x -> recurse x) array
        ; lazy_ = lazy (recurse (Stdlib.Lazy.force lazy_))
        ; list = Stdlib.List.map (fun x -> recurse x) list
        ; option = Stdlib.Option.map (fun x -> recurse x) option
        ; ref_ = ref (recurse !ref_)
        }
      in
      recurse _t
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) =
      let rec recurse ({ array; lazy_; list; option; ref_ } : V1.t) : t =
        { array = Stdlib.Array.map (fun x -> recurse x) array
        ; lazy_ = lazy (recurse (Stdlib.Lazy.force lazy_))
        ; list = Stdlib.List.map (fun x -> recurse x) list
        ; option = Stdlib.Option.map (fun x -> recurse x) option
        ; ref_ = ref (recurse !ref_)
        }
      in
      recurse _t
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Variant_with_container_missing_map = struct
  module V1 = struct
    type t =
      | C1 of t Stable_container1.t
      | C2 of (t, t option) Stable_container2.t
      | C_bad of t Stable_container_bad.t
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~c1:c1_fun ~c2:c2_fun ~c_bad:c_bad_fun = function
            | C1 v0 -> c1_fun v0
            | C2 v0 -> c2_fun v0
            | C_bad v0 -> c_bad_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | C1 of t Stable_container1.t
      | C2 of (t, t option) Stable_container2.t
      | C_bad of t Stable_container_bad.t
    [@@deriving_inline
      stable_variant
        ~version:V1.t
        ~modify:
          [ (* note that we have to modify C_bad because it doesn't implement map function *)
            C_bad
          ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~c1:c1_fun ~c2:c2_fun ~c_bad:c_bad_fun = function
            | C1 v0 -> c1_fun v0
            | C2 v0 -> c2_fun v0
            | C_bad v0 -> c_bad_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~modify_C_bad =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~c1:(fun v0 -> V1.C1 (Stable_container1.map v0 ~f:(fun x -> recurse x)))
            ~c2:(fun v0 ->
              V1.C2
                (Stable_container2.map
                   v0
                   ~f1:(fun x -> recurse x)
                   ~f2:(fun x -> Stdlib.Option.map (fun x -> recurse x) x)))
            ~c_bad:modify_C_bad
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t ~modify_C_bad =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~c1:(fun v0 -> C1 (Stable_container1.map v0 ~f:(fun x -> recurse x)))
            ~c2:(fun v0 ->
              C2
                (Stable_container2.map
                   v0
                   ~f1:(fun x -> recurse x)
                   ~f2:(fun x -> Stdlib.Option.map (fun x -> recurse x) x)))
            ~c_bad:modify_C_bad
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Record_with_container_missing_map = struct
  module V1 = struct
    type t =
      { c1 : t Stable_container1.t
      ; c2 : (t, t option) Stable_container2.t
      ; c_bad : t Stable_container_bad.t
      }
  end

  module V2 = struct
    type t =
      { c1 : t Stable_container1.t
      ; c2 : (t, t option) Stable_container2.t
      ; c_bad : t Stable_container_bad.t
      }
    [@@deriving_inline
      stable_record
        ~version:V1.t
        ~modify:
          [ c_bad
          (* note that we have to modify c_bad because it doesn't implement a map function *)
          ]]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) ~modify_c_bad =
      let rec recurse ({ c1; c2; c_bad } : t) : V1.t =
        { c1 = Stable_container1.map c1 ~f:(fun x -> recurse x)
        ; c2 =
            Stable_container2.map
              c2
              ~f1:(fun x -> recurse x)
              ~f2:(fun x -> Stdlib.Option.map (fun x -> recurse x) x)
        ; c_bad = modify_c_bad c_bad
        }
      in
      recurse _t
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) ~modify_c_bad =
      let rec recurse ({ c1; c2; c_bad } : V1.t) : t =
        { c1 = Stable_container1.map c1 ~f:(fun x -> recurse x)
        ; c2 =
            Stable_container2.map
              c2
              ~f1:(fun x -> recurse x)
              ~f2:(fun x -> Stdlib.Option.map (fun x -> recurse x) x)
        ; c_bad = modify_c_bad c_bad
        }
      in
      recurse _t
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Type_is_recursive_but_upgrade_doesn't_need_rec = struct
  module V1 = struct
    type t =
      { c1 : t Stable_container1.t
      ; c2 : (t, t option) Stable_container2.t
      ; c_bad : t Stable_container_bad.t
      }
  end

  module V2 = struct
    type t = { c1 : t Stable_container1.t }
    [@@deriving_inline stable_record ~version:V1.t ~add:[ c2; c_bad ] ~modify:[ c1 ]]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) ~modify_c1 ~c_bad ~c2 =
      let ({ c1 } : t) = _t in
      ({ c1 = modify_c1 c1; c2; c_bad } : V1.t)
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) ~modify_c1 =
      let ({ c1; c2 = _; c_bad = _ } : V1.t) = _t in
      ({ c1 = modify_c1 c1 } : t)
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Variant_with_changes = struct
  module V1 = struct
    type t =
      | Mn of bool
      | Mr of t
      | Kn of bool
      | Kr of t
      | An of bool
      | Ar of t
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~mn:mn_fun ~mr:mr_fun ~kn:kn_fun ~kr:kr_fun ~an:an_fun ~ar:ar_fun
            = function
              | Mn v0 -> mn_fun v0
              | Mr v0 -> mr_fun v0
              | Kn v0 -> kn_fun v0
              | Kr v0 -> kr_fun v0
              | An v0 -> an_fun v0
              | Ar v0 -> ar_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t =
      | Mn of int
      | Mr of t * t
      | Kn of bool
      | Kr of t
      | Rn of bool
      | Rr of t
    [@@deriving_inline
      stable_variant ~version:V1.t ~remove:[ Rn; Rr ] ~add:[ An; Ar ] ~modify:[ Mn; Mr ]]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~mn:mn_fun ~mr:mr_fun ~kn:kn_fun ~kr:kr_fun ~rn:rn_fun ~rr:rr_fun
            = function
              | Mn v0 -> mn_fun v0
              | Mr (v0, v1) -> mr_fun v0 v1
              | Kn v0 -> kn_fun v0
              | Kr v0 -> kr_fun v0
              | Rn v0 -> rn_fun v0
              | Rr v0 -> rr_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t ~modify_Mn ~modify_Mr ~remove_Rr ~remove_Rn =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map
            v
            ~kn:(fun v0 -> V1.Kn v0)
            ~kr:(fun v0 -> V1.Kr (recurse v0))
            ~mn:modify_Mn
            ~mr:modify_Mr
            ~rn:remove_Rn
            ~rr:remove_Rr
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t ~modify_Mn ~modify_Mr ~remove_Ar ~remove_An =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map
            v
            ~an:remove_An
            ~ar:remove_Ar
            ~kn:(fun v0 -> Kn v0)
            ~kr:(fun v0 -> Kr (recurse v0))
            ~mn:modify_Mn
            ~mr:modify_Mr
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Record_with_changes = struct
  module V1 = struct
    type t =
      { mn : bool
      ; mr : t
      ; kn : bool
      ; kr : t
      ; an : bool
      ; ar : t option
      ; sn : bool
      ; sr : t option
      }
  end

  module V2 = struct
    type t =
      { mn : bool
      ; mr : t * t
      ; kn : bool
      ; kr : t
      ; rn : bool
      ; rr : t
      ; sn : bool
      ; sr : t option
      }
    [@@deriving_inline
      stable_variant
        ~version:V1.t
        ~remove:[ rn; rr ]
        ~add:[ an; ar ]
        ~modify:[ mn; mr ]
        ~set:[ sn; sr ]]

    let _ = fun (_ : t) -> ()

    let to_V1_t (_t : t) ~modify_mn ~modify_mr ~sr ~sn ~ar ~an =
      let rec recurse ({ kn; kr; mn; mr; rn = _; rr = _; sn = _; sr = _ } : t) : V1.t =
        { an; ar; kn; kr = recurse kr; mn = modify_mn mn; mr = modify_mr mr; sn; sr }
      in
      recurse _t
    ;;

    let _ = to_V1_t

    let of_V1_t (_t : V1.t) ~modify_mn ~modify_mr ~sr ~sn ~rr ~rn =
      let rec recurse ({ an = _; ar = _; kn; kr; mn; mr; sn = _; sr = _ } : V1.t) : t =
        { kn; kr = recurse kr; mn = modify_mn mn; mr = modify_mr mr; rn; rr; sn; sr }
      in
      recurse _t
    ;;

    let _ = of_V1_t

    [@@@end]
  end
end

module Deep = struct
  module V1 = struct
    type t = F of (t * t list) option list array lazy_t ref
    [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~f:f_fun = function
            | F v0 -> f_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type t = F of (t * t list) option list array lazy_t ref
    [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~f:f_fun = function
            | F v0 -> f_fun v0
          ;;

          let _ = map
        end
      end

      let to_V1_t =
        let rec recurse (v : t) : V1.t =
          Stable_variant.Helper.map v ~f:(fun v0 ->
            V1.F
              (ref
                 (lazy
                   (Stdlib.Array.map
                      (fun x ->
                         Stdlib.List.map
                           (fun x ->
                              Stdlib.Option.map
                                (fun x ->
                                   let v0, v1 = x in
                                   recurse v0, Stdlib.List.map (fun x -> recurse x) v1)
                                x)
                           x)
                      (Stdlib.Lazy.force !v0)))))
        in
        recurse
      ;;

      let _ = to_V1_t

      let of_V1_t =
        let rec recurse (v : V1.t) : t =
          V1.Stable_variant.Helper.map v ~f:(fun v0 ->
            F
              (ref
                 (lazy
                   (Stdlib.Array.map
                      (fun x ->
                         Stdlib.List.map
                           (fun x ->
                              Stdlib.Option.map
                                (fun x ->
                                   let v0, v1 = x in
                                   recurse v0, Stdlib.List.map (fun x -> recurse x) v1)
                                x)
                           x)
                      (Stdlib.Lazy.force !v0)))))
        in
        recurse
      ;;

      let _ = of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module T_and_s = struct
  module V1 = struct
    type t = F of t [@@deriving_inline stable_variant]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module Stable_variant = struct
        module Helper = struct
          let map ~f:f_fun = function
            | F v0 -> f_fun v0
          ;;

          let _ = map
        end
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module V2 = struct
    type s = F of s [@@deriving_inline stable_variant ~version:V1.t]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : s) -> ()

      module Stable_variant_of_s = struct
        module Helper = struct
          let map ~f:f_fun = function
            | F v0 -> f_fun v0
          ;;

          let _ = map
        end
      end

      let s_to_V1_t =
        let rec recurse (v : s) : V1.t =
          Stable_variant_of_s.Helper.map v ~f:(fun v0 -> V1.F (recurse v0))
        in
        recurse
      ;;

      let _ = s_to_V1_t

      let s_of_V1_t =
        let rec recurse (v : V1.t) : s =
          V1.Stable_variant.Helper.map v ~f:(fun v0 -> F (recurse v0))
        in
        recurse
      ;;

      let _ = s_of_V1_t
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end
end
