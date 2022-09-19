open! Base

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
  [@@stable.changes
    "" ~remove:[ rn; rr ] ~add:[ an; ar ] ~modify:[ mn; mr ] ~set:[ sn; sr ]]
  [@@deriving_inline stable_record ~version:V1.t]

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
