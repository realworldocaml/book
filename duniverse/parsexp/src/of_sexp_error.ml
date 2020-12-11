open! Import

type t =
  { user_exn : exn
  ; sub_sexp : Sexp.t
  ; location : Positions.range option
  }
[@@deriving_inline sexp_of]

let sexp_of_t =
  (function
    | { user_exn = v_user_exn; sub_sexp = v_sub_sexp; location = v_location } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_option Positions.sexp_of_range v_location in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "location"; arg ]
        :: bnds
      in
      let bnds =
        let arg = Sexp.sexp_of_t v_sub_sexp in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "sub_sexp"; arg ]
        :: bnds
      in
      let bnds =
        let arg = sexp_of_exn v_user_exn in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "user_exn"; arg ]
        :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
      : t -> Ppx_sexp_conv_lib.Sexp.t)
;;

[@@@end]

let user_exn t = t.user_exn
let sub_sexp t = t.sub_sexp
let location t = t.location

let report ppf ~filename t =
  let line, start, stop =
    match t.location with
    | None -> 1, 0, 0
    | Some { start_pos; end_pos } ->
      start_pos.line, start_pos.col, start_pos.col + end_pos.offset - start_pos.offset
  in
  Format.fprintf
    ppf
    "File \"%s\", line %d, characters %d-%d:\n\
     Error: s-expression conversion error;\n\
     exception %s\n"
    filename
    line
    start
    stop
    (Printexc.to_string t.user_exn)
;;

exception Of_sexp_error of t [@@deriving_inline sexp_of]

let () =
  Ppx_sexp_conv_lib.Conv.Exn_converter.add
    [%extension_constructor Of_sexp_error]
    (function
      | Of_sexp_error v0 ->
        let v0 = sexp_of_t v0 in
        Ppx_sexp_conv_lib.Sexp.List
          [ Ppx_sexp_conv_lib.Sexp.Atom "of_sexp_error.ml.Of_sexp_error"; v0 ]
      | _ -> assert false)
;;

[@@@end]

let raise ~user_exn ~sub_sexp ~location =
  raise (Of_sexp_error { user_exn; sub_sexp; location })
;;
