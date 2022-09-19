open! Import

type t =
  { user_exn : exn
  ; sub_sexp : Sexp.t
  ; location : Positions.range option
  }
[@@deriving_inline sexp_of]

let sexp_of_t =
  (fun { user_exn = user_exn__002_; sub_sexp = sub_sexp__004_; location = location__006_ } ->
     let bnds__001_ = [] in
     let bnds__001_ =
       let arg__007_ = sexp_of_option Positions.sexp_of_range location__006_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "location"; arg__007_ ] :: bnds__001_
     in
     let bnds__001_ =
       let arg__005_ = Sexp.sexp_of_t sub_sexp__004_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "sub_sexp"; arg__005_ ] :: bnds__001_
     in
     let bnds__001_ =
       let arg__003_ = sexp_of_exn user_exn__002_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "user_exn"; arg__003_ ] :: bnds__001_
     in
     Sexplib0.Sexp.List bnds__001_
     : t -> Sexplib0.Sexp.t)
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
    (Sexplib0.Sexp_conv.printexc_prefer_sexp t.user_exn)
;;

exception Of_sexp_error of t [@@deriving_inline sexp_of]

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Of_sexp_error] (function
    | Of_sexp_error arg0__008_ ->
      let res0__009_ = sexp_of_t arg0__008_ in
      Sexplib0.Sexp.List
        [ Sexplib0.Sexp.Atom "of_sexp_error.ml.Of_sexp_error"; res0__009_ ]
    | _ -> assert false)
;;

[@@@end]

let raise ~user_exn ~sub_sexp ~location =
  raise (Of_sexp_error { user_exn; sub_sexp; location })
;;
