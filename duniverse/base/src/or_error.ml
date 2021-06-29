open! Import

type 'a t = ('a, Error.t) Result.t [@@deriving_inline compare, equal, hash, sexp]

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__001_ b__002_ -> Result.compare _cmp__a Error.compare a__001_ b__002_
;;

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__007_ b__008_ -> Result.equal _cmp__a Error.equal a__007_ b__008_
;;

let hash_fold_t :
  'a. (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
  -> Ppx_hash_lib.Std.Hash.state -> 'a t -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a hsv arg -> Result.hash_fold_t _hash_fold_a Error.hash_fold_t hsv arg
;;

let t_of_sexp : 'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t =
  let _tp_loc = "or_error.ml.t" in
  fun _of_a t -> Result.t_of_sexp _of_a Error.t_of_sexp t
;;

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  fun _of_a v -> Result.sexp_of_t _of_a Error.sexp_of_t v
;;

[@@@end]

let invariant invariant_a t =
  match t with
  | Ok a -> invariant_a a
  | Error error -> Error.invariant error
;;

include (
  Result :
    Monad.S2
  with type ('a, 'b) t := ('a, 'b) Result.t
  with module Let_syntax := Result.Let_syntax)

include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return

    let apply f x =
      Result.combine f x ~ok:(fun f x -> f x) ~err:(fun e1 e2 -> Error.of_list [ e1; e2 ])
    ;;

    let map = `Custom map
  end)

module Let_syntax = struct
  let return = return

  include Monad_infix

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both

    (* from Applicative.Make *)
    module Open_on_rhs = struct end
  end
end

let ok = Result.ok
let is_ok = Result.is_ok
let is_error = Result.is_error

let try_with ?(backtrace = false) f =
  try Ok (f ()) with
  | exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

let ok_exn = function
  | Ok x -> x
  | Error err -> Error.raise err
;;

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let of_exn_result ?backtrace = function
  | Ok _ as z -> z
  | Error exn -> of_exn ?backtrace exn
;;

let error ?strict message a sexp_of_a = Error (Error.create ?strict message a sexp_of_a)
let error_s sexp = Error (Error.create_s sexp)
let error_string message = Error (Error.of_string message)
let errorf format = Printf.ksprintf error_string format
let tag t ~tag = Result.map_error t ~f:(Error.tag ~tag)
let tag_s t ~tag = Result.map_error t ~f:(Error.tag_s ~tag)

let tag_arg t message a sexp_of_a =
  Result.map_error t ~f:(fun e -> Error.tag_arg e message a sexp_of_a)
;;

let unimplemented s = error "unimplemented" s sexp_of_string
let combine_errors l = Result.map_error (Result.combine_errors l) ~f:Error.of_list
let combine_errors_unit l = Result.map (combine_errors l) ~f:(fun (_ : unit list) -> ())

let filter_ok_at_least_one l =
  let ok, errs = List.partition_map l ~f:Result.to_either in
  match ok with
  | [] -> Error (Error.of_list errs)
  | _ -> Ok ok
;;

let find_ok l =
  match List.find_map l ~f:Result.ok with
  | Some x -> Ok x
  | None ->
    Error
      (Error.of_list
         (List.map l ~f:(function
            | Ok _ -> assert false
            | Error err -> err)))
;;

let find_map_ok l ~f =
  With_return.with_return (fun { return } ->
    Error
      (Error.of_list
         (List.map l ~f:(fun elt ->
            match f elt with
            | Ok _ as x -> return x
            | Error err -> err))))
;;

let map = Result.map
let iter = Result.iter
let iter_error = Result.iter_error
