open! Import

type 'a t = 'a option =
  | None
  | Some of 'a

include (
struct
  type 'a t = 'a option [@@deriving_inline compare, hash, sexp, sexp_grammar]

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_option

  let hash_fold_t :
    'a. (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state -> 'a t -> Ppx_hash_lib.Std.Hash.state
    =
    hash_fold_option
  ;;

  let t_of_sexp :
    'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t
    =
    option_of_sexp
  ;;

  let sexp_of_t :
    'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
    =
    sexp_of_option
  ;;

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group)
      =
      { implicit_vars = [ "option" ]
      ; ggid = "j\132);\135qH\158\135\222H\001\007\004\158\218"
      ; types =
          [ "t", Explicit_bind ([ "a" ], Apply (Implicit_var 0, [ Explicit_var 0 ])) ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ option_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "option.ml"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]
end :
sig
  type 'a t = 'a option [@@deriving_inline compare, hash, sexp, sexp_grammar]

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val hash_fold_t
    :  (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> 'a t
    -> Ppx_hash_lib.Std.Hash.state

  include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

  val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

  [@@@end]
end
with type 'a t := 'a t)

let is_none = function
  | None -> true
  | _ -> false
;;

let is_some = function
  | Some _ -> true
  | _ -> false
;;

let value_map o ~default ~f =
  match o with
  | Some x -> f x
  | None -> default
;;

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a
;;

let invariant f t = iter t ~f

let map2 o1 o2 ~f =
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None
;;

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x
;;

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
;;

let value_exn ?here ?error ?message t =
  match t with
  | Some x -> x
  | None ->
    let error =
      match here, error, message with
      | None, None, None -> Error.of_string "Option.value_exn None"
      | None, None, Some m -> Error.of_string m
      | None, Some e, None -> e
      | None, Some e, Some m -> Error.tag e ~tag:m
      | Some p, None, None ->
        Error.create "Option.value_exn" p Source_code_position0.sexp_of_t
      | Some p, None, Some m -> Error.create m p Source_code_position0.sexp_of_t
      | Some p, Some e, _ ->
        Error.create
          (value message ~default:"")
          (e, p)
          (sexp_of_pair Error.sexp_of_t Source_code_position0.sexp_of_t)
    in
    Error.raise error
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [| x |]
;;

let to_list t =
  match t with
  | None -> []
  | Some x -> [ x ]
;;

let min_elt t ~compare:_ = t
let max_elt t ~compare:_ = t

let sum (type a) (module M : Container.Summable with type t = a) t ~f =
  match t with
  | None -> M.zero
  | Some x -> f x
;;

let for_all t ~f =
  match t with
  | None -> true
  | Some x -> f x
;;

let exists t ~f =
  match t with
  | None -> false
  | Some x -> f x
;;

let mem t a ~equal =
  match t with
  | None -> false
  | Some a' -> equal a a'
;;

let length t =
  match t with
  | None -> 0
  | Some _ -> 1
;;

let is_empty = is_none

let fold t ~init ~f =
  match t with
  | None -> init
  | Some x -> f init x
;;

let count t ~f =
  match t with
  | None -> 0
  | Some a -> if f a then 1 else 0
;;

let find t ~f =
  match t with
  | None -> None
  | Some x -> if f x then Some x else None
;;

let find_map t ~f =
  match t with
  | None -> None
  | Some a -> f a
;;

let equal f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false
;;

let some x = Some x

let both x y =
  match x, y with
  | Some a, Some b -> Some (a, b)
  | _ -> None
;;

let first_some x y =
  match x with
  | Some _ -> x
  | None -> y
;;

let some_if cond x = if cond then Some x else None

let merge a b ~f =
  match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (f a b)
;;

let filter t ~f =
  match t with
  | Some v as o when f v -> o
  | _ -> None
;;

let try_with f =
  match f () with
  | x -> Some x
  | exception _ -> None
;;

let try_with_join f =
  match f () with
  | x -> x
  | exception _ -> None
;;

include Monad.Make (struct
    type 'a t = 'a option

    let return x = Some x

    let map t ~f =
      match t with
      | None -> None
      | Some a -> Some (f a)
    ;;

    let map = `Custom map

    let bind o ~f =
      match o with
      | None -> None
      | Some x -> f x
    ;;
  end)

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t

let validate ~none ~some t =
  let module V = Validate in
  match t with
  | None -> V.name "none" (V.protect none ())
  | Some x -> V.name "some" (V.protect some x)
;;
